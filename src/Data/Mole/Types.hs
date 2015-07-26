module Data.Mole.Types where

import           Control.Concurrent
import           Control.Concurrent.STM

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Time (UTCTime)

import qualified Network.Kraken as K



-- | An 'AssetId' is an internal reference to an unprocessed asset. Source files
-- use those to express dependencies on other files. And the 'Config' object
-- contains definitions how to build those.
--
-- Even though the 'AssetId' may look like a filename or path, it doesn't have
-- to refer to an actual file in the filesystem.
newtype AssetId = AssetId { unAssetId :: String }
    deriving (Ord, Eq)

instance Show AssetId where
    show = unAssetId


newtype BuildId = BuildId { unBuildId :: Int }
    deriving (Ord, Eq, Show)

type ContentType = String


data AssetState = Dirty | Building | Failed Error | Completed Result
    deriving (Eq, Show)



data Config = Config
    { assetDefinitions :: Map AssetId AssetDefinition
      -- ^ All the assets we know how to build. This doesn't mean that they
      -- actually will be built. Only if they are referenced / required /
      -- reachable through one of the entry points.
      --
      -- This list is rarely created manually. Usually it's automatically
      -- generated by traversing a source directory and converting each
      -- file into an asset (depending on the file type).
      --
      -- Another option is to only define the entry points and maybe a few
      -- special assets and let auto-discovery do the rest.

    , autoDiscovery :: Handle -> AssetId -> IO (Maybe AssetDefinition)
     -- ^ If an asset is not defined statically, we attempt to do auto-discovery
     -- based on its 'AssetId'. The default implementation tries to locate the
     -- file below the base path where all the sources are. This works really
     -- well for binary files which need no processing (eg. images, font files)
     -- or files where we can infer the 'AssetDefinition' from its content type
     -- or file extension.

    , entryPoints :: [AssetId]
      -- ^ The entry points into the application. Usually this will include at
      -- least the index file (eg. index.html).
    }


data AssetRuntimeState = AssetRuntimeState
    { arsState :: AssetState
    , arsSources :: Set FilePath
    , arsDependencySet :: Set AssetId
    } deriving (Show)


type PublicIdentifier = String

data Result = Result
    { publicIdentifier :: PublicIdentifier
      -- ^ This is how other parts of the application can refer to the asset.
      -- This can be an absolute path or a full URL (for example if you're
      -- serving the assets from a CDN).

    , resource :: Maybe (ByteString, ContentType)
      -- ^ The content of the asset if built locally. For external assets (eg.
      -- jquery served from the google CDN) this is 'Nothing'.

    } deriving (Eq, Show)


data Error
    = UndeclaredDependency AssetId
    | AssetNotFound AssetId
    | DependencyFailed
    deriving (Show, Eq)


data Builder = Builder
    { assetSources :: Set FilePath
      -- An approximation of files which contributed to the asset. This is
      -- stored in the state and used to trigger rebuilds when the files on
      -- disk change.

    , assetDependencies :: Set AssetId
      -- ^ The dependencies of the asset which is currently being built. These
      -- dependencies are automatically built before the asset is packaged
      -- into its final result.

    , packageAsset :: Map AssetId PublicIdentifier -> Either Error Result
      -- ^ A function which takes the public identifiers for all dependencies
      -- and creates the final asset package.
    }


data AssetDefinition = AssetDefinition
    { createBuilder :: Handle -> AssetId -> IO Builder
      -- ^ IO action which returns metadata about the asset and a function
      -- which assembles the asset into its final form.

    , transformPublicIdentifier :: PublicIdentifier -> PublicIdentifier
      -- ^ An optional transformer for the 'PublicIdentifier'. Use this if you
      -- want to serve the assets from a different path or domain. The default
      -- implementation simply prepends "/" to the pubId, therefore making the
      -- path absolute. If you want to serve the assets from a subdirectory,
      -- prepend for example "/assets/". If you use a CDN, make the pubId a
      -- full URL.

    , emitResult :: AssetId -> Result -> IO ()
      -- ^ Action which is invoked every time an asset has completed building.
      -- This is useful if you want to store the asset in an output directory
      -- or maybe even directly upload to the server.
    }



data State = State
    { dispatcherThreadId :: Maybe ThreadId
      -- ^ The thread which waits for assets to be marked as 'Dirty' and
      -- dispatches build jobs. Doing that in a single thread makes it easier
      -- to synchronize STM with IO.

    , stopFileWatcher :: IO ()
      -- ^ The file watcher is run in a separate thread. This is the action
      -- to stop it.

    , assets :: Map AssetId AssetRuntimeState
    }


data Message = Message UTCTime AssetId String

data Handle = Handle
    { state :: TVar State
    , messages :: TQueue Message
    , krakenH :: !(Maybe K.Handle)
    --, messageThreadId :: ThreadId
    , lock :: !(TMVar ())
    }