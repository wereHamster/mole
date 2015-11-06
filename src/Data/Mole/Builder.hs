module Data.Mole.Builder where


import           Data.Mole.Types
import           Data.Mole.Builder.Html
import           Data.Mole.Builder.Binary
import           Data.Mole.Builder.JavaScript
import           Data.Mole.Builder.Image
import           Data.Mole.Builder.Stylesheet

import           System.FilePath



builderForFile :: FilePath -> String -> Handle -> AssetId -> IO Builder
builderForFile basePath x = case takeExtension x of
    ".css"  -> stylesheetBuilder x
    ".html" -> htmlBuilder (drop (length basePath) x) x
    ".js"   -> javascriptBuilder x
    ".png"  -> imageBuilder x "image/png"
    ".jpeg" -> imageBuilder x "image/jpeg"
    ".jpg"  -> imageBuilder x "image/jpeg"
    _       -> binaryBuilder x "application/octet-stream"
