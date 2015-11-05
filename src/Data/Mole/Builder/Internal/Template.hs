{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Builder.Internal.Template where


import           Control.Applicative

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Attoparsec.Text as AP

import           Data.Maybe
import           Data.Monoid
import           Data.Mole.Types



type Bracket = (Text,Text)
type Brackets = [Bracket]
type Context = Bracket -> Text -> Either Error Text
data Fragment = Lit !Text | Var !Bracket !Text deriving (Show, Eq)
newtype Template = Template [Fragment] deriving (Show, Eq)

template :: Brackets -> Text -> Template
template brackets input = case AP.parseOnly (fragmentParser brackets) input of
    Left  x -> error $ x ++ " on input '" ++ T.unpack input ++ "'"
    Right x -> Template $ mergeLiterals x

mergeLiterals :: [Fragment] -> [Fragment]
mergeLiterals = reverse . foldl f []
  where
    f acc          (Lit "") = acc
    f ((Lit a):xs) (Lit b)  = (Lit $ a <> b) : xs
    f acc          frag     = frag:acc

fragmentParser :: Brackets -> AP.Parser [Fragment]
fragmentParser brackets = go
  where
    go = lit <|> rest

    -- The rest of the input as a single 'Lit'. The contents of the text may be
    -- empty, but we drop empty literals during postprocessing.
    rest = pure . Lit <$> AP.takeText

    -- Matches 'Text' which marks the beginning of a 'Var'.
    varMarkers = foldl1 (<|>) $ map (AP.string . fst) brackets

    -- The contents of a variable and its end marker. The start marker must be
    -- consumed before. This function recurses into 'go' to continue parsing
    -- the rest of the input.
    var (a,b) = do
        text <- T.strip . T.pack <$> AP.manyTill AP.anyChar (AP.string b)
        (Var (a, b) text :) <$> go

    -- A literal (may be empty), up until the start marker of a var, followed
    -- by a var.
    lit = do
        (text, a) <- stringTill varMarkers
        let b = fromJust $ lookup a brackets
        (Lit (T.pack text) :) <$> var (a, b)

    -- The string until 'end' matches. What matched at the end is returned, to
    -- give the caller a chance to go different paths depending on how the
    -- string ended.
    stringTill end = scan
        where scan = ((\x -> ([],x)) <$> end) <|> do
                            x <- AP.anyChar
                            (xs, e) <- scan
                            return $ (x:xs, e)


render :: Template -> Context -> Either Error Text
render (Template frags) ctxFunc =
    mconcat <$> traverse renderFrag frags
  where
    renderFrag (Lit s) = pure s
    renderFrag (Var bracket x) = ctxFunc bracket x
