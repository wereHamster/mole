{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Builder.Internal.Template where


import           Control.Applicative

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Attoparsec.Text as AP

import           Data.Monoid
import           Data.Mole.Types



type Context = Text -> Either Error Text
data Fragment = Lit Text | Var Text deriving (Show, Eq)
newtype Template = Template [Fragment] deriving (Show, Eq)

template :: String -> Template
template input = case AP.parseOnly (AP.many1 fragmentParser) (T.pack input) of
    Left  x -> error $ x ++ " on input '" ++ input ++ "'"
    Right x -> Template $ mergeLiterals $ concat x

mergeLiterals :: [Fragment] -> [Fragment]
mergeLiterals = reverse . foldl f []
  where
    f []           frag    = [frag]
    f ((Lit a):xs) (Lit b) = (Lit $ a <> b) : xs
    f acc          frag    = frag:acc

fragmentParser :: AP.Parser [Fragment]
fragmentParser = var <|> lit
  where
    var = do
        text <- AP.string "<*" *> AP.manyTill AP.anyChar (AP.string "*>")
        return $ [Var $ T.strip $ T.pack text]

    lit = do
        text <- AP.takeTill ('<'==)
        ( (var <|> (AP.anyChar >>= \c -> return $ [Lit $ T.singleton c]))
            >>= \v -> return $ [Lit text] ++ v)
            <|> (if T.null text then fail "literal" else return [Lit text])

render :: Template -> Context -> Either Error String
render (Template frags) ctxFunc = do
    res <- traverse renderFrag frags
    return $ T.unpack $ mconcat res
  where
    renderFrag (Lit s) = pure s
    renderFrag (Var x) = ctxFunc x
