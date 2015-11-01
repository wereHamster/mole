{-# LANGUAGE OverloadedStrings #-}

module Data.Mole.Builder.Internal.Template where


import           Control.Applicative
import           Control.Monad

import qualified Data.Set as S

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Attoparsec.Text as AP

import           Data.Monoid
import           Data.Mole.Types



type Bracket = (Text,Text)
type Brackets = [Bracket]
type Context = Bracket -> Text -> Either Error Text
data Fragment = Lit !Text | Var !Bracket !Text deriving (Show, Eq)
newtype Template = Template [Fragment] deriving (Show, Eq)

template :: Brackets -> String -> Template
template brackets input = case AP.parseOnly (AP.many1 $ fragmentParser brackets) (T.pack input) of
    Left  x -> error $ x ++ " on input '" ++ input ++ "'"
    Right x -> Template $ mergeLiterals $ concat x

mergeLiterals :: [Fragment] -> [Fragment]
mergeLiterals = reverse . foldl f []
  where
    f []           frag    = [frag]
    f ((Lit a):xs) (Lit b) = (Lit $ a <> b) : xs
    f acc          frag    = frag:acc

fragmentParser :: Brackets -> AP.Parser [Fragment]
fragmentParser brackets = vars <|> lit
  where
    firsts :: S.Set Char
    firsts = S.fromList $ map (\(x,_) -> T.head x) brackets

    var :: Bracket -> AP.Parser [Fragment]
    var bracket@(a,b) = do
        void $ AP.string a
        text <- AP.scan "" $ \s c -> if T.isSuffixOf b s then Nothing else Just (s <> T.singleton c)
        case text of
            "" -> fail "var"
            _  -> return $ [Var bracket $ T.strip $ T.take (T.length text - T.length b) text]

    vars :: AP.Parser [Fragment]
    vars = foldl1 (<|>) (map var brackets)

    lit :: AP.Parser [Fragment]
    lit = do
        c <- AP.takeTill $ flip S.member firsts
        case c of
            "" -> vars <|> (AP.anyChar >>= \ch -> return [Lit $ T.singleton ch])
            _  -> return [Lit c]

render :: Template -> Context -> Either Error String
render (Template frags) ctxFunc = do
    res <- traverse renderFrag frags
    return $ T.unpack $ mconcat res
  where
    renderFrag (Lit s) = pure s
    renderFrag (Var bracket x) = ctxFunc bracket x
