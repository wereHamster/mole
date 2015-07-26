{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Applicative

import           Test.Hspec
import           Test.SmallCheck
import           Test.SmallCheck.Series
import           Test.Hspec.SmallCheck

import           Data.Monoid         ((<>))
import           Data.Function
import           Data.List
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           Data.Time.Clock.POSIX


import           Data.Mole.Builder.Internal.Template



main :: IO ()
main = do
    hspec $ spec


spec :: Spec
spec = do
    describe "Data.Mole.Builder.Internal.Template" $ do
        it "should parse a non-empty string into a single literal" $ do
            property $ \x -> length x > 0 ==>
                template x == Template [Lit $ T.pack x]
        it "should strip whitespace around a var" $ do
            template "<* \nvar \t *>" `shouldBe`
                Template [ Var "var" ]
        it "should parse a string with a var in it" $ do
            template "some <* var *> template" `shouldBe`
                Template [ Lit "some ", Var "var", Lit " template" ]
        it "should parse two directly adjacent vars" $ do
            template "<* v1 *><* v2 *>" `shouldBe`
                Template [ Var "v1", Var "v2" ]
        it "should parse two adjacent vars with a whitespace in between" $ do
            template "<* v1 *> <* v2 *>" `shouldBe`
                Template [ Var "v1", Lit " ", Var "v2" ]
        it "should ignore a false start of a var" $ do
            template "<div><a href='<* var *>'>link</a></div>" `shouldBe`
                Template [ Lit "<div><a href='", Var "var", Lit "'>link</a></div>" ]
        it "should allow literals between two false starts" $ do
            template "<div> <hr> <* var *> <br>" `shouldBe`
                Template [ Lit "<div> <hr> ", Var "var", Lit " <br>" ]
