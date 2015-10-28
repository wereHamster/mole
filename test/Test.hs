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

bracket :: Bracket
bracket = ("<*","*>")

brackets :: Brackets
brackets = [bracket]

spec :: Spec
spec = do
    describe "Data.Mole.Builder.Internal.Template" $ do
        it "should parse a non-empty string into a single literal" $ do
            property $ \x -> length x > 0 ==>
                template brackets x == Template [Lit $ T.pack x]
        it "should strip whitespace around a var" $ do
            template brackets "<* \nvar \t *>" `shouldBe`
                Template [ Var bracket "var" ]
        it "should parse a string with a var in it" $ do
            template brackets "some <* var *> template" `shouldBe`
                Template [ Lit "some ", Var bracket "var", Lit " template" ]
        it "should parse two directly adjacent vars" $ do
            template brackets "<* v1 *><* v2 *>" `shouldBe`
                Template [ Var bracket "v1", Var bracket "v2" ]
        it "should parse two adjacent vars with a whitespace in between" $ do
            template brackets "<* v1 *> <* v2 *>" `shouldBe`
                Template [ Var bracket "v1", Lit " ", Var bracket "v2" ]
        it "should ignore a false start of a var" $ do
            template brackets "<div><a href='<* var *>'>link</a></div>" `shouldBe`
                Template [ Lit "<div><a href='", Var bracket "var", Lit "'>link</a></div>" ]
        it "should allow literals between two false starts" $ do
            template brackets "<div> <hr> <* var *> <br>" `shouldBe`
                Template [ Lit "<div> <hr> ", Var bracket "var", Lit " <br>" ]
