{- HSpec example!
-}

module SandboxSpec where

import Sandbox
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "foo" $ do
            it "1 + 1 = 2" $ do
                shouldBe (foo 1 1) 2
            it "Checking right zero" $ do
                shouldBe (foo 1 0) 0
    describe "bar" $ do
        it "1 * 1 = 1" $ do
            shouldBe (bar 1 1) 1
