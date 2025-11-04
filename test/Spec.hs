module Main (main) where

import qualified Control.Monad.State as M
import GHC.Stack (HasCallStack)
import Iso (apply, unapply)
import OpenCog.MeTTa (Atom)
import OpenCog.MeTTa.Parser (State (..), parseFile, parseMeTTa)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "MeTTa/HSM conversion" $ do
    it "produces the MeTTa fixture when converting from the HSM fixture" $ do
        hsmSource <- readFile "test.hsmetta"
        mettaSource <- readFile "test.metta"
        expectRight (parseMettaText mettaSource) $ \expectedAtoms ->
            expectRight (parseHsmText hsmSource) $ \hsmAtoms ->
                expectRight (renderMettaText hsmAtoms) $ \convertedMetta ->
                    expectRight (parseMettaText convertedMetta) $ \roundTripAtoms ->
                        roundTripAtoms `shouldBe` expectedAtoms

    it "produces the HSM fixture when converting from the MeTTa fixture" $ do
        mettaSource <- readFile "test.metta"
        hsmSource <- readFile "test.hsmetta"
        expectRight (parseHsmText hsmSource) $ \expectedAtoms ->
            expectRight (parseMettaText mettaSource) $ \mettaAtoms ->
                expectRight (renderHsmText mettaAtoms) $ \convertedHsm ->
                    expectRight (parseHsmText convertedHsm) $ \roundTripAtoms ->
                        roundTripAtoms `shouldBe` expectedAtoms

parseHsmText :: String -> Either String [Atom]
parseHsmText input =
    M.evalStateT (apply parseFile ()) (State {sText = input})

parseMettaText :: String -> Either String [Atom]
parseMettaText input =
    M.evalStateT (apply parseMeTTa ()) (State {sText = input})

renderHsmText :: [Atom] -> Either String String
renderHsmText atoms =
    fmap sText $ M.execStateT (unapply parseFile atoms) (State {sText = ""})

renderMettaText :: [Atom] -> Either String String
renderMettaText atoms =
    fmap sText $ M.execStateT (unapply parseMeTTa atoms) (State {sText = ""})

expectRight :: (HasCallStack, Show e) => Either e a -> (a -> Expectation) -> Expectation
expectRight value f =
    case value of
        Right a -> f a
        Left err -> expectationFailure ("expected Right but got Left: " ++ show err)
