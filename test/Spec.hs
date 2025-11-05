module Main (main) where

import Control.Monad (forM_)
import qualified Control.Monad.State as M
import GHC.Stack (HasCallStack)
import Iso (apply, unapply)
import OpenCog.MeTTa (Atom)
import OpenCog.MeTTa.Parser (State (..), parseFile, parseMeTTa)
import Test.Hspec

main :: IO ()
main = hspec spec

fixtures :: [(String, String)]
fixtures =
    [ ( "fun $x = 1 + $x"
      , "(= (fun $x) (+ 1 $x))"
      ),
      ( "fun $x = if $x == 1 then a else b"
      , "(= (fun $x) (if (== $x 1) a b))"
      ),
      ( "fun $x = case $x of\n\
        \                Nothing -> a\n\
        \                (Just $y) -> $y"
      , "(= (fun $x) (case $x ((Nothing a) ((Just $y) $y))))"
      ),
      ("fold-nested $f $init ( cons $x $xs ) = \n\
       \    if is-expr $x\n\
       \        then fold-nested $f ( fold-nested $f $init $x ) $xs\n\
       \        else fold-nested $f ( $f $init $x ) $xs"
      ,"(= (fold-nested $f $init (cons $x $xs))\n\
       \    (if (is-expr $x)\n\
       \        (fold-nested $f (fold-nested $f $init $x) $xs)\n\
       \        (fold-nested $f ($f $init $x) $xs)))"
      ),
      ("lettest $x = let $y = $x + 1\n\
       \             in $y"
      ,"(= (lettest $x) (let $y (+ $x 1) $y))"
      )
    ]

spec :: Spec
spec = describe "MeTTa/HSM conversion" $ do
    forM_
        (zip [1 :: Int ..] fixtures)
        $ \(fixtureIdx, (hsmSource, mettaSource)) -> do
            let fixtureLabel = "fixture #" ++ show fixtureIdx
            describe fixtureLabel $ do
                it "parses HSM and MeTTa sources to the same atoms" $ do
                    expectRight (parseMettaText mettaSource) $ \expectedAtoms ->
                        expectRight (parseHsmText hsmSource) $ \hsmAtoms ->
                            hsmAtoms `shouldBe` expectedAtoms

                it "preserves atoms when rendered to HSM and parsed back" $ do
                    expectRight (parseHsmText hsmSource) $
                        roundTripAtoms renderHsmText parseHsmText

                it "preserves atoms when rendered to MeTTa and parsed back" $ do
                    expectRight (parseMettaText mettaSource) $
                        roundTripAtoms renderMettaText parseMettaText

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

roundTripAtoms :: (HasCallStack, Show e1, Show e2) => ([Atom] -> Either e1 String) -> (String -> Either e2 [Atom]) -> [Atom] -> Expectation
roundTripAtoms render parse atoms =
    expectRight (render atoms) $ \rendered ->
        expectRight (parse rendered) $ \roundTripped ->
            roundTripped `shouldBe` atoms
