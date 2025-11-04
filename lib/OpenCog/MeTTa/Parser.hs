{-# LANGUAGE TemplateHaskell #-}
module OpenCog.MeTTa.Parser where

import Prelude hiding (fail)

import Iso
import Syntax hiding (Syntax,SynIso)
import qualified Syntax as S
import qualified Control.Monad.State as M

import Control.Monad.Trans.Class
import Control.Arrow

import OpenCog.MeTTa.Lib

import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (listToMaybe)

import Debug.Trace

newtype State = State {sText :: String}
    deriving (Show)

instance SyntaxState State where
    getText = sText
    addText str sta = sta {sText = str ++ sText sta}
    setText str sta = sta {sText = str}

type Syntax a = S.Syntax (M.StateT State) a
type SynIso a b = S.SynIso (M.StateT State) a b

$(defineIsomorphisms ''Atom)

traceIso :: Show a => SynIso a a
traceIso = mkIso (\a -> traceShow a a) (\a -> traceShow a a)

-------------------------------------------------------------------------------

anyWord :: Syntax String
anyWord = some (token (\c -> c /= ' ' && c /= '(' && c /= ')')) <&& optSpace

node :: Syntax Atom
node = (text "$" >>> anyWord >>> variable) <+> (anyWord >>> symbol)

parseExpr :: Syntax Atom
parseExpr = text "(" <&& optSpace >>> many (parseExpr <+> node) <&& text ")" <&& skip1 >>> expr

parseExprH :: Syntax Atom
parseExprH = text "(" <&& optSpace >>> many (parseExprH <+> hnode <+> node) <&& text ")" <&& skip1 >>> expr

parseEval :: Syntax Atom
parseEval = text "!" &&> parseExpr >>> eval

parseMeTTa :: Syntax [Atom]
parseMeTTa = skip &&> some (comment &&> (parseEval <+> parseExpr) <&& skip)

-------------------------------------------------------------------------------

specialList = ["==","->",":","=","+","-","/","*","∧","∨"]

forbidden = " \n()[]"
forbidden2 = ["if","then","else","case","of"]

anyOf :: [a] -> (a -> Syntax b) -> Syntax b
anyOf [] _ = fail
anyOf (x:xs) syn = syn x <+> anyOf xs syn

anyhWord :: Syntax String
anyhWord = (some (token (`notElem` forbidden)) <&& optSpace) >>> isoFilter (`notElem` specialList ++ forbidden2)

hnode :: Syntax Atom
hnode = (text "$" >>> anyhWord >>> variable) <+> (anyhWord >>> symbol) <+> parseExprH

special :: Syntax Atom
special = anyOf specialList string <&& skipSpace >>> symbol

basicExpr :: Syntax Atom
basicExpr = some hnode >>> expr

expcurry :: SynIso (Atom,[Atom]) Atom
expcurry = isoFoldl (expr <<< tolist2)

cExpr :: Syntax Atom
cExpr = expcurry <<< (hnode &&& many hnode)

baseExpr :: Syntax Atom
baseExpr = (expr <<< many hnode) <+> hnode

skipLine :: Syntax ()
skipLine = ignoreAny [(),()] <<< many (text "\n")

skip1 :: Syntax ()
skip1 = skipSpace <&& (ignoreAny Nothing <<< optional (text "\n")) <&& optSpace

opt1 :: Syntax ()
opt1 = skipSpace <&& (ignoreAny (Just ()) <<< optional (text "\n")) <&& optSpace

skip = skipSpace <&& skipLine <&& skipSpace

specialExpr = (baseExpr &&& special <&& skip1 &&& anyExpr) >>> handle
    where handle = Iso f g
          f (e1,(s,e2)) = pure $ Expr [s,e1,e2]
          g (Expr [s,e1,e2]) = pure (e1,(s,e2))
          g _ = lift $ Left "not an special Expr"

specialSymbol a = opt1 &&> string a <&& optSpace >>> symbol

specialSkip s = opt1 &&> optSpace &&> optSpace &&> text s <&& optSpace

ifExpr :: Syntax Atom
ifExpr = (specialSymbol "if" &&&
            ((anyExpr <&& specialSkip "then") &&&
             ((anyExpr <&& specialSkip "else") &&&
              (anyExpr >>> tolist1) >>> cons
             ) >>> cons
            )
          ) >>> cons >>> expr

caseExpr :: Syntax Atom
caseExpr =
    ( specialSymbol "case"
        &&& ( ((anyExpr <&& caseOf) &&& caseBranches) >>> tolist2 )
    ) >>> cons >>> expr

caseOf :: Syntax ()
caseOf = syntax parse print
    where
        parse text =
            let rest = dropWhile (== ' ') text
            in case stripPrefix "of" rest of
                Just remaining -> Right ((), remaining)
                Nothing -> Left "expected 'of' in case expression"
        print () = Right " of"

caseBranches :: Syntax Atom
caseBranches = some caseBranch >>> expr

caseBranch :: Syntax Atom
caseBranch = syntax parseBranch printBranch

parseBranch :: String -> Either String (Atom, String)
parseBranch input = do
    let (indent, rest0) = span (`elem` " \n") input
    if null indent
        then Left "expected indentation before case branch"
        else do
            (patAtom, rest1) <- runSyntax baseExpr rest0
            let rest2 = dropWhile (== ' ') rest1
            rest3 <- stripArrow rest2
            let (bodyChunk, rest4) = splitBodyText rest3
            bodyAtom <- parseBodyAtom bodyChunk
            pure (Expr [patAtom, bodyAtom], rest4)

printBranch :: Atom -> Either String String
printBranch (Expr [patAtom, bodyAtom]) = do
    patText <- renderWith baseExpr patAtom
    bodyText <- renderWith anyExpr bodyAtom
    pure ("\n    " ++ patText ++ " -> " ++ bodyText)
printBranch _ = Left "expected branch expression"

stripArrow :: String -> Either String String
stripArrow text =
    case stripPrefix "->" text of
        Just rest -> Right (dropWhile (== ' ') rest)
        Nothing -> Left "expected '->' in case branch"

parseBodyAtom :: String -> Either String Atom
parseBodyAtom text = do
    let bodyText = rstrip text
    (bodyAtom, rest) <- runSyntax anyExpr bodyText
    if all isSpace rest
        then Right bodyAtom
        else Left "unexpected content after case branch body"

splitBodyText :: String -> (String, String)
splitBodyText text =
    case findNextBranchStart text of
        Just idx -> let (bodyPart, rest) = splitAt idx text in (rstrip bodyPart, rest)
        Nothing -> (rstrip text, "")

findNextBranchStart :: String -> Maybe Int
findNextBranchStart text =
    listToMaybe
        [ idx
        | idx <- candidateIndices
        , let sub = drop idx text
              spaceCount = length (takeWhile (== ' ') sub)
        , spaceCount >= 2
        , startsWithBranch (drop spaceCount sub)
        ]
    where
        candidateIndices = [i | (i, c) <- zip [0 ..] text, c == ' ']

startsWithBranch :: String -> Bool
startsWithBranch txt =
    case runSyntax baseExpr txt of
        Right (_, rest) ->
            let rest' = dropWhile (== ' ') rest
            in isPrefixOf "->" rest'
        Left _ -> False

runSyntax :: Syntax Atom -> String -> Either String (Atom, String)
runSyntax syn str =
    case M.runStateT (apply syn ()) (State str) of
        Right (atom, st) -> Right (atom, sText st)
        Left err -> Left err

renderWith :: Syntax Atom -> Atom -> Either String String
renderWith syn atom =
    fmap sText $ M.execStateT (unapply syn atom) (State "")

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

anyExpr = ifExpr <+> caseExpr <+> specialExpr <+> baseExpr

parseHexpr = (text "!" &&> anyExpr >>> eval) <+> anyExpr

comment :: Syntax ()
comment = many ((text ";" &&> manyTill anytoken (text "\n") <&& skipLine) >>> ignoreAny "") >>> ignoreAny []

parseFile :: Syntax [Atom]
parseFile = skip &&> some (comment &&> parseHexpr <&& skip)
