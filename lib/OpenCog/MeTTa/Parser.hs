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
import Data.List (stripPrefix)
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

specialList :: [String]
specialList = ["==","->",":","=","+","-","/","*","∧","∨"]

forbidden :: String
forbidden = " \n()[]"
forbidden2 :: [String]
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

baseExpr :: Syntax Atom
baseExpr = some hnode >>> (inverse tolist1 <+> expr)

skipLine :: Syntax ()
skipLine = ignoreAny [(),()] <<< many (text "\n")

skip1 :: Syntax ()
skip1 = skipSpace <&& (ignoreAny Nothing <<< optional (text "\n")) <&& optSpace

opt1 :: Syntax ()
opt1 = skipSpace <&& (ignoreAny (Just ()) <<< optional (text "\n")) <&& optSpace

skip :: Syntax ()
skip = skipSpace <&& skipLine <&& skipSpace

specialExpr :: Syntax Atom
specialExpr = (baseExpr &&& special <&& skip1 &&& anyExpr) >>> handle
    where handle = Iso f g
          f (e1,(s,e2)) = pure $ Expr [s,e1,e2]
          g (Expr [s,e1,e2]) = pure (e1,(s,e2))
          g _ = lift $ Left "not an special Expr"

specialSymbol :: String -> Syntax Atom
specialSymbol a = opt1 &&> string a <&& optSpace >>> symbol

specialSkip :: String -> Syntax ()
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
caseBranch =
    branchIndent
        &&> ( ((baseExpr <&& branchArrow) &&& anyExpr)
                >>> tolist2
                >>> expr
            )
    where
        branchIndent =
            (many (text "\n") >>> ignoreAny [()])
                &&> (some (text " ") >>> ignoreAny (replicate 4 ()))
        branchArrow =
            optSpace &&> text "->" <&& optSpace

letExpr :: Syntax Atom
letExpr = specialSkip "let" &&> letBinding <&& specialSkip "in" &&& anyExpr >>> handle
    where handle = Iso f g
          f ((t,p),b) = pure $ Expr [Symbol "let",t,p,b]
          g (Expr [Symbol "let",t,p,b]) = pure ((t,p),b)
          g _ = lift $ Left "not a let Expr"

letBinding :: Syntax (Atom,Atom)
letBinding = (anyExpr  <&& text "=" <&& sepSpace &&& anyExpr)

anyExpr :: Syntax Atom
anyExpr = ifExpr <+> caseExpr <+> letExpr <+> specialExpr <+> baseExpr

parseHexpr :: Syntax Atom
parseHexpr = (text "!" &&> anyExpr >>> eval) <+> anyExpr

comment :: Syntax ()
comment = many ((text ";" &&> manyTill anytoken (text "\n") <&& skipLine) >>> ignoreAny "") >>> ignoreAny []

parseFile :: Syntax [Atom]
parseFile = skip &&> some (comment &&> parseHexpr <&& skip)
