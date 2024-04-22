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

import Debug.Trace

newtype State = State {sText :: String}

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

specialList = ["==","->",":","=","+","-","/","*"]

forbidden = " \n()[]"
forbidden2 = ["if","then","else"]

anyOf :: [a] -> (a -> Syntax b) -> Syntax b
anyOf [] _ = fail
anyOf (x:xs) syn = syn x <+> anyOf xs syn

anyhWord :: Syntax String
anyhWord = (some (token (`notElem` forbidden)) <&& optSpace) >>> isoFilter (`notElem` specialList ++ forbidden2)

hnode :: Syntax Atom
hnode = (text "$" >>> anyhWord >>> variable) <+> (anyhWord >>> symbol) <+> (text "[" <&& optSpace >>> anyExpr <&& text "]" <&& optSpace) <+> parseExprH

special :: Syntax Atom
special = anyOf specialList string <&& skipSpace >>> symbol

basicExpr :: Syntax Atom
basicExpr = some hnode >>> expr

expcurry :: SynIso (Atom,[Atom]) Atom
expcurry = isoFoldl (expr <<< tolist2)

cExpr :: Syntax Atom
cExpr = expcurry <<< (hnode &&& many hnode)

skipLine :: Syntax ()
skipLine = ignoreAny [(),()] <<< many (text "\n")

skip1 :: Syntax ()
skip1 = skipSpace <&& (ignoreAny Nothing <<< optional (text "\n")) <&& optSpace

opt1 :: Syntax ()
opt1 = skipSpace <&& (ignoreAny (Just ()) <<< optional (text "\n")) <&& optSpace

skip = skipSpace <&& skipLine <&& skipSpace

specialExpr = (cExpr &&& special <&& skip1 &&& anyExpr) >>> handle
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

anyExpr = ifExpr <+> specialExpr <+> cExpr

parseHexpr = (text "!" &&> anyExpr >>> eval) <+> anyExpr

comment = many ((text ";" &&> manyTill anytoken (text "\n") <&& skipLine) >>> ignoreAny "") >>> ignoreAny []

parseFile :: Syntax [Atom]
parseFile = skip &&> some (comment &&> parseHexpr <&& skip)


