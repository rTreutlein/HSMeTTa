{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module OpenCog.MeTTa.Lib where

import GHC.Base (Symbol)

import qualified Data.Map as M

type Name = String

data Atom = Symbol Name | Variable Name |  Expr [Atom] | Eval Atom --Missing: Grounded Type
    deriving (Show,Eq,Ord)

type TruthValue = Atom

--pattern TV n tv = Expr [Symbol "tvlink", Symbol n,tv]

pattern Link n as tv = Expr (Symbol n : tv : as)

pattern Node t n tv = Expr [Symbol t , Symbol n , tv]

--pattern Link n as tv <- (Expr (Symbol n : as),[TV _ tv]) where
    --Link n as tv = (Expr (Symbol n : as),[TV n tv])

--pattern Node t n tv <- (Symbol n , [TV _ tv , Expr [Symbol ":",Symbol _, Symbol t]]) where
    --Node t n tv = (Symbol n , [TV n tv,Expr [Symbol ":", Symbol n, Symbol t]])

--pattern Node2 t n tv <- (Symbol n,_) where
    --Node2 t n tv = (node,M.singleton node [TV _ tv,Expr [Symbol ":",node, Symbol t]])
        --where node = Symbol n
    
noTv = Symbol "NoTV"
pattern Stv s c = Expr [Symbol "STV", Symbol s, Symbol c]
stv s c = Stv (show s) (show c)

atomType :: Atom -> String
atomType (Symbol n) = "Symbol"
atomType (Variable n) = "Variable"
atomType (Expr (Symbol t:_)) = t

isTV :: Atom -> Bool
isTV (Symbol "NoTV") = True
isTV (Stv _ _) = True
isTV _ = False

isLink :: Atom -> Bool
isLink (Expr (_:mtv:_)) = isTV mtv

atomGetAllNodes :: Atom -> [Atom]
atomGetAllNodes atom@(Link _ as _) = if isLink atom 
                                        then concatMap atomGetAllNodes as
                                        else [atom]
atomGetAllNodes atom = [atom]

nodeName :: Atom -> String
nodeName (Symbol n) = n
nodeName (Variable n) = n
nodeName (Expr (_:(Symbol n):_)) = n

atomFold :: (a -> Atom -> a) -> a -> Atom -> a
atomFold f v a@(Symbol _) = f v a
atomFold f v a@(Variable _) = f v a
atomFold f v a@(Expr (_:_:ls)) = if isLink a then f (foldl (atomFold f) v ls) a else f v a

atomMap :: (Atom -> Atom) -> Atom -> Atom
atomMap f n@(Symbol _) = f n
atomMap f n@(Variable _) = f n
atomMap f n@(Link t ls tv) = if isLink n then f $ Link t (map (atomMap f) ls) tv else f n

showAtom :: Atom -> String
showAtom (Symbol n) = n
showAtom (Variable n) = "$" ++ n
showAtom a@(Expr as) = "( " ++ unwords (map showAtom as)  ++ " )"

showAtomese :: Atom -> String
showAtomese (Symbol "NoTV") = ""
showAtomese (Symbol n) = n
showAtomese (Variable n) = "$" ++ n
showAtomese a@(Expr as) = if isLink a || isTV a
                        then "( " ++ unwords (map showAtomese as)  ++ " )"
                        else let Node _ n _ = a
                             in n

atomElem :: Atom -> Atom -> Bool
atomElem n a@(Expr ls) = atomFold (\b a -> a == n || b) False a
atomElem n a = n == a
