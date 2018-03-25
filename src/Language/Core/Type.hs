{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, PatternGuards, TupleSections, ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core.Type(
    Module(..),
    Var(..), Con(..), Exp(..), Pat(..),
    toModuleHSE, fromModuleHSE, toExpHSE, fromExpHSE
    )  where

import Data.Data
import Data.Char
import Data.Maybe
import Language.Haskell.Exts hiding (Module,Exp,Name,Pat,Var,Let,App,Case,Con,name,parse,Pretty)
import qualified Language.Haskell.Exts as H
import Language.Core.HSE
import Data.Semigroup
import Control.DeepSeq


---------------------------------------------------------------------
-- TYPE

newtype Var = V {fromVar :: String} deriving (Data,Typeable,Eq,Show,Ord,NFData)
newtype Con = C {fromCon :: String} deriving (Data,Typeable,Eq,Show,Ord,NFData)

data Exp
    = Var Var
    | Con Con
    | App Exp Exp
    | Lam Var Exp
    | Let Var Exp Exp -- non-recursive
    | LetRec [(Var, Exp)] Exp
    | Case Exp [(Pat,Exp)]
      deriving (Data,Typeable,Eq,Ord)

data Pat
    = PCon Con [Var]
    | PWild
      deriving (Data,Typeable,Eq,Ord)

newtype Module = Module {fromModule :: [(Var, Exp)]}
    deriving (Data,Typeable,Eq,Ord,NFData,Semigroup,Monoid)

instance NFData Exp where
    rnf (Var a) = rnf a
    rnf (Con a) = rnf a
    rnf (App a b) = rnf a `seq` rnf b
    rnf (Let a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (Lam a b) = rnf a `seq` rnf b
    rnf (Case a b) = rnf a `seq` rnf b

instance NFData Pat where
    rnf (PCon a b) = rnf a `seq` rnf b
    rnf PWild = ()

instance Read Exp where
    readsPrec _ s = [(f s, "")]
        where f = fromExp . deflate . fromParseResult . parseExp

instance Show Exp where
    show = prettyPrint . unparen . inflate . toExp
        where unparen (Paren _ x) = x
              unparen x = x

instance Read Module where
    readsPrec _ s = [(f s, "")]
        where f = fromModuleHSE . fromParseResult . parseModule

instance Show Module where
    show = prettyPrint . toModuleHSE


---------------------------------------------------------------------
-- FROM/TO HSE

fromExpHSE :: H.Exp S -> Exp
fromExpHSE = fromExp . deflate

toExpHSE :: Exp -> H.Exp H.SrcSpanInfo
toExpHSE = inflate . toExp


fromModuleHSE :: H.Module S -> Module
fromModuleHSE (deflate -> H.Module _ _ _ _ xs) = Module $ concatMap fromDecl xs

fromDecl :: Decl S -> [(Var,Exp)]
fromDecl (PatBind _ (PVar _ f) (UnGuardedRhs _ x) Nothing) = [(V $ fromName f, fromExp x)]
fromDecl TypeSig{} = []
fromDecl DataDecl{} = []
fromDecl TypeDecl{} = []
fromDecl InfixDecl{} = []
fromDecl (InstDecl _ _ _ xs) = concat [fromDecl x | InsDecl _ x <- fromMaybe [] xs]
fromDecl (ClassDecl _ _ _ _ xs) = concat [fromDecl x | ClsDecl _ x <- fromMaybe [] xs]
fromDecl x = error $ "Unhandled fromDecl: " ++ show x

fromExp :: H.Exp S -> Exp
fromExp (Lambda _ [PVar _ (Ident _ x)] bod) = Lam (V x) $ fromExp bod
fromExp (H.App _ x y) = App (fromExp x) (fromExp y)
fromExp (H.Var _ (UnQual _ x)) = Var $ V $ fromName x
fromExp (H.Con _ (UnQual _ x)) = Con $ C $ fromName x
fromExp (Paren _ x) = fromExp x
fromExp (H.Case _ x xs) = Case (fromExp x) $ map fromAlt xs
fromExp (H.Let _ (BDecls _ ds) x) =  LetRec (concatMap fromDecl ds) $ fromExp x
fromExp (ExpTypeSig _ x _) = fromExp x
fromExp (Lit _ x) = Con $ C $ prettyPrint x
fromExp x = error $ "Unhandled fromExp: " ++ show x

fromName :: H.Name S -> String
fromName (Ident _ x) = x
fromName (Symbol _ x) = x

fromAlt :: H.Alt S -> (Pat, Exp)
fromAlt (H.Alt _ pat (UnGuardedRhs _ bod) Nothing) = (fromPat pat, fromExp bod)
fromAlt x = error $ "Unhandled fromAlt: " ++ show x

fromPat :: H.Pat S -> Pat
fromPat (PParen _ x) = fromPat x
fromPat (PApp _ (UnQual _ c) xs) = PCon (C $ fromName c) $ map (V . fromPatVar) xs
fromPat PWildCard{} = PWild
fromPat x@PLit{} = PCon (C $ prettyPrint x) []
fromPat x = error $ "Unhandled fromPat: " ++ show x

fromPatVar :: H.Pat S -> String
fromPatVar (PVar _ x) = fromName x
fromPatVar x = error $ "Unhandled fromPatVar: " ++ show x


---------------------------------------------------------------------
-- TO 

toModuleHSE :: Module -> H.Module H.SrcSpanInfo
toModuleHSE = inflate . H.Module s Nothing [] [] . map (uncurry toDecl) . fromModule

toDecl :: Var -> Exp -> Decl S
toDecl (V f) x = PatBind s (PVar s $ toName f) (UnGuardedRhs s $ toExp x) Nothing

toExp :: Exp -> H.Exp S
toExp (Var (V x)) = H.Var s $ UnQual s $ toName x
toExp (Con (C x)) = H.Con s $ UnQual s $ toName x
toExp (Lam (V x) y) = Lambda s [PVar s $ toName x] $ toExp y
toExp (Let a b y) = H.Let s (BDecls s [toDecl a b]) $ toExp y
toExp (App x y) = H.App s (toExp x) (toExp y)
toExp (Case x y) = H.Case s (toExp x) (map toAlt y)
toExp (LetRec xs y) = H.Let s (BDecls s (map (uncurry toDecl) xs)) $ toExp y

toAlt :: (Pat, Exp) -> H.Alt S
toAlt (x,y) = H.Alt s (toPat x) (UnGuardedRhs s $ toExp y) Nothing

toPat :: Pat -> H.Pat S
toPat (PCon (C c) vs) = PApp s (UnQual s $ toName c) (map (PVar s . Ident s . fromVar) vs)
toPat PWild = PWildCard s

toName :: String -> H.Name S
toName xs@(x:_) | isAlphaNum x || x `elem` "'_(" = Ident s xs
                   | otherwise = Symbol s xs
