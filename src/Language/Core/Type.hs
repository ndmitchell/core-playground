{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, PatternGuards, TupleSections, ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core.Type(
    Module(..), modLookup,
    Var(..), Con(..), Exp(..), Pat(..),
    isVar, isCon, isApp, isLet, isLetRec, isLam, isCase,
    isPCon, isPWild,
    fromApps, fromLets, fromLams,
    mkApps, mkLets, mkLams,
    fromModule2, fromDecl, fromExp, toModule, toDecl, toExp
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

modLookup :: Module -> Var -> Maybe Exp
modLookup m x = lookup x $ fromModule m

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
        where f = fromModule2 . deflate . fromParseResult . parseModule

instance Show Module where
    show = prettyPrint . deflate . toModule

---------------------------------------------------------------------
-- SIMPLE OPERATIONS

isVar, isCon, isApp, isLet, isLetRec, isLam, isCase :: Exp -> Bool
isVar Var{} = True; isVar _ = False
isCon Con{} = True; isCon _ = False
isApp App{} = True; isApp _ = False
isLet Let{} = True; isLet _ = False
isLetRec LetRec{} = True; isLetRec _ = False
isLam Lam{} = True; isLam _ = False
isCase Case{} = True; isCase _ = False

isPCon, isPWild :: Pat -> Bool
isPCon PCon{} = True; isPCon _ = False
isPWild PWild{} = True; isPWild _ = False

fromApps :: Exp -> (Exp, [Exp])
fromApps (App x y) = (a, b ++ [y])
    where (a,b) = fromApps x
fromApps x = (x,[])

mkApps :: Exp -> [Exp] -> Exp
mkApps x (y:ys) = mkApps (App x y) ys
mkApps x [] = x

fromLams :: Exp -> ([Var], Exp)
fromLams (Lam x y) = (x:a, b)
    where (a,b) = fromLams y
fromLams x = ([], x)

mkLams :: [Var] -> Exp -> Exp
mkLams (y:ys) x = Lam y $ mkLams ys x
mkLams [] x = x

fromLets :: Exp -> ([(Var, Exp)], Exp)
fromLets (Let x y z) = ((x,y):a, b)
    where (a,b) = fromLets z
fromLets x = ([], x)

mkLets :: [(Var, Exp)] -> Exp -> Exp
mkLets [] x = x
mkLets ((a,b):ys) x = Let a b $ mkLets ys x


---------------------------------------------------------------------
-- FROM 

fromModule2 :: H.Module S -> Module
fromModule2 (H.Module _ _ _ _ xs) = Module $ concatMap fromDecl xs

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

toModule :: Module -> H.Module H.SrcSpanInfo
toModule = H.Module s Nothing [] [] . map (uncurry toDecl) . fromModule

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
