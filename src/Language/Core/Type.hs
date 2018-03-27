{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core.Type(
    Module(..),
    Var(..), Con(..), Exp(..), Pat(..),
    toModuleHSE, fromModuleHSE, toExpHSE, fromExpHSE
    )  where

import Data.Data
import Data.Char
import Data.Maybe
import Data.List.Extra
import Language.Haskell.Exts hiding (Module,Exp,Name,Pat,Var,Let,App,Case,Con,Pretty)
import qualified Language.Haskell.Exts as H
import Language.Core.HSE
import Data.Semigroup
import Control.DeepSeq


---------------------------------------------------------------------
-- TYPE

-- | A variable, used to represent free variables (likely bound in the 'Module')
--   and locally bound variables (in 'Case', 'Let', 'Lam' etc).
newtype Var = V {fromVar :: String} deriving (Data,Typeable,Eq,Show,Ord,NFData)

-- | A constructor. Note that the list constructors are by default described as
--   @C \"[]\"@ and @C \"(:)\"@.
newtype Con = C {fromCon :: String} deriving (Data,Typeable,Eq,Show,Ord,NFData)

-- | The main expression data type which is central to the package.
--   The 'Read' and 'Show' instances work against a subset of Haskell.
--
--   For operations not available directly you are encouraged to use a generics library
--   where possible, e.g. @uniplate@.
data Exp
    = -- | @variable@, a variable, either a free variable or a locally bound variable.
      Var Var
    | -- | @Ctor@, a constructor.
      Con Con
    | -- | @f x@, function application.
      App Exp Exp
    | -- | @\\v -> e@, lambda abstraction.
      Lam Var Exp
    | -- | @let v = x in y@, non-recursive let binding (so if you see @v@ in @x@, it isn't /this/ @v@).
      Let Var Exp Exp
    | -- | @let v = x in y@, recursive let binding, so any binding can point at any other.
      LetRec [(Var, Exp)] Exp
    | -- | @case x of {p1 -> e1; ...}@, case expression of flat and ordered alternatives.
      Case Exp [(Pat,Exp)]
      deriving (Data,Typeable,Eq,Ord)

-- | A pattern in a 'Case' expression'.
data Pat
    = -- | @C v1 ...@, a constructor followed by some number of variables.
      --   The constructor should be fully applied to the right number of arguments.
      PCon Con [Var]
    | -- | @_@, a wildcard pattern which matches anything.
      --   Usually occurs as the final alternative.
      PWild
      deriving (Data,Typeable,Eq,Ord)


-- | A set of bindings from function names to expressions.
--   The bindings may be mutually recursive.
--   The names in a module are not qualified in any way.
--
--   The 'Read' and 'Show' instances work against a subset of Haskell.
newtype Module = Module {fromModule :: [(Var, Exp)]}
    deriving (Data,Typeable,Eq,Ord,NFData,Semigroup,Monoid)

instance NFData Exp where
    rnf (Var a) = rnf a
    rnf (Con a) = rnf a
    rnf (App a b) = rnf a `seq` rnf b
    rnf (Let a b c) = rnf a `seq` rnf b `seq` rnf c
    rnf (LetRec a b) = rnf a `seq` rnf b
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

-- | Convert an expression from a @haskell-src-exts@ expression to one understood by this package.
--   The conversion will fail if it encounters something it doesn't understand.
fromExpHSE :: H.Exp S -> Exp
fromExpHSE = fromExp . deflate

-- | Convert from an expression to one understood by @haskell-src-exts@.
toExpHSE :: Exp -> H.Exp H.SrcSpanInfo
toExpHSE = inflate . toExp


-- | Convert an expression from a @haskell-src-exts@ module to one understood by this package.
--   The conversion will fail if it encounters something it doesn't understand.
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
fromPat (PApp _ (UnQual _ c) xs) = PCon (C $ fromName c) $ map V $ zipWithFrom fromPatVar 1 xs
fromPat PWildCard{} = PWild
fromPat x@PLit{} = PCon (C $ prettyPrint x) []
fromPat x = error $ "Unhandled fromPat: " ++ show x

fromPatVar :: Int -> H.Pat S -> String
fromPatVar i (PVar _ x) = fromName x
fromPatVar i PWildCard{} = "_" ++ show i
fromPatVar i x = error $ "Unhandled fromPatVar: " ++ show x


---------------------------------------------------------------------
-- TO

-- | Convert from a module to one understood by @haskell-src-exts@.
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
