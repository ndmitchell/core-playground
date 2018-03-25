{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for operating on haskell-src-exts expressions.
module Language.Core.HSE(deflate, inflate, S, s) where

import Data.Data
import Data.List.Extra
import Language.Haskell.Exts hiding (app)
import Control.Monad.Trans.State
import Data.Generics.Uniplate.Data


type S = SrcSpanInfo

s :: S
s = noSrcSpan

app = App s

names :: Data (f S) => f S -> [String]
names x = map f $ universeBi x
    where f (Ident (_ :: S) x) = x
          f (Symbol _ x) = x

fresh :: [String] -> [String]
fresh del = ["v" ++ show i | i  <- [1..]] \\ del

---------------------------------------------------------------------
-- DEFLATE

-- | Use fewer constructors to express the same program.
deflate :: Data (f S) => f S -> f S
deflate = transformBi deflateExp . transformBi deflatePat . transformBi deflateQName . transformBi deflateDecl . deflateWildcard

spec :: SpecialCon S -> QName S
spec UnitCon{} = UnQual s $ Ident s "()"
spec ListCon{} = UnQual s $ Ident s "[]" 
spec Cons{} = UnQual s $ Symbol s ":"
spec (TupleCon _ Boxed i) = UnQual s $ Ident s $ "(" ++ replicate (i-1) ',' ++ ")"
spec x = Special s x

deflateDecl :: Decl S -> Decl S
deflateDecl (FunBind _ [Match _ f vars (UnGuardedRhs _ x) decs]) =
    PatBind s (PVar s f) (UnGuardedRhs s $ Lambda s vars $ maybe id (Let s) decs x) Nothing
deflateDecl (FunBind _ [InfixMatch _ a b c d e]) = deflateDecl $ FunBind s [Match s b (a:c) d e]
deflateDecl x = x

deflateQName :: QName S -> QName S
deflateQName (Special _ x) = spec x
deflateQName x = x

deflateExp :: Exp S -> Exp S
deflateExp (Lambda _ ps x) | length ps /= 1 = foldr (\p x -> Lambda s [p] x) x ps
deflateExp (LeftSection _ x (QVarOp _ y)) = App s (Var s y) x
deflateExp (LeftSection _ x (QConOp _ y)) = App s (Con s y) x
deflateExp (RightSection _ (QVarOp _ y) x) = Paren s $ Var s (UnQual s $ Ident s "flip") `app` Var s y `app` Paren s x
deflateExp (RightSection _ (QConOp _ y) x) = Paren s $ Var s (UnQual s $ Ident s "flip") `app` Con s y `app` Paren s x
deflateExp (List _ []) = Con s $ spec $ ListCon s
deflateExp (List _ (x:xs)) =  Paren s $ Con s (spec $ Cons s) `app` Paren s x `app` deflateExp (List s xs)
deflateExp (Tuple _ b xs) = foldl (App s) (Con s $ spec $ TupleCon s b $ length xs) xs
deflateExp (InfixApp _ a (QVarOp _ b) c) = Var s b `app` a `app` c
deflateExp (InfixApp _ a (QConOp _ b) c) = Con s b `app` a `app` c
deflateExp (NegApp _ x) = Paren s $ Var s (UnQual s $ Ident s "negate") `app` Paren s x
deflateExp o@(Lambda _ [p] e) | not $ isPVar p = Lambda s [PVar s new] $ Case s (Var s $ UnQual s new) [Alt s p (UnGuardedRhs s e) Nothing]
    where new:_ = map (Ident s) $ fresh $ names o
deflateExp (Case _ ov@(Var _ (UnQual _ v)) (Alt _ op@(PVar _ p) (UnGuardedRhs _ e) Nothing:_))
    | v == p = e
    | otherwise = Let s (BDecls s [PatBind s op (UnGuardedRhs s ov) Nothing]) e
deflateExp (If _ a b c) = Case s a [f "True" b, f "False" c]
    where f con x = Alt s (PApp s (UnQual s $ Ident s con) []) (UnGuardedRhs (ann x) x) Nothing
deflateExp (Let _ (BDecls s bs) x) = foldr (\b x -> Let s (BDecls s [b]) x) x bs -- FIXME: Only safe if variables are not mutually recursive
deflateExp (EnumFromTo _ x y) = Paren s $ Var s (UnQual s $ Ident s "enumFromTo") `app` x `app` y
deflateExp (EnumFromThen _ x y) = Paren s $ Var s (UnQual s $ Ident s "enumFromThen") `app` x `app` y
deflateExp (EnumFromThenTo _ x y z) = Paren s $ Var s (UnQual s $ Ident s "enumFromThenTo") `app` x `app` y `app` z
deflateExp (EnumFrom _ x) = Paren s $ Var s (UnQual s $ Ident s "enumFrom") `app` x
deflateExp (ListComp _ res xs) = lst xs
    where
        {-
        -- variants returning a Maybe
        may [] = Just $ Con s (UnQual s $ Ident s "Just") `app` Paren s res
        may (QualStmt _ (LetStmt _ bind):xs) = deflateExp . Let s bind <$> may xs
        may (QualStmt _ (Qualifier _ e):xs) = (\xs -> Paren s $ deflateExp $ If s e xs $ Con s $ UnQual s $ Ident s "Nothing") <$> may xs
        may _ = Nothing

        -- optimised shortcuts (use map or mapMaybe)
        lst (QualStmt (Generator _ p e):[]) | fasterListComp, irrefutable p = Var (UnQual $ Ident "map") `app` deflateExp (Lambda sl [p] res) `app` e
        lst o@(QualStmt (Generator _ p e):xs) | fasterListComp, Just ans <- may xs =
            Var (UnQual $ Ident "mapMaybe") `app` deflateExp (Lambda sl [PVar new] $ bod ans) `app` e
            where new:_ = map Ident $ fresh $ names $ ListComp res o
                  bod ans = deflateExp $ Case (Var $ UnQual new) $
                            [Alt sl p (UnGuardedRhs ans) $ BDecls []] ++
                            [Alt sl PWildCard (UnGuardedRhs $ Con $ UnQual $ Ident "Nothing") $ BDecls [] | not $ irrefutable p]
        -}

        -- from the report, returning a list
        lst o@(QualStmt _ (Generator _ p e):xs) = Var s (UnQual s $ Ident s "concatMap") `app` deflateExp (Lambda s [PVar s new] bod) `app` e
          where new:_ = map (Ident s) $ fresh $ names $ ListComp s res o
                bod = deflateExp $ Case s (Var s $ UnQual s new)
                          [Alt s p (UnGuardedRhs s $ lst xs) Nothing
                          ,Alt s (PWildCard s) (UnGuardedRhs s $ deflateExp $ List s []) Nothing]
        lst (QualStmt _ (Qualifier _ e):xs) = Paren s $ deflateExp $ If s e (lst xs) (deflateExp $ List s [])
        lst (QualStmt _ (LetStmt _ bind):xs) = Paren s $ deflateExp $ Let s bind $ lst xs
        lst [] = deflateExp $ List s [res]
        lst xs = ListComp s res xs
deflateExp x = x

{-
irrefutable :: Pat S -> Bool
irrefutable x = case deflatePat x of
    PApp (UnQual (Ident ('(':(dropWhile (== ',') -> ")")))) xs -> all irrefutable xs
    PVar{} -> True
    _ -> False
-}

deflatePat :: Pat S -> Pat S
deflatePat (PInfixApp _ a b c) = PApp s b [a,c]
deflatePat (PList _ []) = PApp s (spec $ ListCon s) []
deflatePat (PTuple _ b xs) = PApp s (spec $ TupleCon s b $ length xs) xs
deflatePat x = x

-- removing wildcards needs some state (the unused variables), so has to be monadic
deflateWildcard :: Data (f S) => f S -> f S
deflateWildcard x = evalState (transformBiM f x) (["_" ++ show i | i <- [1..]] \\ names x)
    where f :: Pat S -> State [String] (Pat S)
          f PWildCard{} = do v:vs <- get; put vs; return $ PVar s $ Ident s v
          f x = return x

isPVar PVar{} = True; isPVar _ = False


---------------------------------------------------------------------
-- INFLATE

-- | Add back in syntactic forms to make it more readable.
inflate :: Data (f S) => f S -> f S
inflate =
    transformBi inflateRhs . transformBi inflateAlt . transformBi inflateRhs .
    transformBi inflatePat . transformBi inflateExp .
    transformBi (Paren s) . transformBi (PParen s)

inflateExp :: Exp S -> Exp S
inflateExp (Lambda _ ps (Paren _ x)) = inflateExp $ Lambda s ps x
inflateExp (Lambda _ ps1 (Lambda _ ps2 x)) | disjoint ps1 ps2 = Lambda s (ps1++ps2) x
inflateExp (Paren _ (Paren _ x)) = inflateExp $ Paren s x
inflateExp (Paren _ (Var _ x)) = Var s x
inflateExp (Paren _ (Con _ x)) = Con s x
inflateExp (Paren _ (List _ x)) = List s x
inflateExp (Paren _ (Lit _ x)) = Lit s x
inflateExp (App _ (Paren _ (App _ a b)) c) = App s (App s a b) c
inflateExp (Con _ (UnQual _ (Symbol _ "[]"))) = List s []
inflateExp x = x

inflatePat :: Pat S -> Pat S
inflatePat (PParen _ (PParen _ x)) = PParen s x
inflatePat (PParen _ (PVar _ x)) = PVar s x
inflatePat (PApp _ (UnQual _ (Symbol _ "[]")) []) = PList s []
inflatePat x = x

inflateRhs :: Rhs S -> Rhs S
inflateRhs (UnGuardedRhs _ (Paren _ x)) = UnGuardedRhs s x
inflateRhs x = x

inflateAlt :: Alt S -> Alt S
inflateAlt (Alt _ (PParen _ p) x y) = Alt s p x y
inflateAlt x = x
