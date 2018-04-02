{-# LANGUAGE ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core.Variables(
    vars, varsP, free, subst, relabel, relabelAvoid, fresh,
    ) where

import Data.Maybe
import Data.List
import Data.Tuple.Extra
import Control.Monad
import Control.Monad.Trans.State
import Data.Generics.Uniplate.Data
import Language.Core.Type


-- | Get all the variables mentioned in an 'Exp', including inside 'Pat'.
vars :: Exp -> [Var]
vars = universeBi

-- | Get all the variables mentioned in a 'Pat'.
varsP :: Pat -> [Var]
varsP = universeBi

-- | Find all the free variables of an expression - those whose value in the surrounding
--   scope is used.
free :: Exp -> [Var]
free (Var x) = [x]
free (Con _) = []
free (App x y) = nub $ free x ++ free y
free (Lam x y) = delete x $ free y
free (Case x y) = nub $ free x ++ concat [free b \\ varsP a | (a,b) <- y]
free (Let a b y) = nub $ free b ++ delete a (free y)
free (LetRec xs y) = nub $ concatMap free (y:map snd xs) \\ map fst xs


-- | Perform a substitution of the given variables for expressions. Note that
--   if any of the substitution expressions have free variables that are bound
--   locally in the target expression it will go wrong.
subst :: [(Var,Exp)] -> Exp -> Exp
subst [] x = x
subst ren e = case e of
    Var x -> fromMaybe (Var x) $ lookup x ren
    App x y -> App (f [] x) (f [] y)
    Lam x y -> Lam x (f [x] y)
    Case x y -> Case (f [] x) [(a, f (varsP a) b) | (a,b) <- y]
    Let a b y -> Let a (f [] b) $ f [a] y
    LetRec xs y -> let f' = f (map fst xs) in LetRec (map (second f') xs) $ f' y
    x -> x
    where f del = subst (filter (flip notElem del . fst) ren)


-- | Relabel an expression so each bound variable occurs once and no variables overlap with
--   free variables from the initial expression.
relabel :: Exp -> Exp
relabel x = relabelAvoid (free x) x

-- | Relabel given a set of variables to avoid, typically the 'free' variables.
--   If you want exactly the 'free' variables use 'relabel' - typically used when you also want
--   to exclude some additional set of variables as well.
relabelAvoid :: [Var] -> Exp -> Exp
relabelAvoid xs x = evalState (f [] x) (fresh xs)
    where
        f :: [(Var,Var)] -> Exp -> State [Var] Exp
        f mp (Lam v x) = do i <- var; Lam i <$> f ((v,i):mp) x
        f mp (Let v x y) = do i <- var; Let i <$> f mp x <*> f ((v,i):mp) y
        f mp (LetRec (unzip -> (vs, es)) y) = do
            is <- replicateM (length vs) var
            let f' = f $ zip vs is ++ mp
            es <- mapM f' es
            LetRec (zip is es) <$> f' y
        f mp (Case x alts) = Case <$> f mp x <*> mapM (g mp) alts
        f mp (App x y) = App <$> f mp x <*> f mp y
        f mp (Var x) = return $ Var $ fromMaybe x $ lookup x mp
        f mp x = return x

        g mp (PWild, x) = (,) PWild <$> f mp x
        g mp (PCon c vs, x) = do is <- replicateM (length vs) var; (,) (PCon c is) <$> f (zip vs is ++ mp) x

        var = do
            ss <- get
            case ss of
                [] -> error "relabelAvoid, ran out of variables"
                x:xs -> put xs >> return x

-- | Create an infinite list of fresh variables given a list of variables to exclude.
fresh :: [Var] -> [Var]
fresh used = map V (concatMap f [1..]) \\ used
    where f 1 = map return ['a'..'z']
          f i = [a ++ b | a <- f 1, b <- f (i-1)]
