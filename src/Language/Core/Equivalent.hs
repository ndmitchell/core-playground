{-# LANGUAGE ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core.Equivalent(
    equivalent
    ) where

import Data.Maybe
import Data.Tuple.Extra
import Data.Generics.Uniplate.Data
import Language.Core.Type
import Language.Core.Variables
import Language.Core.Operations


equivalent :: Exp -> Exp -> Bool
equivalent x y = eval x == eval y

eval :: Exp -> Exp
eval = relabel . nf . relabel
    where
        whnf (Let v x y) = whnf $ subst [(v,x)] y
        whnf (App (whnf -> Lam v x) y) = whnf $ subst [(v,y)] x
        whnf (App (whnf -> Case x alts) y) = whnf $ Case x $ map (second $ flip App y) alts
        whnf (Case (whnf -> x) alts) | Just (bs, bod) <- caseCon $ Case x alts = whnf $ subst bs bod
        whnf (Case (whnf -> Case x alts1) alts2) = Case x [(a, Case b alts2) | (a,b) <- alts1]
        whnf x = x

        nf = descend nf . whnf


caseCon :: Exp -> Maybe ([(Var,Exp)], Exp)
caseCon o@(Case (fromApps -> (Con c, xs)) alts) = Just $ head $ mapMaybe f alts ++ [error $ "Malformed case: " ++ show o]
    where f (PWild, x) = Just ([], x)
          f (PCon c2 vs, x) | c /= c2 = Nothing
                            | length vs /= length xs = error "Malformed arity"
                            | otherwise = Just (zip vs xs, x)
caseCon _ = Nothing

