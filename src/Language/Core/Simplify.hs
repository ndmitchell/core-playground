{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, PatternGuards, TupleSections, ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core.Simplify(
    simplify
    ) where

import Data.Maybe
import Data.List
import Data.Generics.Uniplate.Data
import Language.Core.Type
import Language.Core.Variables
import Language.Core.Operations
import Language.Core.Debug
import Language.Core.Equivalent


caseCon :: Exp -> Maybe ([(Var,Exp)], Exp)
caseCon o@(Case (fromApps -> (Con c, xs)) alts) = Just $ head $ mapMaybe f alts ++ [error $ "Malformed case: " ++ show o]
    where f (PWild, x) = Just ([], x)
          f (PCon c2 vs, x) | c /= c2 = Nothing
                            | length vs /= length xs = error "Malformed arity"
                            | otherwise = Just (zip vs xs, x)
caseCon _ = Nothing


simplify :: Exp -> Exp
simplify = debugAssertEq equivalent (fs . relabel)
    where
        fs = transform f

        f o@(App (fromLets -> (bs@(_:_), Lam v z)) q) = fs $ Let v q $ mkLets bs z
        f o@(Case (Let v x y) alts) = fs $ Let v x $ Case y alts
        f (App (Lam v x) y) = f $ Let v y x
        f (Let v x y) | cheap x || linear v y = fs $ subst [(v,x)] y
        f (LetRec [] y) = y
        f (LetRec xs y) | not $ null notRec = f $ LetRec isRec $ mkLets notRec y
            where used = concatMap (free . snd) xs
                  (isRec,notRec) = partition (flip elem used . fst) xs
        f o@(Case (Case on alts1) alts2) =  fs $ Case on $ map g alts1
            where g (PWild, c) = (PWild, Case c alts2)
                  g (PCon a vs, c) = (PCon a vs, Case c alts2)
        f x | Just ((unzip -> (vs, xs)), bod) <- caseCon x = fs $ mkLets (zip vs xs) bod
        f x = x

cheap (Var _) = True
cheap (Con _) = True
cheap (Lam _ _) = True
cheap _ = False


linear :: Var -> Exp -> Bool
linear v x = count v x <= 1

count :: Var -> Exp -> Int
count v (Var x) = if v == x then 1 else 0
count v (Lam w y) = if v == w then 0 else count v y * 2 -- lambda count is infinite, but 2 is close enough
count v (Let w x y) = count v x + (if v == w then 0 else count v y)
count v (LetRec xs y) = if v `elem` map fst xs then 0 else sum (map (count v . snd) xs) + count v y
count v (Case x alts) = count v x + maximum [if v `elem` varsP p then 0 else count v c | (p,c) <- alts]
count v (App x y) = count v x + count v y
count v _ = 0
