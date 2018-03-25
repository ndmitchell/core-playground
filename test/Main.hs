{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import Language.Core
import Data.List
import System.Directory
import Control.Monad.Extra
import Data.Monoid
import Data.Tuple.Extra
import Data.Generics.Uniplate.Data
import Debug.Trace


main :: IO ()
main = do
    files <- filter (not . isPrefixOf ".") <$> getDirectoryContents "imports"
    base <- fmap mconcat $ forM files $ \file -> do
        parseCoreFile ("imports/" ++ file)
    qed <- parseCoreFile "qed.hs"
    let (check, rest) = partition (isPrefixOf "proof" . fromVar . fst) $ fromModule $ transformBi hack $ qed <> base
    mapM_ (uncurry (equivalent $ Module rest) . toProof . snd) check

unwild "Nothing" = Just $ PCon (C "Just") [V "_magic"]
unwild "Just" = Just $ PCon (C "Nothing") []
unwild "(:)" = Just $ PCon (C "[]") []
unwild "[]" = Just $ PCon (C "(:)") [V "_magic1",V "_magic2"]
unwild _ = Nothing

hack :: Exp -> Exp
hack (Let a b x) = transform hack $ subst [(a,b)] x
hack (LetRec bs x) = transform hack $ subst bs x
hack (Case x [(PCon a1 a2,a3),(PWild,b3)]) | Just b12 <- unwild $ fromCon a1 = hack $ Case x [(PCon a1 a2,a3),(b12,b3)]
hack (Case x ps@[(PCon _ a, _), (PCon _ b, _)]) | length a > length b = Case x $ reverse ps
hack x = x

toProof :: Exp -> (Exp, Exp)
toProof (fromLams -> (vs, fromApps -> (Var (V "==="), [a,b]))) = (a,b)

printEq :: Exp -> Exp -> IO ()
printEq (show -> a) (show -> b)
    | short a, short b = putStrLn $ a ++ "  ===  " ++ b
    | otherwise = putStr $ unlines [a,"  ===",b]
    where short x = case lines x of [a] -> length a < 30; _ -> False

equivalent :: Module -> Exp -> Exp -> IO ()
equivalent m (norm -> a) (norm -> b) = do
    putStrLn "== EQUIVALENT =="
    f [] [(a,b)]
    putStrLn "== SUCCESS =="
    where
        f done todo = do
            let new = filter (uncurry (/=)) todo \\ done
            forM_ new $ \(a,b) -> do
                printEq a b
                -- print b
                -- print $ whnf m $ norm b
                case unpeel' m a b of
                    Nothing -> fail "FAILED TO PROVE"
                    Just xs -> f ((a,b):done) xs

norm = relabel . simplify

unpeel' m x y = map (both norm) <$> unpeel m (op x) (op y)
    where op = norm . whnf m . norm

unpeel :: Module -> Exp -> Exp -> Maybe [(Exp, Exp)]
unpeel m (Case x1 x2) (Case y1 y2)
    | map fst x2 == map fst y2
    = ((x1,y1):) . concat <$> zipWithM (unpeel' m) (map snd x2) (map snd y2)
unpeel m (fromApps -> (Con x1, x2)) (fromApps -> (Con y1, y2))
    | x1 == y1 = Just $ zip x2 y2
unpeel m (fromApps -> (Var x1, x2)) (fromApps -> (Var y1, y2))
    | x1 == y1 = Just $ zip x2 y2
unpeel m (Lam x1 x2) (Lam y1 y2)
    | x1 == y1 = Just [(x2,y2)]
unpeel m x y = error $ show (x, y)

whnf :: Module -> Exp -> Exp
whnf m = f
    where
        f x | False, traceShow x False = undefined
        f (Var v) | Just e <- modLookup m v = whnf m e
        f (App a b) = case f a of
            Lam v x -> f $ subst [(v,b)] x
            a -> App a b
        f (Case a b) = case f a of
            a@(fromApps -> (Con{}, _)) -> f $ simplify $ Case a b
            a@Case{} -> f $ simplify $ Case a b
            a -> Case a b
        f x = x
