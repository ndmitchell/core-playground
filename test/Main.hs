{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import Language.Core
import Data.List.Extra
import System.Directory
import Control.Monad.Extra
import Data.Monoid
import Data.Generics.Uniplate.Data


main :: IO ()
main = do
    debugEnable "debug.log"
    files <- filter (not . isPrefixOf ".") <$> getDirectoryContents "imports"
    base <- fmap mconcat $ forM files $ \file -> do
        parseCoreFile ("imports/" ++ file)
    qed <- parseCoreFile "qed.hs"
    let (check, rest) = partition (isPrefixOf "proof" . fromVar . fst) $ fromModule $ simpler $ qed <> base
    mapM_ (prove (Module rest) . toProp . snd) check


-- | Reduce the language we have to deal with, without changing semantics
simpler :: Module -> Module
simpler = descendBi (simplify . transform f . simplifyAlts ds)
    where
        f (Let a b x) = subst [(a,b)] x
        f x = x

        ds = [[(C "Nothing",0),(C "Just",1)]
             ,[(C "[]",0),(C "(:)",2)]
             ,[(C "LT",0),(C "EQ",0),(C "GT",0)]
             ]


data Prop = Prop [Var] Exp Exp
    deriving Eq

instance Show Prop where
    show (Prop quant (show -> a) (show -> b))
        | short a, short b = pre quant ++ " " ++ a ++ "  ===  " ++ b
        | otherwise = pre quant ++ "\n  " ++ indent a ++ "\n  ===\n  " ++ indent b
        where
            indent = replace "\n" "\n  "
            short x = case lines x of [a] -> length a < 40; _ -> False
            pre quant = "forall " ++ unwords (map fromVar quant) ++ "."

toProp :: Exp -> Prop
toProp (fromLams -> (vs, fromApps -> (Var (V "==="), [a,b]))) = Prop vs a b

isPropEq :: Prop -> Bool
isPropEq (Prop _ x y) = x == y

prove :: Module -> Prop -> IO ()
prove m p = do
    putStrLn "== EQUIVALENT? =="
    print p
    f [] [p]
    putStrLn "== QED =="
    where
        f done todo = do
            let new = filter (not . isPropEq) (map simpP todo) \\ done
            forM_ new $ \p -> do
                print p
                let p' = simpP $ whnfP m p
                print p'
                case descendP p' of
                    Just xs -> f (p:done) xs
                    Nothing -> do
                        print p'
                        fail "FAILED TO PROVE"


simpP :: Prop -> Prop
simpP = label . short . simp
    where
        -- run the simplifier
        simp (Prop vs x y) = Prop vs (f x) (f y)
            where f x = relabelAvoid (vs ++ free x) $ simplify x

        -- remove any quantifiers that don't matter, put them in a good order
        short (Prop vs x y) = Prop vs2 x y
            where vs2 = filter (`elem` vs) $ nubOrd (free x ++ free y)

        -- try putting things in a canonical order
        label (Prop vs x y) = Prop vs2 (f x) (f y)
            where
                vs2 = take (length vs) $ fresh []
                f = lbl . subst (zip vs $ map Var vs2) . lbl
                lbl x = relabelAvoid (vs2 ++ free x) x



whnfP :: Module -> Prop -> Prop
whnfP m (Prop vs x y) = Prop vs (f x) (f y)
    where f = whnfCount 1000 m

descendP :: Prop -> Maybe [Prop]
descendP (Prop vs e1 e2) = f e1 e2
    where
        f (Lam v1 x1) (Lam v2 x2)
            | v1 == v2
            = Just [Prop (vs++[v1]) x1 x2]

        f (Case x1 as1) (Case x2 as2)
            | map fst as1 == map fst as2
            = Just $ Prop vs x1 x2 : zipWith g as1 as2
            where g (p1@(PCon _ vs2), a1) (p2, a2) = Prop (vs ++ vs2) (mkAlt x1 p1 a1) (mkAlt x2 p2 a2)
                  g (PWild, x1) (_, x2) = Prop vs x1 x2

        f (fromApps -> (x1, x2)) (fromApps -> (y1, y2))
            | x1 == y1 && isVarCon x1
            = Just $ zipWith (Prop vs) x2 y2

        f _ _ = Nothing

        isVarCon Var{} = True
        isVarCon Con{} = True
        isVarCon _ = False

mkAlt (Var v) (PCon c vs) = Let v (mkApps (Con c) $ map Var vs)
mkAlt _ _ = id
