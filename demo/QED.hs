{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import Language.Core
import Data.List.Extra
import Control.Monad.Extra
import Data.Monoid
import Data.Generics.Uniplate.Data
import System.Environment
import Prelude


main :: IO ()
main = do
    args <- getArgs
    when (null args) $
        putStrLn $ "Run: QED FileToCheck.hs"
    forM_ args $ \arg -> do
        debugEnable $ arg ++ ".log"
        base <- parseCoreImports
        qed <- parseCoreFile arg
        let (check, rest) = partition (isPrefixOf "proof" . fromVar . fst) $ fromModule $ simpler $ qed <> base
        mapM_ (prove (Module rest) . toProp . snd) check

-- | Reduce the language we have to deal with, without changing semantics
simpler :: Module -> Module
simpler = descendBi $ simplifyNoLet . simplifyAlts
    [[(C "Nothing",0),(C "Just",1)]
    ,[(C "[]",0),(C ":",2)]
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
    putStrLn "== QED ==\n"
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


simplifyNoLet :: Exp -> Exp
simplifyNoLet = g . f . simplify
    where
        g (Case o@(Var (V "bottom")) _) = o
        g x = x

        f x | null [x | Let{} <- universe x] = x
            | otherwise = f $ simplify $ transformBi unlet x

        unlet (Let v x y) = subst [(v,x)] y
        unlet x = x

simpP :: Prop -> Prop
simpP = label . short . simp
    where
        -- run the simplifier
        simp (Prop vs x y) = Prop vs (f x) (f y)
            where f x = relabelAvoid (vs ++ free x) $ simplifyNoLet x

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

        f (Case s1 as1) (Case s2 as2)
            | length as1 == length as2
            = (Prop vs s1 s2 :) <$> zipWithM fAlt as1 as2
            where
                fAlt (PWild, x1) (PWild, x2) = Just $ Prop vs x1 x2
                fAlt (PCon c1 v1, x1) (PCon c2 v2, x2)
                    | c1 == c2, length v1 == length v2
                    = Just $ Prop vs (mkLams v1 $ mkAlt s1 (PCon c1 v1) x1)
                                     (mkLams v2 $ mkAlt s2 (PCon c2 v2) x2)
                fAlt _ _ = Nothing

        f (fromApps -> (x1, x2)) (fromApps -> (y1, y2))
            | x1 == y1 && isVarCon x1
            = Just $ zipWith (Prop vs) x2 y2

        f _ _ = Nothing

        isVarCon Var{} = True
        isVarCon Con{} = True
        isVarCon _ = False

mkAlt (Var v) (PCon c vs) = Let v (mkApps (Con c) $ map Var vs)
mkAlt _ _ = id
