{-# LANGUAGE ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core.Reduce(
    whnfM, whnf, whnfCount, whnfOnce
    ) where

import Control.Monad.Extra
import Control.Monad.Trans.State
import qualified Data.Set as Set
import Language.Core.Type
import Language.Core.Variables
import Language.Core.Simplify
import Language.Core.Operations


whnfM :: Monad m => (Var -> m (Maybe Exp)) -> Exp -> m Exp
whnfM ask = f
    where
        f (Var v) = maybe (return $ Var v) f =<< ask v
        f (App a b) = do
            a <- f a
            case a of
                Lam v x -> do
                    App (Lam v x) b <- return $ relabel $ App (Lam v x) b
                    f $ subst [(v,b)] x
                _ -> return $ App a b
        f (Case a b) = do
            a <- f a
            case a of
                (fromApps -> (Con{}, _)) -> f $ simplify $ Case a b
                Case{} -> f $ simplify $ Case a b
                _ -> return $ Case a b
        f x = return x


whnf :: (Var -> s -> Maybe (s, Exp)) -> Exp -> s -> (Exp, s)
whnf ask = runState . whnfM f
    where f v = do
                res <- gets (ask v)
                whenJust res $ put . fst
                return $ fmap snd res

whnfCount :: Int -> Module -> Exp -> Exp
whnfCount i m e = fst $ whnf f e i
    where f v i = if i <= 0 then Nothing else (,) (i-1) <$> modLookup m v

whnfOnce :: Module -> Exp -> Exp
whnfOnce m e = fst $ whnf f e Set.empty
    where f v seen = if v `Set.member` seen then Nothing else (,) (Set.insert v seen) <$> modLookup m v
