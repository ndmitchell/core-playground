
-- | Module for defining and manipulating expressions.
module Language.Core.Operations(
    modLookup,
    fromApps, fromLets, fromLams,
    mkApps, mkLets, mkLams
    )  where

import Language.Core.Type


-- | Look up a variable in a 'Module', returning 'Nothing' if the variable
--   is not defined.'
modLookup :: Module -> Var -> Maybe Exp
modLookup m x = lookup x $ fromModule m

-- | Given @f x y z@ return @(f, [x,y,z])@. Inverse of 'mkApps'.
--   If not passed an 'App' the second component will be @[]@.
fromApps :: Exp -> (Exp, [Exp])
fromApps (App x y) = (a, b ++ [y])
    where (a,b) = fromApps x
fromApps x = (x,[])

-- Given @mkApps f [x,y,z]@ return @f x y z@. Inverse of 'fromApps'
mkApps :: Exp -> [Exp] -> Exp
mkApps = foldl App

-- | Given @\x y z -> e@ return @([x,y,z], e)@. Inverse of 'mkLams'.
--   If not passed a 'Lam' the first component will be @[]@.
fromLams :: Exp -> ([Var], Exp)
fromLams (Lam x y) = (x:a, b)
    where (a,b) = fromLams y
fromLams x = ([], x)

-- | Given @mkLams [x,y,z] e@ return @\x y z -> e@. Inverse of 'fromLams'.
mkLams :: [Var] -> Exp -> Exp
mkLams ys x = foldr Lam x ys

-- | Given @let x = 1 in y = 2 in z@ return @([(x,1),(y,2)], z)@. Inverse of 'mkLets'.
--   If not passed a 'Let' the first component will be @[]@.
fromLets :: Exp -> ([(Var, Exp)], Exp)
fromLets (Let x y z) = ((x,y):a, b)
    where (a,b) = fromLets z
fromLets x = ([], x)

-- | Given  @mkLets [(x,1),(y,2)] z@ return @let x = 1 in y = 2 in z@. Inverse of 'fromLets'.
mkLets :: [(Var, Exp)] -> Exp -> Exp
mkLets xs e = foldr (uncurry Let) e xs
