{-# LANGUAGE ViewPatterns #-}

module Language.Core.Debug(
    debugEnable,
    assertEq,
    debugAssertEq,
    debugTrace
    ) where

import Data.Maybe
import System.IO.Unsafe
import Control.Exception
import Control.DeepSeq
import Control.Concurrent.Extra


{-# NOINLINE debugRef #-}
debugRef :: Var (Maybe FilePath)
debugRef = unsafePerformIO $ newVar Nothing

debugEnable :: FilePath -> IO ()
debugEnable file = modifyVar_ debugRef $ \_ -> do
    writeFile file "= Language.Core.debugEnable log =\n\n"
    return $ Just file


assertEq :: Show e => (e -> e -> Bool) -> (e -> e) -> e -> e
assertEq by f x
    | by x x2 = x2
    | otherwise = error $ "FAILURE: assertEq\nBEFORE: " ++ show x ++ "\nAFTER: " ++ show x2
    where x2 = f x


{-# NOINLINE debugAssertEq #-}
debugAssertEq :: Show e => (e -> e -> Bool) -> (e -> e) -> e -> e
debugAssertEq by f x = unsafePerformIO $ do
    debug <- readVar debugRef
    return $ if isJust debug then assertEq by f x else f x

{-# NOINLINE debugTrace #-}
debugTrace :: Show e => e -> a -> a
debugTrace e x = unsafePerformIO $ withVar debugRef $ \v -> case v of
    Nothing -> return x
    Just file -> do
        s <- evaluate $ force $ show e
        appendFile file $ "\n-- debugTrace --\n" ++ s ++ "\n"
        return x
