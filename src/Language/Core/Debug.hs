{-# LANGUAGE LambdaCase #-}

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


debugRef :: Var (Maybe FilePath)
{-# NOINLINE debugRef #-}
debugRef = unsafePerformIO $ newVar Nothing

-- | Enable debugging features, in particular 'debugTrace' and 'debugAssertEq'.
--   Pass the name of the file to write the traces to. The file will be overwritten.
debugEnable :: FilePath -> IO ()
debugEnable file = modifyVar_ debugRef $ \_ -> do
    writeFile file "= Language.Core.debugEnable log =\n\n"
    return $ Just file

-- | @assertEq eq f x@ asserts that @f x `eq` x@ before turning @f x@.
--   Typically used with the 'equivalent' function on 'Exp'.
assertEq :: Show e => (e -> e -> Bool) -> (e -> e) -> e -> e
assertEq by f x
    | by x x2 = x2
    | otherwise = error $ "FAILURE: assertEq\nBEFORE: " ++ show x ++ "\nAFTER: " ++ show x2
    where x2 = f x

-- | A version of 'assertEq' that only fires if 'debugEnable' has been called.
debugAssertEq :: Show e => (e -> e -> Bool) -> (e -> e) -> e -> e
{-# NOINLINE debugAssertEq #-}
debugAssertEq by f x = unsafePerformIO $ do
    debug <- readVar debugRef
    return $ if isJust debug then assertEq by f x else f x

-- | If 'debugEnabled' has been called this function writes its value to the
--   specified file - otherwise it simply returns the second argument.
debugTrace :: Show e => e -> a -> a
{-# NOINLINE debugTrace #-}
debugTrace e x = unsafePerformIO $ withVar debugRef $ \case
    Nothing -> return x
    Just file -> do
        s <- evaluate $ force $ show e
        appendFile file $ "\n-- debugTrace --\n" ++ s ++ "\n"
        return x
