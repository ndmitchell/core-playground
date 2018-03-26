
-- | Module for defining and manipulating expressions.
module Language.Core(
    -- * Core data type
    Module(..),
    Var(..), Con(..), Exp(..), Pat(..),
    -- * Parsing files
    parseCoreFile,
    parseCoreImport,
    parseCoreImports,
    -- * Operations
    module Language.Core.Operations,
    -- * Free variable untilities
    module Language.Core.Variables,
    -- * Simplification and reduction
    module Language.Core.Simplify,
    module Language.Core.Reduce,
    -- * Equivalence
    module Language.Core.Equivalent,
    -- * Debugging
    module Language.Core.Debug,
    -- * Conversion to\/from HSE
    toModuleHSE, fromModuleHSE, toExpHSE, fromExpHSE
    ) where

import Language.Core.Type
import Language.Core.Variables
import Language.Core.Simplify
import Language.Core.Operations
import Language.Core.Reduce
import Language.Core.Equivalent
import Language.Core.Debug
import Control.Exception
import Control.DeepSeq
import Control.Monad
import qualified Language.Haskell.Exts as H
import Data.List
import System.Directory
import Paths_simple_core


parseCoreFile :: FilePath -> IO Module
parseCoreFile file = evaluate . force . fromModuleHSE . H.fromParseResult =<< H.parseFile file


parseCoreImport :: String -> IO Module
parseCoreImport x = do
    dir <- getDataDir
    parseCoreFile $ dir ++ "/imports/" ++ x ++ ".hs"

parseCoreImports :: IO Module
parseCoreImports = do
    dir <- getDataDir
    files <- filter (not . isPrefixOf ".") <$> getDirectoryContents ("imports/" ++ dir)
    fmap mconcat $ forM files $ \file ->
        parseCoreFile $ dir ++ "/imports/" ++ file
