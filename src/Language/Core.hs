
-- | This module provides a playground for consuming and manipulating programs written
--   in a small core language. In contrast to GHC Core:
--
-- * You can't compile real Haskell programs.
--
-- * It's untyped.
--
-- * It's inefficient in some places.
--
--   On the plus side, it's very easy to get started with. The hope is that if you have a
--   quick idea you can /prototype/ it in the core-playground, refine the idea, and only once
--   you have the basic concepts down might you consider /productionising/ with GHC.
--
--   You can parse modules with 'parseCoreFile', which will produce a 'Module'. After that
--   you can manipulate it with the rest of the functions.
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
import Paths_core_playground


-- | Parse a Haskell-light file, raises an error if @haskell-src-exts@ can't parse the program,
--   of if @core-playground@ can't convert it.
parseCoreFile :: FilePath -> IO Module
parseCoreFile file = evaluate . force . fromModuleHSE . H.fromParseResult =<< H.parseFile file


-- | Parse a library shipped with this package. Examples include @Prelude@, @List@, @Maybe@.
--   Note that the modules shipped with this package are subject to change in the future,
--   if you rely on their specific concents you are advised to make local copies of them.
parseCoreImport :: String -> IO Module
parseCoreImport x = do
    dir <- getDataDir
    parseCoreFile $ dir ++ "/imports/" ++ x ++ ".hs"

-- | Parse all libraries shipped with this package.
--   Note that the modules shipped with this package are subject to change in the future,
--   if you rely on their specific concents you are advised to make local copies of them.
parseCoreImports :: IO Module
parseCoreImports = do
    dir <- getDataDir
    files <- filter (not . isPrefixOf ".") <$> getDirectoryContents (dir ++ "/imports")
    fmap mconcat $ forM files $ \file ->
        parseCoreFile $ dir ++ "/imports/" ++ file
