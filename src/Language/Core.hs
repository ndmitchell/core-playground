{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, PatternGuards, TupleSections, ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core(
    -- * Core data type
    Module(..),
    Var(..), Con(..), Exp(..), Pat(..),
    -- * Parsing files
    parseCoreFile,
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
import qualified Language.Haskell.Exts as H


parseCoreFile :: FilePath -> IO Module
parseCoreFile file = evaluate . force . fromModuleHSE . H.fromParseResult =<< H.parseFile file
