{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, PatternGuards, TupleSections, ViewPatterns #-}

-- | Module for defining and manipulating expressions.
module Language.Core(
    -- * Core data type
    Module(..),
    Var(..), Con(..), Exp(..), Pat(..),
    -- * Operations
    module Language.Core.Operations,
    -- * Free variable untilities
    module Language.Core.Variables,
    -- * Simplification
    module Language.Core.Simplify,
    -- * Parsing files
    parseCoreFile,
    -- * Conversion to\/from HSE
    toModuleHSE, fromModuleHSE, toExpHSE, fromExpHSE
    ) where

import Language.Core.Type
import Language.Core.Variables
import Language.Core.Simplify
import Language.Core.Operations
import Control.Exception
import Control.DeepSeq
import qualified Language.Haskell.Exts as H


parseCoreFile :: FilePath -> IO Module
parseCoreFile file = evaluate . force . fromModuleHSE . H.fromParseResult =<< H.parseFile file
