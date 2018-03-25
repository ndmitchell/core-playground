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
    fromModuleHSE, fromDeclHSE, fromExpHSE,
    toModuleHSE, toDeclHSE, toExpHSE,
    ) where

import Language.Core.HSE
import Language.Core.Type
import Language.Core.Variables
import Language.Core.Simplify
import Language.Core.Operations
import qualified Language.Haskell.Exts as H


parseCoreFile :: FilePath -> IO Module
parseCoreFile file = fromModuleHSE . H.fromParseResult <$> H.parseFile file

fromModuleHSE :: H.Module H.SrcSpanInfo -> Module
fromModuleHSE = fromModule2 . deflate

fromDeclHSE :: H.Decl H.SrcSpanInfo -> [(Var,Exp)]
fromDeclHSE = fromDecl . deflate

fromExpHSE :: H.Exp S -> Exp
fromExpHSE = fromExp . deflate

toModuleHSE :: Module -> H.Module H.SrcSpanInfo
toModuleHSE = inflate . toModule

toDeclHSE :: Var -> Exp -> H.Decl H.SrcSpanInfo
toDeclHSE v x = inflate $ toDecl v x

toExpHSE :: Exp -> H.Exp H.SrcSpanInfo
toExpHSE = inflate . toExp
