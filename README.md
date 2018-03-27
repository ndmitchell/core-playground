# core-playground [![Hackage version](https://img.shields.io/hackage/v/core-playground.svg?label=Hackage)](https://hackage.haskell.org/package/core-playground) [![Stackage version](https://www.stackage.org/package/core-playground/badge/lts?label=Stackage)](https://www.stackage.org/package/core-playground) [![Build Status](https://img.shields.io/travis/ndmitchell/core-playground.svg)](https://travis-ci.org/ndmitchell/core-playground)

This package provides a playground for consuming and manipulating programs written
in a small core language. In contrast to GHC Core:

* You can't compile real Haskell programs.
* It's untyped.
* It's inefficient in some places.

On the plus side, it's very easy to get started with. The hope is that if you have a
quick idea you can _prototype_ it in the core-playground, refine the idea, and only once
you have the basic concepts down might you consider _productionising_ with GHC.
