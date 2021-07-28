#!/bin/sh

find . -name '*.hs' | xargs ormolu --ghc-opt -XImportQualifiedPost --mode=inplace
