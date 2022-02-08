#!/bin/sh

find . -name '*.hs' | xargs ormolu --ghc-opt -XImportQualifiedPost --ghc-opt -XTypeApplications --mode=inplace
