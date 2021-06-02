#!/bin/sh

rm -rf ./doc/*
cabal haddock --haddock-html --haddock-hyperlink-source
cp -r dist-newstyle/build/x86_64-linux/ghc-8.10.4/shell-run-0.1.0.0/doc/html/shell-run/* ./doc