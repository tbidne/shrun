#!/bin/sh

find . -name '*.hs' | xargs ormolu --mode=inplace