#!/usr/bin/env bash

shrun_vers="0.9"

# strip tab and/or spaces from output
apple_vers=$(sw_vers | grep ProductVersion | cut -d':' -f2 | tr -d ' \t')

arch=$(uname -m)

chmod a+x bin/shrun-macos-latest
mv bin/shrun-macos-latest "bin/shrun_$shrun_vers-$arch-apple_$apple_vers-darwin"
