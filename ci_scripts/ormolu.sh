#!/bin/sh

files_string=$(find . -type d \( -path ./dist-newstyle -o -path ./.stack-work \) -prune -false -o -name '*.hs')
readarray -t files <<<"$files_string"

any_failed=0
ran_test=0
for f in "${files[@]}"; do
    ormolu --mode check "$f"
    if [ $? != 0 ]; then
        echo "Ormolu failure: $f"
        any_failed=1
    fi
    ran_test=1
done

if [ $ran_test == 0 ]; then
    echo "Did not run on any files!"
    exit 1
elif [ $any_failed != 0 ]; then
    echo "Ormolu failed!"
    exit 1
else
    echo "Ormolu passed!"
    exit 0
fi
