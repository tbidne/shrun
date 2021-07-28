#!/bin/sh

metrics_string=$(cabal update && cabal haddock --haddock-all)
readarray -t metrics <<<"$metrics_string"

# This is extremely gross (thanks bash). Generally, lines look like:
#   100% ( 2 / 2) in 'Module.Name'
# and we are trying to capture the first number (percentage) and module name.
regex="([0-9]{1,3})%[[:space:]]+\([[:space:]]*[0-9]+[[:space:]]*\/[[:space:]]*[0-9]+[[:space:]]*\)[[:space:]]+in[[:space:]]+'([a-zA-Z\.]+)'"

any_failed=0
ran_test=0
for m in "${metrics[@]}"; do
    if [[ $m =~ $regex ]]; then
      val="${BASH_REMATCH[1]}"
      fn="${BASH_REMATCH[2]}"

      # ShellRun.Prelude will be < 100 because it re-exports
      # prelude functions that are missing documentation.
      threshold=100
      if [[ $fn == "ShellRun.Prelude" ]]; then
        threshold=90
      fi

      if [[ $val -lt $threshold ]]; then
        echo "Haddock missing for $fn: $val"
        any_failed=1
      fi
    fi

    ran_test=1
done

if [ $ran_test == 0 ]; then
    echo "Did not run on any files!"
    exit 1
elif [ $any_failed != 0 ]; then
    echo "Haddock failed!"
    exit 1
else
    echo "Haddock passed!"
    exit 0
fi
