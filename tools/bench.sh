set -e

export LANG="C.UTF-8"

cabal bench --benchmark-options \
  '+RTS -T -RTS -t100 --csv benchmarks/bench.csv --svg benchmarks/bench.svg'
