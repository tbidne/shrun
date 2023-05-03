.PHONY: build clean repl watch ;\
	doctest ;\
	cic ci formatc format lint lintc ;\
	haddock hackage

# core

T = ""

clean:
	cabal clean

build:
	if [ -z "$(T)" ]; then \
		cabal build; \
	else \
		cabal build $(T); \
	fi

doctest:
	cabal build --write-ghc-environment-files=always; \
	RUN_DOCTEST=1 cabal test doctest; \
	rm .ghc.environment.*

repl:
	if [ -z "$(T)" ]; then \
		cabal repl shrun; \
	else \
		cabal repl $(T); \
	fi

watch:
	if [ -z "$(T)" ]; then \
		ghcid --command "cabal repl shrun"; \
	else \
		ghcid --command "cabal repl $(T)"; \
	fi

watcht:
	find . -type f -name "*hs" | entr -s "cabal test $(T)"

# ci

cic: formatc lintc

ci: lint format

# formatting

EXCLUDE_BUILD := ! -path "./.*" ! -path "./*dist-newstyle/*" ! -path "./*stack-work/*"
FIND_HS := find . -type f -name "*hs" $(EXCLUDE_BUILD)
FIND_CABAL := find . -type f -name "*.cabal" $(EXCLUDE_BUILD)

formatc:
	nixpkgs-fmt ./ --check && \
	$(FIND_CABAL) | xargs cabal-fmt --check && \
	$(FIND_HS) | xargs ormolu --mode check

format:
	nixpkgs-fmt ./ && \
	$(FIND_CABAL) | xargs cabal-fmt --inplace && \
	$(FIND_HS) | xargs ormolu -i

# linting

lint:
	$(FIND_HS) | xargs -I % sh -c " \
		hlint \
		--ignore-glob=dist-newstyle \
		--ignore-glob=stack-work \
		--refactor \
		--with-refactor=refactor \
		--refactor-options=-i \
		%"

lintc:
	hlint . --ignore-glob=dist-newstyle --ignore-glob=stack-work

haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/shrun-0.8.1/opt/doc/html/shrun/* docs/