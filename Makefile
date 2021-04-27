# Haskell/Cabal Makefile
# Requires GNU Make

TOOL =	cabal

all:
	$(TOOL) build

Makefile:
	@echo make $@

%:	force
	$(TOOL) $@

.PHONY:	all force
