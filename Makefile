# Haskell/Cabal Makefile
# Requires GNU Make
# The all target creates a default configuration if need be.

PACKAGE := $(wildcard *.cabal)
CONFIG	= dist/setup-config
SETUP	= runhaskell Setup.hs

all:	$(CONFIG)
	$(SETUP) build

Makefile:
	@echo make $@

$(PACKAGE):
	@echo make $@

$(CONFIG):	$(PACKAGE)
	$(SETUP) configure $(CABALFLAGS) --ghc --user --prefix="${HOME}"

%:	force
	$(SETUP) $@

.PHONY:	all force
