# Haskell/Cabal Makefile
# Requires GNU Make
# The all target creates a default configuration if need be.

PACKAGE := $(wildcard *.cabal)
CONFIG	= configured

all:	$(CONFIG)
	cabal build

Makefile:
	@echo make $@

$(PACKAGE):
	@echo make $@

$(CONFIG):	$(PACKAGE)
	cabal configure
	touch $(CONFIG)

clean:
	-rm $(CONFIG)
	cabal clean

%:	force
	cabal $@

.PHONY:	all clean force
