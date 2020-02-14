CPSAFLAGS = +RTS -M512m -N -RTS

SRCS := $(wildcard *.scm) $(wildcard *.lsp)

include ../doc/cpsa.mk

all:    $(SRCS:%.scm=%_shapes.xhtml) $(SRCS:%.scm=%.xhtml) \
        $(SRCS:%.lsp=%_shapes.xhtml) $(SRCS:%.lsp=%.xhtml)

clean:
	-rm *.txt *.xhtml
