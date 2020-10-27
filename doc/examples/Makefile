CPSAFLAGS = +RTS -N3 -M512m -RTS

SRCS := $(wildcard *.scm) $(wildcard *.lsp)

include ../cpsa.mk

all:    $(SRCS:%.scm=%_shapes.xhtml) $(SRCS:%.scm=%.xhtml) \
	$(SRCS:%.lsp=%_shapes.xhtml) $(SRCS:%.lsp=%.xhtml)

clean:
	-rm *.txt *.xhtml
