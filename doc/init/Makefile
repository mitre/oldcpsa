CPSAFLAGS = +RTS -M512m -N1 -RTS
CPSATIME = time

SRCS := $(wildcard *.scm) $(wildcard *.lsp)

include cpsa.mk

all:    $(SRCS:%.scm=%_shapes.xhtml) $(SRCS:%.scm=%.xhtml) \
        $(SRCS:%.lsp=%_shapes.xhtml) $(SRCS:%.lsp=%.xhtml)

clean:
	-rm *.txt *.xhtml
