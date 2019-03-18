# OCaml Makefile that relies on ocamlbuild
#
# Type "make cpsa.d.otarget" to make a debugable executable
#
# Enable warnings by adding to OCFLAGS: -cflags "-w +A-4"

OCFLAGS	= -classic-display -use-ocamlfind -cflags "-w +A-4"
OCB	= ocamlbuild
# Define DEBUG during development
# DEBUG	= d.
TARGET	= cpsa.$(DEBUG)otarget

all:
	$(OCB) $(OCFLAGS) $(TARGET)

clean:
	$(OCB) $(OCFLAGS) -clean

Makefile:
	@echo make $@

%:	force
	$(OCB) $(OCFLAGS) $@

.PHONY:	all clean force
