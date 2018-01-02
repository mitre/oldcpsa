# When the binary changes, run the test suite, and look for changes in
# the output.  The changes are not fatal errors, but for a *.scm input
# file, an error produced by the binary is fatal.

TSTS	:= $(patsubst %.scm,%.txt,$(wildcard *.scm)) \
		$(patsubst %.lsp,%.txt,$(wildcard *.lsp))
CPSA	= ../dist/build/cpsa/cpsa$(EXE)
DIFF	= ../dist/build/cpsadiff/cpsadiff$(EXE)
SHAPES	= ../dist/build/cpsashapes/cpsashapes$(EXE)
SAS	= ../dist/build/cpsasas/cpsasas$(EXE)
ANNOTATIONS = ../dist/build/cpsaannotations/cpsaannotations$(EXE)
GRAPH	= ../dist/build/cpsagraph/cpsagraph$(EXE)
CPSAFLAGS = +RTS -M512m -N8 -RTS

# The expected answers are in files with the .tst extension.

# Analyze protocols for shapes expecting success
%.txt:	%.scm $(CPSA)
	time $(CPSA) $(CPSAFLAGS) -o $@ $*.scm
	-$(DIFF) $(DIFFFLAGS) $*.tst $*.txt

# Analyze protocols for shapes expecting failure
%.txt:	%.lsp $(CPSA)
	-time $(CPSA) $(CPSAFLAGS) -o $@ $*.lsp
	-$(DIFF) $(DIFFFLAGS) $*.tst $*.txt

# Extract shapes
%_shapes.txt:	%.txt
	$(SHAPES) $(SHAPESFLAGS) -o $@ $<

# Extract shape analysis sentences
%_sas.text:	%.txt
	$(SAS) $(SASFLAGS) -o $@ $<

# Annotate shapes
%_annotations.txt:	%_shapes.txt
	$(ANNOTATIONS) $(ANNOTATIONSFLAGS) -o $@ $<

# Visualize output
%.svg:		%.txt
	$(GRAPH) -c -o $@ $<

# Visualize output using the expanded format
%.xhtml:	%.txt
	$(GRAPH) $(GRAPHFLAGS) -o $@ $<

.PRECIOUS:	%.txt %_shapes.txt %_annotations.txt

all:	$(patsubst %.txt,%.xhtml,$(TSTS)) \
		$(patsubst %.txt,%_shapes.xhtml,$(TSTS))

clean:
	-rm *.txt *.xhtml
