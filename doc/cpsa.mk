# Makefile rules for CPSA

# Suggested CPSA flags include a memory use limit:
# CPSAFLAGS = +RTS -M512m -RTS

# Analyze protocols for shapes
%.txt:		%.scm
	$(CPSATIME) cpsa3 $(CPSAFLAGS) -o $@ $<

# Analyze protocols for shapes, but don't fail when CPSA does
%.txt:		%.lsp
	-$(CPSATIME) cpsa3 $(CPSAFLAGS) -o $@ $<

# Extract shapes
%_shapes.txt:	%.txt
	cpsa3shapes $(SHAPESFLAGS) -o $@ $<

# Extract shape analysis sentences
%_sas.text:	%.txt
	cpsa3sas $(SASFLAGS) -o $@ $<

# Annotate shapes
%_annotations.txt:	%_shapes.txt
	cpsa3annotations $(ANNOTATIONSFLAGS) -o $@ $<

# Compute protocol parameters
%_parameters.txt:	%_shapes.txt
	cpsa3parameters $(PARAMETERSFLAGS) -o $@ $<

# Visualize output using the expanded format (default)
%.xhtml:	%.txt
	cpsa3graph $(GRAPHFLAGS) -o $@ $<

# Visualize output using the compact format
%.svg:		%.txt
	cpsa3graph -c -o $@ $<

# Visualize output using the LaTeX format
%.tex:		%.txt
	cpsa3graph -l -m 62 -o $@ $<

.PRECIOUS:	%.txt %_shapes.txt %_annotations.txt
