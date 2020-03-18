# Makefile for the LaTeX version of the thesis.
# Most of this Makefile is used to prepare supporting documents.
# The actual compilation is fairly simple with latexmk or manually compiling.

# Define the command for removing things, depending on the platform.
ifeq ($(OS),Windows_NT)
RM = powershell -noprofile Remove-Item -Recurse -Force -ErrorAction Ignore
else
RM = rm -rf
endif

# Create the main pdf.
all: src/main.pdf

# Clean temporar files.
clean:
	$(RM) src/generated
	latexmk -cd -pdflua -use-make -c src/main.tex

# Generate dependencies needed for the main file.
dependencies:

# Generated dependencies
src/generated/description.md:
	python scripts/mathjax.py ../exercise/lotto/description/description.nl.md $@

src/generated/isbn_description.md:
	python scripts/mathjax.py ../exercise/isbn/description/description_clean.nl.md $@

# Do the main file.
src/main.pdf: dependencies
	latexmk -cd -lualatex -shell-escape -use-make ${CONTINUOUS} src/main.tex

watch: CONTINUOUS=-pvc -synctex=1
watch: src/main.pdf
