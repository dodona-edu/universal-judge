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
dependencies: src/generated/description.md

# Generate the json schema of the serialisation format.
src/generated/serialisation.json:
	python ../judge/serialisation.py > src/generated/serialisation.json

# Generate this file.
src/generated/description.md:
	python scripts/mathjax.py src/code/description.md src/generated/description.md

# Do the main file.
src/main.pdf: dependencies
	latexmk -cd -lualatex -shell-escape -use-make ${CONTINUOUS} src/main.tex

watch: CONTINUOUS=-pvc -synctex=1
watch: src/main.pdf
