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

# Clean temporary files.
clean:
	$(RM) src/generated
	latexmk -cd -pdflua -use-make -c src/main.tex

# Dependencies needed for the main file.
copies:
	python scripts/copier.py ../judge/src/tested/languages/c/config.json c-config.json
	python scripts/copier.py ../judge/src/tested/languages/c/config.py c-config.py
	python scripts/copier.py ../judge/src/tested/languages/c/templates/context.c c-context.mako
	python scripts/copier.py ../judge/src/tested/languages/c/templates/selector.c c-selector.mako
	python scripts/copier.py ../judge/src/tested/languages/c/templates/statement.mako c-statement.mako
	python scripts/copier.py ../judge/src/tested/languages/__init__.py languages-init.py
	python scripts/copier.py ../exercise/lotto/solution/correct.py lotto-correct.py
	python scripts/copier.py ../exercise/lotto/solution/correct.java lotto-correct.java

markdown:
	python scripts/mathjax.py ../exercise/lotto/description/description.nl.md ./src/sources/lotto-description.md

generate:
	python scripts/generate_echo.py
	python scripts/generate_echo_function.py

# Do the main file.
src/main.pdf: copies markdown generate
	latexmk -cd -lualatex -shell-escape -use-make ${CONTINUOUS} src/main.tex

watch: CONTINUOUS=-pvc -synctex=1
watch: src/main.pdf
