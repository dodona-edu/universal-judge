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
	$(RM) src/sources
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
	python scripts/copier.py ../exercise/lotto/evaluation/evaluator.py lotto-evaluator.py
	python scripts/copier.py ../exercise/lotto/evaluation/plan.tson lotto-plan.tson
	python scripts/copier.py ../exercise/division/evaluation/Evaluator.java division-evaluator.java
	python scripts/copier.py ../exercise/division/evaluation/plan.json division-plan.tson
	python scripts/copier.py ../exercise/sum/evaluation/short.tson sum-plan.tson

markdown:
	python scripts/mathjax.py ../exercise/lotto/description/description.nl.md ./src/sources/lotto-description.md

generate:
	python scripts/generate_echo_c.py
	python scripts/generate_echo_python.py
	python scripts/generate_echo_java.py
	python scripts/generate_echo_function_c.py
	python scripts/generate_echo_function_python.py
	python scripts/generate_echo_function_java.py

# Do the main file.
src/main.pdf: copies markdown generate
	latexmk -cd -lualatex -shell-escape -use-make ${CONTINUOUS} src/main.tex

watch: CONTINUOUS=-pvc -synctex=1
watch: src/main.pdf
