# Thesis

De thesis is geschreven in LaTeX. Vereisten:

- Een normale LaTeX-installatie met LuaLaTeX.
- Lettertype [Libertinus](https://github.com/alif-type/libertinus), het hoofdlettertype van de thesis.
- Lettertype [Jetbrains Mono](https://www.jetbrains.com/lp/mono/), het lettertype voor code.
- Package [ugent2016](https://github.com/niknetniko/ugent2016), voor de huisstijl.
- Python en pygmentize voor syntaxiskleuring.

## Structuur

In de thesis wordt regelmatig code uit TESTed getoond.
Om ervoor te zorgen dat de thesis blijft werken, wordt bij het uitvoeren van make een kopie gemaakt van alle nodige code.
Dit om te vermijden dat de verkeerde code getoond wordt als TESTed verandert, want sommige code wordt getoond op regelnummer.

Bij het bijwerken van deze code (in de map `/src/sources`) zal om bevestiging gevraagd worden indien het bestand veranderd is (een waarschuwing of een bevestiging als het aantal regels verandert).

De code voor de tekst staat in `/src`.

## Compilatie

Gebruik `make`.
