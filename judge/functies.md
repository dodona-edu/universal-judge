# Gedachten over ingebedde programmeertaal

In het testplan hebben we op verschillende plaatsen een "universele" taal nodig, om programmeerconcepten uit te drukken, en die vervolgens te vertalen naar allerhande talen.

Functies die de taal moet kunnen (en best geen andere dingen kunnen):

- Toekennen van een resultaat aan een variabele (convert_statement)
- Het resultaat van hierboven kan van drie mogelijkheden komen:
  - Gewone functie-oproep
  - Constructor call
  - Waarde (literal)
  
Om de vertaling naar de doeltalen gemakkelijk te houden, is het belangrijk om het onderscheid tussen verschillende soorten functies in te bouwen, bv. het verschil tussen een constructor en een gewone functie is belangrijk voor Java.

Theoretisch gezien kunnen we stellen dat we het resultaat van een functie-oproep een naam moeten kunnen geven (dus toekennen aan een veranderlijke). Later moet deze veranderlijke kunnen gebruikt worden als parameter in nieuwe functie-oproepen, en in talen die het ondersteunen moeten er functies kunnen opgeroepen worden op deze veranderlijken. Dit kan door:
- Te stellen dat constructors functies zijn die een object terug geven.
- Waarden zijn het resultaat van de identiteistfunctie op die waarde (i.e. id(5) == 5)

We kiezen expliciet NIET voor een bepaalde taal en stellen GEEN eigen programmeertaal op. In het testplan, dat bestaat uit json, stellen we nu een soort beperkte AST op. Dit heeft twee grote voordelen:
- Het laat de keuze van hoe we dit uiteindelijk willen implementeren (welke taal, enz.) vrij; zolang deze taal vertaald kan worden naar het JSON-formaat werkt het.
- De judge is eenvoudiger.
- Het implementeren van nieuwe doeltalen is eenvoudiger.
- We moeten ons enkel bezighouden met de "AST", niet met syntax, parsing, en andere high-level dingen.


Enkele dingen die overwogen zijn:
- Bestaande taal parsen en dan vertalen
- Bestaande AST of IR gebruiken, zoals https://doc.bblf.sh/ of https://nekovm.org/

Moesten we een bestaande taal kiezen, is de voorwaarde als volgt:
- Moet vertaalbaar zijn naar "redelijk" eenvoudige AST
- Liefst te parsen in Python, met iets als https://github.com/erikrose/parsimonious, PEG (parsing expression grammar)
