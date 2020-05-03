
Voor we beginnen aan de configuratie, overlopen we kort welke functionaliteit we langs beide kanten willen ondersteunen: welke functionaliteit uit C kunnen we aanbieden in TESTed en welke functionaliteit uit TESTed kunnen we implementeren in C?
Uiteraard willen we zoveel mogelijk ondersteunen, maar vooral op het vlak van gegevenstypes zijn er momenteel beperkingen.

Welke basistypes gaan we niet ondersteunen?

- `sequence` - Arrays zijn een speciaal geval in C: statische arrays kunnen bijvoorbeeld niet als returnwaarde dienen, en ook als functieargument zijn ze niet ideaal. Dynamische arrays nemen de vorm aan van een pointer en een grootte. TESTed heeft momenteel geen ondersteuning voor datatypes die als twee waarden geïmplementeerd moeten worden, dus worden arrays momenteel niet ondersteund.
- `set` - C heeft geen ingebouwde verzamelingen.
- `map` - C heeft geen ingebouwde map of dict. Er zijn wel structs, maar daarvan is het niet mogelijk om de velden at runtime op te vragen, waardoor we ze niet kunnen serialiseren.

Welke geavanceerde types gaan we niet ondersteunen?

- `big_int` - C heeft geen ingebouwd type voor getallen van arbitraire grootte.
- `fixex_precision` - C heeft geen ingebouwd type voor kommagetallen me willekeurige precisie.
- Andere datastructuren, zoals `array` en `list` (om dezelfde redenen als hierboven). Ook `tuple` wordt niet ondersteund, omdat het niet bestaat in C.

# Locatie van de code

De eerste stap in het configureren van een programmeertaal is het aanmaken van een map waarin we de code voor de programmeertaal zullen zetten. Deze map moet de naam van de programmeertaal krijgen en op de juiste plaats binnen TESTed aanwezig zijn. Maak een nieuwe map `judge/src/tested/languages/c`. Na het aanmaken van de map moet de mappenstructuur er zo uitzien:

```text
universal-judge
├── judge/
│   ├── src/
│   │   └── tested/
│   │       ├── languages/
│   │       │   ├── c/          <- nieuwe map
│   │       │   ├── haskell/
│   │       │   ├── java/
│   │       │   ├── python/
│   │       │   ├── config.py
│   │       │   ...
│   │       ...
│   ...
...
```


# Configuratiebestand

Het configuratiebestand is een json-bestand met enkele eigenschappen van de programmeertaal. Dit configuratiebestand maakt het implementeren van de configuratieklasse een stuk eenvoudiger, omdat de implementatie van die klasse daardoor veel minder lang zal zijn. Maak eerst het configuratiebestand aan: `judge/src/tested/languages/c/config.json`.

Merk op dat het configuratiebestand slechts een hulpmiddel is: indien gewenst kunnen al deze opties ook ingesteld worden door de juiste methodes te implementeren in de configuratieklasse, maar we verwachten dat dit in veel gevallen niet nodig zal zijn.

## Algemene opties

- `general.dependencies` - Dit zijn bestanden die beschikbaar zullen zijn tijdens het compileren en tijdens het uitvoeren van de beoordeling. Dit betekent dat deze dependencies gebruikt kunnen worden in de testcode voor de contexten en de evaluatiecode voor de geprogrammeerde en programmeertaalspecifieke code. In het geval van C is dit de `values`-module, waarvan we de implementatie later bespreken. Het kan gebeuren dat de code van de dependencies ook beschikbaar is voor de ingediende oplossing. Dit is echter toeval en niet de bedoeling. Er is momenteel geen ondersteuning om dependencies beschikbaar te maken voor de ingediende oplossing.
- `general.selector` - Dit geeft aan of de programmeertaal gebruikmaakt van een selector tijdens het uitvoeren van code die gecompileerd is in batchcompilatie. Voor de meeste talen met compilatie zal dit `true` zijn, zoals ook bij C.
TODO: referentie!
- `extensions.file` - Geeft de voornaamste bestandsextensie aan van de bestanden. Met voornaamste bedoelen we de extensie van de bestanden die gegenereerd worden. Bijvoorbeeld bij C bestaan zowel `.h` en `.c`, maar de gegenereerde code gebruikt `.c`.
- `extensions.templates` - wordt gebruikt om aan te geven welke extensies gebruikt worden voor de sjablonen. Standaard is dit de bestandsextensie van hierboven en `.mako`. Het is vaak niet nodig om dit op te geven.

```json
{
  "general": {
    "dependencies": [
      "values.h",
      "values.c"
    ],
    "selector": true
  },
  "extensions": {
    "file": "c",
    "templates": ["c", "mako"]
  }
}
```

## Codestijl

Programmeertaalelementen zoals functies en namespaces worden omgezet in functie van de codestijl die gebruikelijk is in de programmeertaal:

```json
{
  "naming_conventions": {
    "namespace": "snake_case",
    "function": "snake_case"
  }
}
```

De mogelijke waarden zijn:

- `snake_case` - Tussen elk woord staat een underscore: `dit_is_een_voorbeeld`.
- `came_case` - Elk woord, buiten het eerste, start met een hoofdletter: `ditIsEenVoorbeeld`. Deze variant wordt ook wel _lowerCamelCase_ genoemd.
- `pascal_case` - Elk woord, ook het eerste, start met een hoofdletter: `DitIsEenVoorbeeld`. Deze variant wordt ook wel _UpperCamelCase_ genoemd.

Standaard wordt `snake_case` gebruikt, dus bij C is dit niet strikt nodig om het erbij te zetten.

## Functionaliteit

De laatste twee blokken in de configuratie geven aan welke constructies en gegevenstypes de programmeertaal ondersteunt. We hebben reeds besproken welke functionaliteit we willen ondersteunen en welke niet (TODO: reference!). We beginnen met de taalconstructies vast te leggen:

```json
{
  "constructs": {
    "objects": false,
    "exceptions": false,
    "function_calls": true,
    "assignments": true,
    "heterogeneous_collections": false,
    "heterogeneous_arguments": false,
    "evaluation": false
  }
}
```

Hier kan voor elke taalconstructie opgegeven worden of ze ondersteund wordt of niet (met een `boolean`). Standaard wordt geen enkele taalconstructie ondersteund: dit zorgt ervoor dat alle ondersteunde constructies expliciet in het configuratiebestand staan en dat nieuwe taalconstructies toegevoegd kunnen worden zonder dat bestaande configuraties van programmeertalen aangepast moeten worden.

De mogelijke taalconstructie zijn deze uit de enum `tested.features.Construct`. Hieronder volgt een lijst van elke taalconstructie en een korte beschrijving:

`objects`
: Objectgeoriënteerde zaken zoals klassen.

`exceptions`
: Exceptions en uitzonderingen.

`function_calls`
: Functieoproepen. Merk op dat constructors in het testplan een speciale soort functie zijn, maar deze hangen af van de taalconstructie `objects`.

`assignments`
: Het toekennen van een waarde aan een variabele. Een "assignment" moet ruim geïnterpreteerd worden als ondersteuning voor iets dat neerkomt op een assigment. Zo kent Haskell bijvoorbeeld geen assignments: `x = 5` definieert technisch gezien een functie met een constante returnwaarde `5`. Dit moet ook onder `assignments` gerekend worden.

`heterogeneous_collections`
: Hiermee bedoelen we verzamelingen met elementen met verschillende gegevenstypes. Dit is bijvoorbeeld geen probleem in Python (`[5, 52.23]`), gaat al iets moeilijker in Java (`List<Object> = List.of(1, 52.23)`), maar zal niet lukken in Haskell.

`heterogeneous_arguments`
: Hiermee bedoelen we functieoproepen waarbij dezelfde functie meerdere keren wordt opgeroepen met argumenten met verschillende datatypes (bijvoorbeeld eerst `check(True)` daarna `check('hallo')`). Dit zal lukken in Python en Java, maar niet in Haskell en C.

`evaluation`
: Of een geprogrammeerde evaluatie mogelijk is in deze programmeertaal. Dit is technisch gezien geen taalconstructie.

Dan moeten we nu de ondersteuning voor de gegevenstypes vastleggen:

```json
{
  "datatypes": {
    "integer": "supported",
    "rational": "supported",
    "char": "supported",
    "text": "supported",
    "boolean": "supported",
    "sequence": "unsupported",
    "set": "unsupported",
    "map": "unsupported",
    "nothing": "supported",

    "int8": "supported",
    "uint8": "supported",
    "int16": "supported",
    "uint16": "supported",
    "int32": "supported",
    "uint32": "supported",
    "int64": "supported",
    "uint64": "supported",
    "bigint": "reduced",
    "single_precision": "supported",
    "double_precision": "supported",
    "double_extended": "unsupported",
    "fixed_precision": "unsupported",
    "array": "unsupported",
    "list": "unsupported",
    "tuple": "unsupported"
  }
}
```

Zoals uitgelegd in (TODO: referentie!) zijn er twee soorten gegevenstypes in TESTed: de basistypes en de geavanceerde types. De basistypes zijn abstracte types voor concepten (zoals een sequentie of een geheel getal), terwijl de geavanceerde types concreter zijn (zoals een geheel getal van 8 bits).
Een gegevenstype kan drie niveaus van ondersteuning hebben:

- `supported` - volledige ondersteuning
- `reduced` - wordt ondersteund, maar wordt herleid tot een basistype (bijvoorbeeld een `list` wordt geïnterpreteerd als een `sequence`)
- `unsupported` - geen ondersteuning, dit is de standaardwaarde

Een opmerking hierbij is dat de status `reduced` voor de basistypes equivalent is aan `supported`: een basistype reduceren tot een basistype blijft hetzelfde type.

Het is de bedoeling dat de meeste programmeertalen voor het merendeel van de datatypes ten minste `reduced` hebben. Toch is gekozen om `unsupported` als standaardwaarde te nemen; dit zorgt ervoor dat de ondersteunde datatypes explicit uitgeschreven zijn. Ook laat dit opnieuw toe om datatypes toe te voegen aan TESTed zonder bestaande configuraties van programmeertalen te moeten aanpassen.
Ter illustratie vermelden we hier voor C alle datatypes, ook de niet-ondersteunde.

# Configuratieklasse

De configuratieklasse is de schakel tussen de generieke aspecten van TESTed en het programmeertaalafhankelijke gedrag.
Omdat TESTed in Python geschreven is, moet deze klasse ook in Python geïmplementeerd worden.

Maak een nieuw Python-bestand in `judge/src/tested/langauges/c/config.py`.
Hierin moet een klasse komen die `Language` uitbreidt:

```python
from tested.languages import Language

class CConfig(Language):
    pass
```

In de rest van deze paragraaf overlopen we de verschillende methodes die geïmplementeerd moeten worden in deze klasse.
In de superklasse, `Language`, zijn de abstracte methodes voorzien van uitgebreide documentatie.

## Compileren van de code

Een eerste en belangrijke methode is de callback voor de compilatiestap:

```python
def compilation(self, files: List[str]) -> CallbackResult:
    main_file = files[-1]
    exec_file = Path(main_file).stem
    result = executable_name(exec_file)
    return ["gcc", "-std=c11", "-Wall", "evaluation_result.c", "values.c", main_file, "-o", result], [result]
```

Doordat het compileren een samenspel is van gegenereerde code uit de sjablonen en de dependencies, moet deze methode met zorg geïmplementeerd worden. Hieronder volgt een (licht gewijzigde) versie van de documentatie van deze methode.

Als argument krijgt deze methode een lijst van bestanden mee waarvan TESTed vermoed dat ze nuttig kunnen zijn voor de compilatiestap. Het bevat onder andere de dependencies uit het configuratiebestand, de ingediende oplossing en de uit de sjablonen gegenereerde bestanden. Die laatste bestanden zijn bijvoorbeeld de verschillende contexten bij een batchcompilatie, maar kunnen ook de evaluator zijn bij een geprogrammeerde evaluatie. De bestanden bestaan uit de naam en een bestandsextensie.

De conventie is om het bestand met de main-functie als laatste te plaatsen.

Al deze bestanden zullen zich in de map bevinden waarin de compilatie zal plaatsvinden.
Het is niet verplicht al deze bestanden ook effectief te gebruiken: sommige programmeertalen hebben zelf een detectiesysteem voor bestanden.
Zo is het in C voldoende om enkel het laatste bestand met de main-functie te gebruiken: alle andere bestanden worden gevonden door `gcc`.

Concreet ziet deze parameter er bijvoorbeeld als volgt uit:

```python
["values.py", "evaluation_utils.py", "context_0_0.py"]
```

Als returnwaarde moet deze methode een tuple met twee element teruggeven: het compilatiecommando en een lijst van resulterende bestanden of een filter.

Het compilatiecommando neemt de vorm aan van een lijst van de elementen waaruit het commando bestaat. Bij het uitvoeren van dit commando zal deze lijst aan de Python-module `subprocess` gegeven worden.

Na het uitvoeren van het compilatiecommando moet TESTed weten welke bestanden relevant zijn om mee te nemen naar een volgende stap in de beoordeling. Daarom moet een lijst van resulterende bestanden teruggegeven worden. Enkel bestanden in deze lijst zullen bijvoorbeeld beschikbaar zijn bij het uitvoeren van de contexten.
Een lijst van bestanden teruggeven is mogelijk indien op voorhand geweten is in welke bestanden de compilatie resulteert. Dit is bijvoorbeeld hier het geval (in C resulteert de compilatie in één uitvoerbaar bestand), of ook bij Python, waar de compilatie voor elk `.py`-bestand resulteert in een `.pyc`-bestand. Ook hier moet de conventie dat het bestand met de main-functie als laatste komt gerespecteerd worden.

Het is op voorhand echter niet altijd mogelijk om te weten in welke bestanden de code zal resulteren. Zo resulteert compilatie van één `.java`-bestand mogelijk in meerdere `.class`-bestanden, afhankelijk van de inhoud van de bestanden.
Om dit op te lossen kan in plaats van een lijst ook een filterfunctie teruggegeven worden.

TESTed zal deze filter toepassen nadat de compilatie uitgevoerd is op elk bestand in de map waarin de compilatie uitgevoerd is. De filterfunctie krijgt als argument de naam van een bestand en moet `true` of `False` teruggeven als het bestand respectievelijk wel of niet meegenomen moet worden naar een volgende stap.

Een voorbeeld van de in- en uitvoer van de compilatiemethode:

```pycon
>>> compilation(["submission.c", "context_0_0.c", "selector.c"])
(
    ["gcc", "-std=c11", "-Wall", "values.c", "selector.c", "-o", "selector.exe"],
    ["selector.exe"]
)
```

Als een leeg compilatiecommando wordt teruggegeven, dan wordt er geen compilatie gedaan. Dit is ook de standaardimplementatie van deze methode. Voor programmeertalen waar geen compilatie nodig is, moet deze methode niet geïmplementeerd worden.

## Uitvoeren van de testcode

Na het compileren moeten we een methode implementeren om de gecompileerde code uit te voeren:

```python
def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
    local_file = cwd / executable_name(Path(file).stem)
    return [str(local_file.absolute()), *arguments]
```

Deze functie heeft drie parameters:

- `cwd`: de map waarin het uitvoeren plaatsvindt
- `file`: het uitvoerbaar bestand dat moet uitgevoerd worden
- `arguments`: argumenten die aan het proces moeten meegegeven worden

Als resultaat moet het commando teruggegeven worden, dat ook aan `subprocess` doorgegeven wordt.

In het geval van C is dit commando eenvoudig: we geven het absolute pad naar het uitvoerbare bestand mee en geven ook de argumenten mee. Het absolute pad is nodig omdat de executable die we willen uitvoeren (en gemaakt hebben in de compilatiestap) niet in de `PATH` zit.

Een voorbeeld van deze functie in werking is:

```pycon
>>> execution("/test/path", "executable.exe", ["arg1", "arg2"])
["/test/path/executable.exe", "arg1", "arg2"]
```

De basisimplementatie van de configuratie is nu klaar. Voor de meeste programmeertalen kan nu overgegaan worden naar de sjablonen, maar in C moeten we nog een kleine methode implementeren.

## Aanpassen van de oplossingscode

In TESTed kunnen er meerdere `main`-functies zijn:

- De oplossing van de student kan een `main`-functie hebben.
- Zowel de contexten als de selector kunnen `main`-functies hebben.

C staat slechts één `main`-functie toe.

Een ander probleem is dat de selector elke context insluit (zoals we later zullen zien bij de sjablonen), en elke context ook de oplossing insluit.

Om deze redenen moeten we de code van de ingediende oplossing een beetje aanpassen:

- We voegen aan `guard` toe, zodat de oplossing slechts eenmaal geladen wordt.
- We hernoemen de `main`-functie naar `solution_main` indien die bestaat.

```python
def solution(self, solution: Path, bundle: Bundle):
    with open(solution, 'r') as file:
        contents = file.read()
    with open(solution, 'w') as file:
        header = '#pragma once\n\n'
        result = header + contents.replace('main', 'solution_main')
        file.write(result)
```

# Sjablonen

De derde stap is het schrijven van de sjablonen.
We hebben uiteraard de verplichte sjablonen nodig (zie TODO REFERENTIE voor een beschrijving van welke sjablonen verplicht zijn en welke niet), maar om code te hergebruiken kiezen we ervoor om enkele bijkomende sjablonen te schrijven:

- `context.c` - het sjabloon om de contextcode te genereren
- `selector.c` - het sjabloon om de selector voor batchcompilatie
- `declaration.mako` - vertaal het type van een variabele naar code
- `function.mako`-  vertaalt een functieoproep
- `statement.mako` - vertaalt een statement of expressie naar code
- `value.mako` - vertaalt een waarde naar code
- `value_arguments.mako` - hulpsjabloon voor `value.mako`
- `value_basic.mako`- hulpsjabloon voor `value.mako`

Al deze sjablonen komen in de map `judge/src/tested/languages/c/templates`.

Het is vrij om de bestandsextensie van de sjablonen te kiezen, zolang het een extensie is uit de configuratie. Standaard zijn de toegelaten extensies `.mako` en een programmeertaalafhankelijke extensie, hier `.c`.
Een conventie die gebruikt wordt binnen TESTed, is de volgende:

- Sjablonen eindigen op de programmeertaalafhankelijke extensie (`.c`) indien het sjabloon resulteert in een opzichzelfstaand bestand.
  Voorbeeld zijn het contextsjabloon en de selector.
- Sjablonen die resulteren in een codefragment en dus vooral gebruikt worden als onderdeel van andere sjablonen eindigen op `.mako`.
  Dit zijn bijvoorbeeld de sjablonen om functies en statements om te zetten.
  
Dit wordt niet afgedwongen door TESTed; alle sjablonen hadden de extensie `.c` of `.mako` kunnen krijgen, of een mengeling.

## Het contextsjabloon

Dit is veruit het grootste en het meest ingewikkelde sjabloon. Het is verantwoordelijk voor het genereren van de testcode voor één context.

We importeren de values-module (hierover later meer) en de ingediende oplossing.
De variabele `submission_name` zal de naam van het oplossingsbestand bevatten.
Een overzicht van alle beschikbare variabelen in het contextsjabloon is te vinden in de klasse `tested.languages.generator._ContextArguments`.

We importeren ook alle programmeertaalspecifieke evaluatoren die we nodig zullen hebben. De variabele `evaluator_names` bevat een verzameling van deze namen.

```mako
#include <stdio.h>

#include "values.h"
#include "${submission_name}.c"

% for name in evaluator_names:
    #include "${name}.c"
% endfor
```

Vervolgens maken we twee variabelen aan waarin de bestanden komen die dienst doen als return- en exception-channel.
We noemen deze bestanden de uitvoerbestanden.
Merk op dat C geen exceptions ondersteunt, maar TESTed verwacht toch een bestand voor het exception-channel.
Anders zal TESTed ervan uitgaan dat er iets verkeerd liep tijdens het uitvoeren.
We definiëren ook direct een functie om de separator naar alle uitvoerkanalen te schrijven.

In onderstaande codefragment, en in de rest van het contextsjabloon, wordt regelmatig de naam van de context als prefix gebruikt voor functies en variabelen.
Dit is omdat het in C niet mogelijk is om in meerdere bestanden functies met dezelfde naam te hebben.
Als we dus meerdere contexten samen compileren en elke context heeft zijn eigen `write_separator`-functie, dan zou het compileren mislukken.

```mako
static FILE* ${context_name}_value_file = NULL;
static FILE* ${context_name}_exception_file = NULL;

static void ${context_name}_write_separator() {
    fprintf(value_file, "--${secret_id}-- SEP");
    fprintf(exception_file, "--${secret_id}-- SEP");
    fprintf(stdout, "--${secret_id}-- SEP");
    fprintf(stderr, "--${secret_id}-- SEP");
}
```

Als een resultaat geproduceerd wordt voor de return- of exception-channel, dan moet dat resultaat geserialiseerd worden en naar de uitvoerbestanden geschreven worden.
TESTed verwacht dat volgende functies beschikbaar zijn:

- `send_value(value)` - schrijf een waarde naar een bestand.
- `send_exception(exception)` - schrijf een exception naar een bestand.
- `send_specific_value(value)` - schrijf het resultaat van een programmeertaalspecifieke evaluatie naar de return-channel.
- `send_specific_exception(exception)` - schrijf het resultaat van een programmeertaalspecifieke evaluatie naar de exception-channel.

Concreet in het geval van C zijn de exception-functies niet nodig, daar C geen exceptions ondersteunt. Ook gebruiken we een marco in plaats van een functie: dit opnieuw omdat we niet dezelfde functie in meerdere bestanden kunnen definiëren.

```mako
#undef send_value
#define send_value(value) write_value(${context_name}_value_file, value)

#undef send_specific_value
#define send_specific_value(r) send_evaluated(${context_name}_value_file, r)
```

We zien ook dat de implementatie eenvoudig is: we geven de gekregen waarde of exception door aan de juiste functie uit de `values`-module, en geven ook het bestand mee waarin de waarde of exception moet komen.

De lezer zal zich misschien afvragen waarom het nodig is om deze functies te gebruiken: als TESTed een functieoproep naar deze functies kan definiëren, waarom kan TESTed dan niet direct de `values`-module gebruiken, zonder daar deze functies tussen te plaatsen?

Het antwoord is dat de `values`-module niet verplicht is. Dit is een conventie die in alle ondersteunde programmeertalen gebruikt wordt, maar het is evengoed mogelijk om bij de implementatie van bijvoorbeeld `send_value` de waarde rechtstreeks naar het bestand te schrijven.
Deze functies moeten beschouwd worden als de "interface" tussen TESTed en de programmeertaal: TESTed verwacht dat deze functies bestaan en de waarde of exception naar het juiste bestand schrijven, maar hoe dat gebeurt maakt voor TESTed niet uit.

Nu zijn we aangekomen bij het uitvoeren van de testgevallen zelf. In C gebeurt dit in een functie die de naam van de context krijgt. Als eerste stap maken we de bestanden voor de return- en exception-channel aan.

```mako
int ${context_name}() {

    ${context_name}_value_file = fopen("${value_file}", "w");
    ${context_name}_exception_file = fopen("${exception_file}", "w");
```

Vervolgens printen we de `before`-code.
De `before`-code is een fragment code dat uitgevoerd wordt voor het uitvoeren van de context.
Deze kan opgegeven worden in het testplan.

Verder schrijven de _separator_ naar de uitvoerbestanden door gebruik te maken van de functie die we eerder gedefinieerd hebben in ons sjabloon.
Zoals we reeds bespraken, komt de uitvoer van de return- en exception-channel in bestanden terecht. Het is nodig om de waarden van elkaar te kunnen onderscheiden, om goed te weten waar de resultaten van een testgeval stoppen en waar de resultaten van het volgende testgeval beginnen.
Hiervoor gebruiken we de _separator_.

Het is belangrijk om de separator altijd vóór de aanvang van een testgeval naar de uitvoerbestanden te schrijven. TESTed is daar zo op voorzien: de separator na het testgeval uitschrijven zal tot verkeerde resultaten leiden.

Ook roepen we de `main`-functie van de oplossing op indien het testplan dat vereist. Oefeningen waar geen `main`-functie opgeroepen wordt zijn bijvoorbeeld deze waarbij de student een functie moet implementeren.

TODO: arguments in de main-functie

```mako
    ${before}

    ${context_name}_write_separator();

    % if context_testcase.exists:
        solution_main();
    % endif
```

Vervolgens genereren we de code voor alle normale testgevallen.
Omdat C geen exceptions ondersteunt, is deze implementatie eenvoudig: we schrijven de separator naar de uitvoerbestanden en voeren het invoerstatement uit.

```mako
    % for testcase in testcases:
        ${context_name}_write_separator();
        <%include file="statement.mako" args="statement=testcase.input_statement()" />;
    % endfor
```


Dat invoerstatement is `testcase.input_statement()`, wat een geserialiseerd statement zal teruggeven.
Wat dat statement juist is, is eigenlijk niet relevant voor het sjabloon, maar het kan toch geen kwaad om het te weten:

- Als de invoer van het testgeval een assignment is, zal dit resulteren in code die er zo uitziet:
    ```c
    int variable = functieoproep();
    ```
- Is de invoer een uitdrukking (_expression_) en zijn we geïnteresseerd in de returnwaarde (het is dus niet van het type `void`), dan zal de gegenereerde code er als volgt uitzien:
    ```c
    send_value(functieoproep());
    ```

Als afsluiter zetten we de `after`-code en sluiten we de bestanden.
De `after`-code is analoog aan de `before`-code.

```mako
    ${after}

    fclose(${context_name}_value_file);
    fclose(${context_name}_exception_file);
    return 0;
}
```

Omdat TESTed zowel contextcompilatie als batchcompilatie ondersteunt, moet elke context een `main`-functie hebben. C laat slechts 1 `main`-functie toe. Indien we in batchcompilatie zitten, zal de selector gebruikt worden, en zal `INCLUDED` op `TRUE` staan. In dat geval voegen we geen `main`-functie toe.

```mako
#ifndef INCLUDED
int main() {
    return ${context_name}();
}
#endif
```

## Het selectorsjabloon

Het is nuttig om er meteen het selectorsjabloon bij te halen: dit wordt gebruikt als TESTed in batchcompilatie werkt en is verantwoordelijk om de juiste context uit te voeren op basis van een argument.
Het is in dit sjabloon dat de macro `INCLUDED` op `true` gezet wordt, waardoor de `main`-functies in andere contexten niet gebruikt worden.

```mako
#define INCLUDED true

% for c in contexts:
    #include "${c}.c"
% endfor

int main(int argc, const char* argv[]) {

    const char* name = argv[1];

    if (name == NULL) {
        fprintf(stderr, "You must select a context");
        return -2;
    }

    % for c in contexts:
        if (strcmp("${c}", name) == 0) {
            return ${c}();
        }
    % endfor
    return -1;
}
```

## Statementsjabloon

Dit sjabloon wordt door TESTed gebruikt om statements te vertalen naar code.
Dit omvat assignments, functieoproepen, waarden, enzovoort:

```mako
## Convert a statement and/or expression into C code.
<%! from tested.utils import get_args %>
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment %>

<%page args="statement,full=False"/>
% if isinstance(statement, Identifier):
    ## If the expression is an identifier, just print it.
    ${statement}\
% elif isinstance(statement, FunctionCall):
    ## Delegate to the function template for function calls.
    <%include file="function.mako" args="function=statement"/>
% elif isinstance(statement, get_args(Value)):
    ## Delegate to the value template if we have a value.
    <%include file="value.mako", args="value=statement" />
% else:
    <% assert isinstance(statement, get_args(Assignment)) %>
    % if full:
        ## If full is true, we want to include the type of the variable.
        ##      int name = expression;
        ##      ---------- <- This is the declaration part.  
        <%include file="declaration.mako" args="value=statement.expression" /> \
    % endif
    ${statement.name} = <%include file="statement.mako" args="statement=statement.expression"/>
% endif
```

De implementatie van dit sjabloon komt conceptueel neer op een grote `switch`, waarbij we delegeren naar het juiste sjabloon op basis van wat het statement juist is.

Een aspect dat meer uitleg vraagt, is de `full`-parameter.
Dit geeft aan dat het gegevenstype van de variabele bij een assignment ook nodig is.
Het verschil is duidelijk met een voorbeeld:

```c
int variabele = 5; // Met declaration
variabele = 6; // Zonder variabele.
```

In C is deze parameter minder relevant, maar deze is vooral nodig in talen zoals Java.

## Overige

De overige sjablonen vertalen ook elk een taalelement op een gelijkaardige wijze als het statementsjabloon.
Het gaat om de sjablonen `declaration.mako`, `function.mako`, `value.mako`, `value_arguments.mako` en `value_basic.mako`.

Daar de sjablonen qua werken sterk lijken op het statementsjabloon hebben we ze niet toegevoegd aan dit hoofdstuk: de implementatie ervan is te bekijken in de repository.

# Hulpmodules

Zoals we in het begin van dit hoofdstuk vermeld hebben, zijn er twee bestanden die als "dependency" opgegeven zijn: `values.c` en `values.h`. Deze bestanden implementeren het serialiseren van data naar het serialisatieformaat en vormen samen de `values`-module. De zaken die geserialiseerd moeten worden:

- Waarden, zoals returnwaarden.
- Exceptions (niet het geval in C, want die bestaan niet in C).
- Resultaten van geprogrammeerde en programmeertaalspecifieke evaluaties.

Hier nemen we de implementatie opnieuw niet op, daar de implementatie van deze module sterk programmeertaalafhankelijk is.

# Afsluiting

Als laatste rest nu nog om de nieuwe programmeertaal te registreren bij TESTed. Hiervoor volstaat het om de programmeertaal en de bijhorende configuratieklasse toe te voegen aan het bestand `judge/src/tested/languages/__ini__.py`, in de dictionary `LANGUAGES`:

```python
LANGUAGES = {
    'python':  PythonConfig,
    'java':    JavaConfig,
    'haskell': HaskellConfig,
    'c': CConfig  # Voeg dit toe
}
```

Om de programmeertaal manueel te testen is volgende stappenplan aanbevolen:

1. Implementeer oplossingen voor een of meerdere oefeningen uit de map `exercises` in de nieuwe programmeertaal.
2. Wijzig `judge/src/tested/manual.py` zodat dit bestand de oefening gebruikt waarvoor een oplossing bestaat (en stel ook de juiste programmeertaal in).
3. Voer uit, zoals we in het begin besproken hebben:

```console
> python -m tested.manual
```

TESTed heeft ook een testsuite met verschillende oefeningen en scenario's.
Om de nieuwe programmeertaal hieraan toe te voegen, moeten de juiste oplossingen geïmplementeerd worden.
Hiervoor wordt best gekeken naar `judge/tests/test_functionality.py`. In dat bestand staan de verschillende testen. Bij elke test staat welke oplossing gebruikt wordt; indien het niet duidelijk zou zijn wat de oplossing voor een bepaalde test moet doen, kunnen de bestaande oplossingen in de bestaande programmeertalen een grote hulp zijn.


# Resultaat

We hebben nu een werkende C-judge voor TESTed en bij uitbreiding voor het Dodona-platform, een programmeertaal waarvoor nog geen judge bestaat in Dodona.