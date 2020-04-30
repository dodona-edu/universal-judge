# Algemeen

Het implementeren van een programmeertaal in TESTed bestaat uit drie grote onderdelen:

- Het configuratiebestand
- De configuratieklasse
- De sjablonen

Verder kan het tijdens het testen of debuggen nuttig zijn om te zien wat TESTed doet en welke code er gegenereerd wordt. Via de module `tested.manual` kan een oefening uitgevoerd worden, waarbij er uitgebreide logs zijn. De werkmap van deze oefening met de gegenereerde code zal ook in de `judge`-map zijn, teneinde de code te kunnen inkijken (dit in tegenstelling tot de normale manier van uitvoeren, waar deze code in een tijdelijk map geplaatst wordt).
De manuele modus van TESTed kan als volgt uitgevoerd worden vanuit de map `judge/src`:

```bash
python -m tested.manual
```

# Welke functionaliteit we willen ondersteunen

Voor we van start gaan, overlopen we eerst kort welke functionaliteit we willen ondersteunen, langs beide kanten: welke functionaliteit uit C kunnen we aanbieden in TESTed en welke functionaliteit uit TESTed kunnen we implementeren in C?
Uiteraard willen we zoveel mogelijk ondersteunen, maar vooral op het vlak van gegevenstypes zijn er momenteel beperkingen.

Welke basistypes gaan we niet ondersteunen?

- `sequence` - Arrays zijn een speciaal geval in C: statische arrays kunnen bijvoorbeeld niet als returnwaarde dienen, en ook als functieargument zijn ze niet ideaal. Dynamische arrays nemen de vorm aan van een pointer en een grootte. Doordat dit twee waarden zijn, ondersteunt TESTed dat momenteel niet.
- `set` - C heeft geen ingebouwde verzamelingen.
- `map` - C heeft geen ingebouwde map of dict. Er zijn wel structs, maar daarvan is het niet mogelijk om at runtime de velden te achterhalen, waardoor we ze niet kunnen serialiseren.

Welke geavanceerde types gaan we niet ondersteunen?

- `big_int` - C heeft geen ingebouwd type voor getallen van arbitraire grootte.
- `fixex_precision`- C heeft geen ingebouwd type voor kommagetallen me arbitraire precisie.
- Andere datastructuren, zoals `array` en `list` (voor de redenen van hierboven). Ook `tuple` wordt niet ondersteund, omdat het niet bestaat in C.

# Waar de code moet komen

De eerste stap is het aanmaken van een map waarin we de code voor de programmeertaal zullen zetten. Deze map moet de naam van de programmeertaal krijgen en op de juiste plaats binnen TESTed aanwezig zijn. Maak een nieuwe map `judge/src/tested/languages/c`.

# Configuratiebestand

Het configuratiebestand is een json-bestand met enkele eigenschappen van de programmeertaal. Dit configuratiebestand maakt het implementeren van de configuratieklasse een stuk eenvoudiger, omdat die implementatie veel minder lang zal zijn. Maak eerst het configuratiebestand aan: `judge/src/tested/languages/c/config.json`.

Merk op dat het configuratiebestand slechts een hulpmiddel is: indien gewenst kunnen al deze opties ook ingesteld worden door de juiste functie te implementeren in de configuratieklasse, maar we verwachten dat dit in veel gevallen niet nodig zal zijn.

## Algemene opties

- `general.dependencies`: Dit zijn bestanden die beschikbaar zullen zijn tijdens het compileren en tijdens het uitvoeren van de code. In het geval van C is dit de `values` module, waarvan we de implementatie later bespreken.
- `general.selector`: Dit geeft aan of de programmeertaal gebruik maakt van een selector tijdens het uitvoeren van code die gecompileerd is in batchcompilatie. Voor de meeste talen met compilatie zal dit `true` zijn, zoals ook bij C.
- `extensions.file`: Geeft de voornaamste bestandsextensie aan van de bestanden.

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
    "file": "c"
  }
}
```

## Codestijl

Elementen als functies en namespaces kunnen omgezet worden naar de codestijl die gebruikelijk is in de programmeertaal:

```json
{
  "formats": {
    "namespace": "snake_case",
    "function": "snake_case"
  }
}
```

Standaard wordt `snake_case` gebruikt, dus bij C is dit niet strikt nodig om het erbij te zetten. Andere mogelijkheden zijn `camel_case` en `pascal_case`.

## Functionaliteit

De laatste twee blokken in de configuratie geven aan welke constructies en datatypes de programmeertaal ondersteunt. Hieronder is de omzetting van wat we eerder besproken hebben:

```json
{
  "constructs": {
    "exceptions": false,
    "heterogeneous_collections": false,
    "heterogeneous_arguments": false,
    "evaluation": false
  },
  "datatypes": {
    "int8": "supported",
    "uint8": "supported",
    "int16": "supported",
    "uint16": "supported",
    "int32": "supported",
    "uint32": "supported",
    "int64": "supported",
    "uint64": "supported",
    "single_precision": "supported",
    "double_extended": "unsupported",
    "fixed_precision": "unsupported",
    "array": "unsupported",
    "set": "unsupported",
    "map": "unsupported",
    "list": "unsupported",
    "tuple": "unsupported"
  }
}
```

Bij de `constructs` kunnen alle types uit de enum `tested.features.Construct` gegeven worden.
Bij de `datatypes` kunnen alle datatypes uit `tested.datatypes` gebruikt worden. Voor elk datatype zijn drie mogelijkheden:

- `supported` - volledige ondersteuning
- `reduced` - wordt ondersteund, maar wordt herleid tot een basistype (bijvoorbeeld een `list` wordt geïnterpreteerd als een `sequence`)
- `unsupported` - geen ondersteuning

# Configuratieklasse

Maak een nieuw Python-bestand in `judge/src/tested/langauges/c/config.py`.
Hierin moet een klasse komen die `Language` uitbreidt:

```python
from tested.languages import Language

class CConfig(Language):
    pass
```

## Compileren van de testcode

Een eerste en belangrijke functie om te implementeren is de functie die de callback voorziet voor de compilatiestap:

```python
def compilation(self, files: List[str]) -> CallbackResult:
    main = files[-1]
    exec_file = Path(main_file).stem
    result = executable_name(exec_file)
    return ["gcc", "-std=c11", "-Wall", "values.c", main, "-o", result], [result]
```

De exacte details over hoe deze functie geïmplementeerd moet worden in beschreven in de documentatie van de superklasse `Language`. Samengevat krijgt deze functie als parameter een lijst van bestanden waarvan TESTed denkt dat ze gecompileerd kunnen worden. Conventioneel is het laatste bestand het bestand met de `main`-functie.
Als returnwaarde moet deze functie een tuple met twee dingen teruggeven:

- Het commando dat uitgevoerd moet worden voor de compilatie. Dit commando zal aan de module `subprocess` van Python gegeven worden.
- Een lijst van bestanden die het resultaat zijn van de compilatie of een functie die de lijst van bestanden filtert.

Die laatste waarde vraagt wat meer uitleg: nadat TESTed de compilatie uitgevoerd heeft, zullen enkel de bestanden in deze lijst gebruikt worden. Bij Python gaat het om `pyc`-bestanden, terwijl hier bij C enkel het uitvoerbaar bestand gebruikt wordt.

Soms is een statische lijst van bestanden niet voldoende, en is een filterfunctie nodig, zoals wanneer de gegenereerde bestanden hangt af van de inhoud van de bestanden die gecompileerd worden. Dit is bijvoorbeeld het geval bij Java, waar één `java`-bestand in meerdere `class`-bestanden kan resulteren.
- Niet alle bestanden zijn nodig voor elke context. In Python wordt bijvoorbeeld voor elke context een `context_x_y_.py` gecompileerd naar een `context_x_y.pyc` bestand. Om context XY uit te voeren, is wel enkel dat ene bestand nodig, niet de bestanden van alle contexten.
Een voorbeeld van hoe deze filterfunctie gebruikt kan worden is te zien in de configuratieklassen voor Python en Java.

Een programmeertaal die geen compilatie nodig heeft kan een leeg compilatiecommando teruggeven (of deze functie niet implementeren, een leeg commando is de standaardimplementatie).

Merk tot slot op dat we hier in C de `files` niet gebruiken: de linker van `gcc` vindt vanzelf de meeste bestanden (buiten `values.c`).

Een voorbeeld van de in- en uitvoer van deze functie:

```pycon
>>> generation_callback(["submission.c", "context0.c", "selector.c"])
(
    ["gcc", "-std=c11", "-Wall", "values.c", "selector.c", "-o", "selector.exe"],
    ["selector.exe"]
)
```

## Uitvoeren van de testcode

Na het compileren moeten we nu een functie implementeren om de gecompileerde code uit te voeren:

```python
def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
    local_file = cwd / executable_name(Path(file).stem)
    return [str(local_file.absolute()), *arguments]
```

Deze functie heeft enkele parameters:

- `cwd`: de map waarin het uitvoeren plaatsvindt
- `file`: het uitvoerbaar bestand dat moeten worden uitgevoerd
- `arguments`: argumenten die aan het proces meegegeven moeten worden

Als resultaat het is het commando nodig, dat ook aan `subprocess` gegeven wordt.

In het geval van C is dit commando eenvoudig: we geven het absolute pad naar het uitvoerbare bestand mee en geven ook de argumenten mee. Het absolute pad is nodig omdat de executable die we willen uitvoeren (en gemaakt hebben in de compilatiestap) niet in de `PATH` zit.

Een voorbeeld van deze functie in werking is:

```pycon
>>> execution("/test/path", "executable.exe", ["arg1", "arg2"])
["/test/path/executable.exe", "arg1", "arg2"]
```

## Aanpassen van de oplossingscode

In TESTed kunnen er meerdere `main`-functies zijn:

- De oplossing van de student kan een `main`-functie hebben.
- Zowel de contexten als de selector kunnen `main`-functies hebben.

C staat slechts één `main`-functie toe.

Een ander probleem is dat de selector elke context insluit (zoals we later zullen zien bij de sjablonen), en elke context ook de oplossing insluit.

Om deze redenen moeten we de code van de oplossing een beetje aanpassen:

- We voegen aan `guard` toe, zodat de oplossing eenmaal geladen wordt.
- We hernoemen de `main`-functie naar `solution_main` indien die bestaat.

```python
def solution(self, solution: Path, bundle: Bundle):
    with open(solution, "r") as file:
        contents = file.read()
    with open(solution, "w") as file:
        header = "#pragma once\n\n"
        result = header + contents.replace("main", "solution_main")
        file.write(result)
```

# Sjablonen

De tweede stap is het implementeren van de sjablonen. Naast de verplichte sjablonen implementeren we ook een paar bijkomende:

- `context.c` - het sjabloon voor de contextcode te genereren
- `selector.c` - het sjabloon voor de selecter voor batchcompilatie
- `declaration.mako` - vertaal het type van een variabele naar code
- `function.mako`-  vertaalt een functieoproep
- `statement.c` - vertaalt een statement of expressie naar code
- `value.mako` - vertaalt een waarde naar code
- `value_arguments.mako` - hulpsjabloon voor `value`
- `value_basic.mako`- hulpsjabloon voor `value`

Al deze sjablonen komen in `judge/src/tested/languages/c/templates`.

## Het contextsjabloon

Dit is veruit het grootste en ingewikkeldste sjabloon. Het is verantwoordelijk om de testcode te genereren voor één context.

```mako
<%! from tested.languages.generator import _TestcaseArguments %>
<%! from tested.serialisation import Statement, Expression %>
<%! from tested.utils import get_args %>

#include <stdio.h>

#include "values.h"
#include "${submission_name}.c"
```

We importeren de values-module (hierover later meer) en de oplossing van de student.
De variabele `submission_name` zal de naam van het oplossingsbestand bevatten.

```mako
% for name in evaluator_names:
    #include "${name}.c"
% endfor
```

In `evaluator_names` zitten de namen van de programmeertaalspecifieke evaluatoren die we nodig hebben. We importeren (of _includen_) die.

Vervolgens maken we twee variabelen waarin de bestanden komen die dienst doen als exception- en return-channel. Merk op dat C geen exceptions ondersteunt, maar TESTed verwacht toch een bestand. Anders zal TESTed ervan uitgaan dat er iets verkeerds liep tijdens het uitvoeren. We definiëren ook direct een functie om de separator naar alle uitvoerkanalen te schrijven.

We gebruiken de naam van de context als prefix: dit is omdat in batchcompilatie, alle contexten samen gecompileerd worden. In C is het niet mogelijk om dezelfde naam meerdere keren te gebruiken, dus prefixen we met de contextnaam. In andere programmeertalen met geavanceerdere modulesystemen (zoals Java, Python of Haskell) is dat niet nodig. 

```mako
static FILE* ${context_name}_value_file = NULL;
static FILE* ${context_name}_exception_file = NULL;

static void ${context_name}_write_delimiter() {
    fprintf(value_file, "--${secret_id}-- SEP");
    fprintf(exception_file, "--${secret_id}-- SEP");
    fprintf(stdout, "--${secret_id}-- SEP");
    fprintf(stderr, "--${secret_id}-- SEP");
}
```

Nu definiëren we enkele standaardfuncties die gebruikt worden om een resultaat te verwerken. Normaal gaat het om deze functies:

- `send` - schrijf een waarde naar een bestand.
- `send_exception` - schrijf een exception naar een bestand.
- `send_specific_value` - schrijf het resultaat van een programmeertaalspecifieke evaluatie naar de return-channel.
- `send_specific_exception` - schrijf het resultaat van een programmeertaalspecifieke evaluatie naar de exception-channel.

In C wijken we daar lichtjes van af:

- De twee functies die we implementeren zijn niet als functie geïmplementeerd, maar als macro. Dit is opnieuw omdat het in C niet gaat om twee bestanden met functies die dezelfde naam hebben samen te compileren.
- De `exception`-functies implementeren we niet.

De implementatie van deze functies is eenvoudig: meestal wordt gewoon de overeenkomende functie uit de `values`-module opgeroepen met het juiste bestand als parameter.

```mako
#undef send
#define send(value) write_value(${context_name}_value_file, value)

#undef send_specific_value
#define send_specific_value(r) send_evaluated(${context_name}_value_file, r)
```

Elke testgeval heeft twee functies evaluatiefuncties nodig:

- `v_evaluate_x` - verwerkt de waarde van testgeval `x`.
- `e_evaluate_x` - verwerkt de exception van testgeval `x`.

De inhoud van deze functies wordt door TESTed gegeven als variabele in `testcase.value_function`. Deze functie bevat ofwel de code om een programmeertaalspecifieke evaluator op te roepen, of bevat de code om een van de standaardfuncties die we hierboven gedefinieerd hebben op te roepen. Merk op dat we deze variabele meegeven aan het functiesjabloon, want deze variabele bevat een functieoproep, geëncodeerd in het serialisatieformaat.

Bij C implementeren we de exception-functie opnieuw niet. De eerste functie is ook een macro om meerdere gegevenstypes te kunnen aanvaarden en gebruikt de prefix.

```mako
% for testcase in testcases:
    % if testcase.value_function:
        #define ${context_name}_v_evaluate_${loop.index}(value) <%include file="statement.mako" args="statement=testcase.value_function"/>
    % endif
% endfor
```

Nu zijn we aangekomen bij het eigenlijk uitvoeren van de testgevallen. In C gebeurt dit in een functie, die de naam van de context krijgt. Als eerste stap maken we nu effectief de bestanden voor de return- en exception-channel.

```mako
int ${context_name}() {

    ${context_name}_value_file = fopen("${value_file}", "w");
    ${context_name}_exception_file = fopen("${exception_file}", "w");
```

Als volgende zetten we de `before`-code.
We schrijven altijd de separator uit voor de start van het testgeval, wat we hier ook doen.
Tot slot roepen we de `main`-functie van de oplossing op indien het testplan dat vereist.

TODO: arguments in de main-functie

```mako
    ${before}

    ${context_name}_write_delimiter();

    % if context_testcase.exists:
        solution_main();
    % endif
```

Hieronder doen we de eigenlijke testgevallen. Elk testgeval heeft een statement als invoer. Indien dit statement een expression is en we zijn geïnteresseerd in de returnwaarde, moeten we natuurlijk de waarde opvangen. Dat gebeurt met `v_evaluate_${loop.index}`. De `\` op het einde duidt aan dat het regeleinde weggelaten mag worden.

Na elke testcase schrijven we ook opnieuw het scheidingsteken uit.

```mako
    % for testcase in testcases:
        ${context_name}_write_delimiter();
        ## Als value_function bestaat, hebben we een expressie.
        ## Dan moeten we het resultaat aan TESTed geven.
        % if testcase.value_function:
            ${context_name}_v_evaluate_${loop.index}(\
        % endif
        <%include file="statement.mako" args="statement=testcase.command" />\
        % if testcase.value_function:
            )\
        % endif
        ;        
    % endfor
```

Als er dus een expressie is, zal bovenstaande fragment leiden tot een functieoproep die er zo uitziet:

```c
context_0_1_v_evaluate_1(functieoproep());
```

Indien er geen expressie is, wordt het:

```c
functieoproep();
```

Als afsluiter zetten we de `after`-code en sluiten we de bestanden:

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

Het is nuttig om meteen het selectorsjabloon erbij te halen: dit wordt gebruikt als TESTed in batchcompilatie werkt en is verantwoordelijk om de juiste context uit te voeren op basis van een argument.
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

## Deserialisatiesjablonen

De sjablonen `declaration.mako`, `function.mako`, `statement.c`, `value.mako`, `value_arguments.mako` en `value_basic.mako` zorgen er samen voor dat items het uit serialisatieformaat in C-code omgezet kunnen worden.

De implementatie van deze sjablonen is niet bijster ingewikkeld: ze komen neer op een groot `switch`-statement dat de verschillende items omzet. De implementaties van deze sjablonen (zowel in C, maar ook die van Java, Python en Haskell) kunnen inspiratie bieden hoe deze te implementeren zijn.

# Hulpmodules

Zoals gezegd helemaal in het begin zijn er twee bestanden die als "dependency" opgegeven zijn: `values.c` en `values.h`. Deze bestanden implementeren het serialiseren van data naar het serialisatieformaat. Er moeten enkele zaken geserialiseerd kunnen worden:

- Waarden, zoals returnwaarden.
- Exceptions (niet het geval in C, want die bestaan niet in C).
- Resultaten van geprogrammeerde en programmeertaalspecifieke evaluaties.

Opnieuw zijn de implementaties de beste handleiding. Ook is de manier waarop dit geïmplementeerd wordt vrij: in alle bestaande programmeertalen is gekozen voor bijkomende bestanden, maar niets houdt tegen op de waarden _inline_ te serialiseren in het contextsjabloon (op de posities waar nu een oproep van een functie uit `values.c` gebeurt). 

# Afsluiting

Als laatste rest ons nu nog TESTed duidelijk maken dat onze nieuwe programmeertaal gebruikt kan worden. Hiervoor volstaat het om de programmeertaal en bijhorende configuratieklasse toe te voegen aan het bestand `judge/src/tested/languages/__ini__.py`, in de dictionary `LANGUAGES`:

```python
LANGUAGES = {
    'python':  PythonConfig,
    'java':    JavaConfig,
    'haskell': HaskellConfig,
    'c': CConfig  # Voeg dit toe
}
```

Om de programmeertaal te testen is het aan te raden volgende stappen te doen:

1. Implementeer enkele oefeningen uit `exercises` in de repository van TESTed in de nieuwe programmeertaal.
2. Voeg de nieuwe programmeertaal toe aan de testsuite bij de gepaste oefeningen. Is bijvoorbeeld de oefening `echo` geïmplementeerd, kan de programmeertaal toegevoegd worden aan de testen die deze oefening gebruiken.
