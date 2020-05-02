
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

Merk op dat het configuratiebestand slechts een hulpmiddel is: indien gewenst kunnen al deze opties ook ingesteld worden door de juiste functie te implementeren in de configuratieklasse, maar we verwachten dat dit in veel gevallen niet nodig zal zijn.

## Algemene opties

- `general.dependencies` - Dit zijn bestanden die beschikbaar zullen zijn tijdens het compileren en tijdens het uitvoeren van de beoordeling. Dit betekent dat deze dependencies gebruikt kunnen worden in de testcode voor de contexten en de evaluatiecode voor de geprogrammeerde en programmeertaalspecifieke code. In het geval van C is dit de `values` module, waarvan we de implementatie later bespreken. Het kan gebeuren dat de code van de dependencies ook beschikbaar is voor de ingediende oplossing. Dit is echter toeval en niet de bedoeling. Er is momenteel geen ondersteuning om dependencies beschikbaar te maken voor de ingediende oplossing.
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

Taalelementen als functies en namespaces kunnen omgezet worden in functie van de codestijl die gebruikelijk is in de programmeertaal:

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

De laatste twee blokken in de configuratie geven aan welke constructies en datatypes de programmeertaal ondersteunt. We hebben reeds besproken welke functionaliteit we willen ondersteunen en welke niet (TODO: reference!). We beginnen met de taalconstructies vast te leggen:

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

Hier kan voor elke taalconstructie opgegeven worden of ze ondersteund wordt of niet. Standaard wordt geen enkele taalconstructie ondersteunt: dit zorgt ervoor dat alle ondersteunde constructies expliciet in het configuratiebestand staan en dat nieuwe taalconstructies toegevoegd kunnen worden zonder dat bestaande configuraties van programmeertalen aangepast moeten worden.

De mogelijke taalconstructie zijn deze uit de enum `tested.features.Construct`. Voor het gemak volgt hieronder een oplijsten en een korte beschrijving van elke taalconstructie:

`objects`
: Objectgeoriënteerde zaken zoals klassen.

`exceptions`
: Exceptions en uitzonderingen.

`function_calls`
: Functieoproepen. Merk op dat constructors in het testplan een speciale soort functie zijn, maar deze hangen af van de taalconstructie `objects`.

`assignments`
: Het toekennen van een waarde aan een variabele. Dit moet eerder los geïnterpreteerd worden als ondersteuning voor iets dat neerkomt op een assigment. Zo kent Haskell bijvoorbeeld geen assignments, want `x = 5` definieert technisch gezien een functie met een constante returnwaarde `5`. Dit moet ook onder `assignments` gerekend worden.

`heterogeneous_collections`
: Hiermee bedoelen we verzamelingen met elementen met verschillende gegevenstypes. Dit is bijvoorbeeld geen probleem in Python (`[5, 52.23]`), gaat al iets moeilijker in Java (`List<Object> = List.of(1, 52.23)`), maar zal niet lukken in Haskell.

`heterogeneous_arguments`
: Hiermee bedoelen we functieoproepen waarbij dezelfde functie meerdere keren wordt opgeroepen met argumenten met verschillende datatypes (bijvoorbeeld eerst `check(True)` daarna `check("hallo")`). Dit zal lukken in Python en Java, maar niet in Haskell en C.

`evaluation`
: Of een geprogrammeerde evaluatie mogelijk is in deze programmeertaal. Dit is technisch gezien geen taalconstructie, maar wordt erbij genomen omdat dezelfde infrastructuur gebruikt wordt om te controleren of dit nodig is of niet.

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

Een opmerking hierbij is dat de status `reduced` voor de basistypes equivalent is aan `supported`; een basistype reduceren tot een basistype blijft hetzelfde type.

Het is de bedoeling dat de meeste programmeertalen voor het merendeel van de datatypes ten minste `reduced` hebben. Toch is gekozen om `unsupported` als standaardwaarde te nemen; dit zorgt ervoor dat de ondersteunde datatypes explicit uitgeschreven zijn. Ook laat dit opnieuw toe om datatypes toe te voegen aan TESTed zonder bestaande configuraties van programmeertalen te moeten aanpassen.
Ter illustratie vermelden we hier bij C alle datatypes, ook de niet-ondersteunde.

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

Een eerste en belangrijke methode is de methode die de callback voorziet voor de compilatiestap:

```python
def compilation(self, files: List[str]) -> CallbackResult:
    main_file = files[-1]
    exec_file = Path(main_file).stem
    result = executable_name(exec_file)
    return ["gcc", "-std=c11", "-Wall", "evaluation_result.c", "values.c", main_file, "-o", result], [result]
```

Doordat het compileren een samenspel is van gegenereerde code uit de sjablonen en de dependencies, moet deze functie met zorg geïmplementeerd worden. Hieronder volgt een (licht gewijzigde) versie van de documentatie van deze methode.

Als argument krijgt deze methode een lijst van bestanden mee waarvan TESTed vermoed dat ze nuttig kunnen zijn voor de compilatiestap. Het bevat onder andere de dependencies uit het configuratiebestand, de ingediende oplossing en de uit de sjablonen gegenereerde bestanden. Die laatste bestanden zijn bijvoorbeeld de verschillende contexten bij een batchcompilatie, maar kunnen ook de evaluator zijn bij een geprogrammeerde evaluatie. De bestanden bestaan uit de naam en een bestandsextensie.

De conventie is om het bestand met de main-functie als laatste te plaatsen.

Al deze bestanden zullen zich bevinden in de map waarin de compilatie plaatsvindt.
Het is niet verplicht al deze bestanden ook effectief te gebruiken: sommige programmeertalen hebben zelf een detectiesysteem voor bestanden.

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
