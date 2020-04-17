Halo [^35]

Dan kan nu eindelijk het onderwerp van dit hoofdstuk onder de loep
genomen worden, namelijk "expressies." Een expressie is een combinatie
van één of meerdere waardes (zoals strings, integers, of floats) met
behulp van operatoren, die dan een nieuwe waarde oplevert. Je kunt je
expressies dus voorstellen als berekeningen.

### Eenvoudige berekeningen

Eenvoudige berekeningen worden gemaakt door twee waardes te combineren
met een operator ertussenin. Een aantal voor de hand liggende operatoren
zijn:

| operator | beschrijving |
|:--------:|:------------|
| `+` | optelling |
| `-` | aftrekking |
| `*` | vermenigvuldiging |
| `/` | deling |
| `//` | integer deling |
| `**` | machtsverheffing |
| `%` | modulo |
{:class="table table-striped table-condensed" style="width:auto;margin-left:auto;margin-right:auto;"}

Hier volgen een paar voorbeelden:

```python
print( 15+4 )
print( 15-4 )
print( 15*4 )
print( 15/4 )
print( 15//4 )
print( 15**4 )
print( 15%4 )
```

Ik neem aan dat je weet wat deze operatoren voorstellen, behalve
misschien de integer deling en de modulo.

De integer deling (ook wel genoemd "floor division") is simpelweg een
deling die naar beneden afrondt naar een geheel getal. Als er floats in
de berekening zitten, is het resultaat nog steeds een float, maar naar
beneden afgerond. Als de berekening alleen integers omvat, is het
resultaat een integer.

De modulo operator (%) produceert de rest die overblijft na deling.
Bijvoorbeeld: als ik 14 deel door 5, is de uitkomst 2.8. Dat betekent
dat ik twee keer 5 kan aftrekken van 14, en dan nog steeds een positief
getal overhoud, maar als ik het een derde keer aftrek wordt het
resultaat negatief. Dus als ik 5 twee keer aftrek van 14, rest er een
getal kleiner dan 5. Deze rest is wat de modulo operator oplevert.

In eenvoudige termen: als ik 14 koekjes heb die ik moet verdelen over 5
kinderen, kan ik ieder kind 2 koekjes geven. Ik heb dan nog 4 koekjes
over, omdat ik dan meer kinderen dan koekjes heb. Dus als je 14 deelt
door 5 met integer deling, geeft dat 2 (koekjes per kind), terwijl 14
modulo 5 als rest 4 (koekjes in mijn hand) geeft.

Als zijdelingse opmerking: de code hierboven bestaat uit meerdere
regels. Iedere regel is één "statement," bestaande uit een commando dat
Python uitvoert (in de code hierboven is dat voor iedere regel een
`print()` commando). De meeste programmeertalen stellen het als een
verplichting dat ieder statement eindigt met een speciaal teken,
bijvoorbeeld een puntkomma ($$;$$). Python verlangt dat niet, maar dan
moet ieder statement ook op zijn eigen regel staan. In principe mag je
meerdere Python statements op één regel zetten, maar dan moeten er
puntkomma's tussen de statements staan. In de praktijk doen Python
programmeurs dat niet, omdat het code lelijk, slecht leesbaar, en slecht
onderhoudbaar maakt. Dus ik stel voor dat je de Python-gewoonte volgt en
ieder statement zijn eigen regel geeft.

### Complexe berekeningen

Je mag waardes en operatoren combineren om grotere berekening te maken,
net zoals je kunt met geavanceerde rekenmachines. Je mag daarbij haakjes
gebruiken om de evaluatievolgorde te bepalen, en je mag die haakjes
zelfs nesten. Zonder haakjes zal Python de operatoren evalueren in de
volgorde die wiskundigen gebruiken, waarvoor op basisscholen vaak de zin
"Meneer Van Dalen Wacht Op Antwoord" wordt gebruikt (machtsverheffen,
vermenigvuldigen, delen, worteltrekken, optellen, aftrekken).[^3]

Bekijk de berekening hieronder en probeer te bepalen wat de uitkomst is
voordat je hem uitvoert in de Python shell.

```python
print( 5*2-3+4/2 )
```

Ik moet een paar opmerkingen maken over deze berekening.

Op de eerste plaats valt het op dat de uitkomst een float is (zelfs al
zijn er geen decimalen). De reden is dat er een deling in de berekening
zit, en dat betekent voor Python dat de uitkomst automatisch een float
is.

Op de tweede plaats is het zo dat, zoals ik al eerder opmerkte, spaties
door Python genegeerd worden. De code hierboven is dus equivalent met:

```python
print( 5 * 2 - 3 + 4 / 2 )
```

Het is zelfs gelijk aan:

```python
print( 5*2 - 3+4    / 2 )
```

Ik heb lange discussies moeten voeren met mensen die beweren dat de code
hierboven als uitkomst $$6.5$$ of $$1.5$$ geeft, want het is toch
*overduidelijk* dat je eerst $$5*2$$ en $$3+4$$ moet berekenen voordat je
toekomt aan die aftrekking en deling. Dat is kolder. Het maakt niet uit
hoeveel spaties je rondom de operatoren zet: spaties worden genegeerd.
Als je echt eerst $$3+4$$ wilt laten berekenen, moet je er haakjes omheen
zetten. Spaties kunnen leesbaarheid verhogen als je ze goed toepast,
maar voor Python zijn ze betekenisloos.

```python
print( (5*2) - (3+4)/2 )
print( ((5*2)-(3+4)) / 2 )
```

Nu is de tijd gekomen om je eerste programma te schrijven. Schrijf een
programma dat het aantal seconden in een week berekent. Je moet daarvoor
natuurlijk niet je rekenmachine of smartphone grijpen, daarop de
berekening doen, en dan gewoon de uitkomst afdrukken. Je moet de
berekening doen in Python code. Het programma is slechts één regel lang,
dus je kunt het gewoon in de Python shell doen, maar ik raad je aan om
echt een programmabestand te maken.

### String expressies

Een aantal van de hierboven genoemde operatoren kunnen voor strings
worden gebruikt, maar niet allemaal.

Specifiek, je kunt de plus ($$+$$) gebruiken om twee strings aan elkaar te
"plakken," en je kunt de ster ($$*$$) gebruiken met een string en een
integer om een string te maken die een herhaling van de originele string
bevat. Zie hier:

```python
print( "tot"+"ziens" )
print( 3*"hallo" )
print( "tot ziens"*3 )
```

Je kunt geen getal optellen bij een string, of twee strings met elkaar
vermenigvuldigen. Zulke operatoren zijn niet gedefinieerd, en geven
foutmeldingen. Geen van de andere operatoren werkt voor strings.

### Type casting

Soms moet je een data type of waarde veranderen in een ander data type.
Je kunt dat doen met "type casting" functies.

In latere hoofdstukken
(<a href="#ch:simplefunctions" data-reference-type="ref" data-reference="ch:simplefunctions">6</a>
en
<a href="#ch:functions" data-reference-type="ref" data-reference="ch:functions">9</a>)
ga ik in detail op functies in, maar voor nu is het voldoende als je
weet dat een functie een naam heeft, en parameters kan hebben tussen de
haakjes die achter de naam staan. De functie doet iets met die
parameters en geeft een resultaat terug. Bijvoorbeeld, de functie
`print()` drukt de parameter waardes af op het scherm, en geeft niks
terug.

Type casting functies nemen de parameter waarde die tussen de haakjes is
gegeven, en geven een waarde terug die (bijna) hetzelfde is als de
parameter waarde, maar van een verschillend type. De drie belangrijkste
type casting functies zijn:

-   `int()` geeft de parameter waarde terug als integer (indien
    noodzakelijk afgerond naar beneden)

-   `float()` geeft de parameter waarde terug als float (waarbij .0
    indien noodzakelijk wordt toegevoegd)

-   `str()` geeft de parameter waarde terug als string

Bekijk de verschillen tussen de volgende twee regels code:

```python
print( 15/4 )
print( int( 15/4 ) )
```

Of de volgende twee regels:

```python
print( 15+4 )
print( float( 15+4 ) )
```

Ik had al aangegeven dat je de plus-operator niet kunt gebruiken om een
getal aan een string vast te plakken. Als je zoiets toch wilt doen, kun
je een oplossing maken met behulp van string type casting:

```python
print( "Ik heb " + str( 15 ) + " appels." )
```

[^3]: In het Engels wordt de volgorde PEMDAS genoemd, en eigenlijk geeft
    die beter aan wat daadwerkelijk de volgorde is: haakjes
    (parentheses), exponenten, multiplicatie en divisie (deling),
    additie (optelling) en subtractie (aftrekking). Feitelijk is de
    Nederlandse volgorde incorrect, omdat worteltrekken na delen wordt
    geplaatst, terwijl wiskundig gezien worteltrekken een vorm van
    machtsverheffen is, en dus voor vermenigvuldigen komt.
    
[^35]: TEST
