De **lotto** is een vorm van loterij die voornamelijk bekend is vanwege de genummerde balletjes, waarvan er een aantal getrokken worden. Deelnemers mogen zelf hun eigen nummers aankruisen op een lottoformulier. Hoe groter het aantal overeenkomstige nummers tussen het formulier en de getrokken balletjes, hoe groter de geldprijs.

### Opgave  {#opgave}

Schrijf een functie `loterij` waarmee een lottotrekking kan gesimuleerd worden. De functie moet twee parameters `aantal` en `maximum` hebben. Aan de parameter `aantal` (`int`) kan doorgegeven worden hoeveel balletjes $a$ er moeten getrokken worden (standaardwaarde 6). Aan de parameter `maximum` (`int`) kan doorgegeven worden uit hoeveel balletjes $m$ er moet getrokken worden (standaardwaarde 42). Beide parameters kunnen ook weggelaten worden, waarbij dan de standaardwaarde gebruikt moet worden. De balletjes zijn daarbij dus genummerd van 1 tot en met $m$. Je mag ervan uitgaan dat $ 1 \leq a \leq m$. De functie moet een string (`str`) teruggeven die een strikt stijgende lijst (`list`) van $a$ natuurlijke getallen (`int`) beschrijft, waarbij de getallen van elkaar gescheiden zijn door een spatie, een koppelteken (`-`) en nog een spatie. Voor elk getal $n$ moet gelden dat $1 \leq n \leq m$.

### Voorbeeld   {#voorbeeld}

```python
> loterij()
'2 - 17 - 22 - 27 - 35 - 40'
> loterij(8)
'5 - 13 - 15 - 31 - 34 - 36 - 39 - 40'
> loterij(4, 38)
'16 - 20 - 35 - 37'
```
