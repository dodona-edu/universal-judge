Binnen het ISBN-10 (_International Standard Book Numbering_) systeem dat tot eind 2006 gebruikt werd, kreeg elk boek een unieke code toegewezen die bestaat uit 10 cijfers. De eerste 9 daarvan geven informatie over het boek zelf, terwijl het laatste louter een controlecijfer is dat dient om foutieve ISBN-10 codes te detecteren.

<div class="dodona-centered-group">
<img src="media/ISBN.gif" alt="IBSN">
</div>

Indien $$x_1, \ldots, x_9$$ de eerste 9 cijfers van een ISBN-10 code voorstellen, dan wordt het controlecijfer $$x_{10}$$ als volgt berekend:

$$
x_{10} = (x_1+ 2x_2+ 3x_3+ 4x_4+ 5x_5+ 6x_6+ 7x_7+ 8x_8+ 9x_9) \mod{11}
$$

Het controlecijfer $$x_{10}$$ kan m.a.w. de waarden 0 tot en met 10 aannemen. Indien het controlecijfer gelijk is aan 10, dan wordt dit in de ISBN-10 code genoteerd als de letter `X`.

Binnen het nieuwe ISBN-13 systeem krijgt elk boek een unieke code bestaande uit 13 cijfers. De eerste 12 daarvan geven informatie over het boek zelf, terwijl het laatste louter een controlecijfer is dat dient om foutieve ISBN-13 codes te detecteren. Indien $$x_1, \ldots, x_{12}$$ de eerste 12 cijfers van een ISBN-13 code voorstellen, dan wordt het controlecijfer $$x_{13}$$ als volgt berekend: 

$$
\begin{align}
 o &= x_1 + x_3 + x_5 + x_7 + x_9 + x_{11} \\
 e &= x_2 + x_4 + x_6 + x_8 + x_{10} + x_{12} \\
 x_{13} &= (10 - (o + 3e) \mod{10}) \mod{10}
\end{align}
$$

Het controlecijfer $$x_{13}$$ kan m.a.w. de waarden 0 tot en met 9 aannemen, waardoor ISBN-13 codes uitsluitend uit cijfers bestaan.

### Opgave

*   Schrijf een functie `is_isbn` waaraan een string $$c$$ (`str`) moet doorgegeven worden. De functie moet een Booleaanse waarde (`bool`) teruggeven, die aangeeft of $$c$$ een geldige ISBN-code is. De functie heeft ook een tweede parameter `isbn13` waaraan een Booleaanse waarde (`bool`) moet doorgegeven worden die aangeeft of het om een ISBN-10 code (`False`) of om een ISBN-13 code (`True`) moet gaan.

*   Schrijf een functie `are_isbn` waaraan een lijst (`list`) met $$n \in \mathbb{N}$$ codes moet doorgegeven worden. De functie moet voor alle codes uit de gegeven lijst aangegeven of ze geldige ISBN-codes voorstellen. De functie heeft ook een tweede parameter `isbn13` waaraan een Booleaanse waarde (`bool`) moet doorgegeven worden die aangeeft of het om ISBN-10 codes (`False`) of om ISBN-13 codes (`True`) moet gaan.

    De functie moet een nieuwe lijst (`list`) met $$n$$ Booleaanse waarden (`bool`) teruggeven, die aangeven of de code op de corresponderende positie in de gegeven lijst een geldige ISBN-code is.

### Voorbeeld

```pydocstring
>>> is_isbn('9789027439642', False)
False
>>> is_isbn('9789027439642', True)
True
>>> is_isbn('080442957X', True)
False
>>> is_isbn('080442957X', False)
True

>>> codes = ['0012345678', '0012345679', '9971502100', '080442957X', 'The Practice of Computing Using Python', '9789027439642', '5486948320146']
>>> are_isbn(codes, True)
[False, False, False, False, False, False, False, True, False]
>>> are_isbn(codes, False)
[False, True, True, True, False, False, False, False, False]
```

### Pythia spreekt …

In onderstaande video legt Pythia uit hoe je deze opgave kunt aanpakken. Bekijk deze video als opstapje naar het oplossen van de oefeningen over [geavanceerde functies en modules](https://dodona.ugent.be/nl/exercises/?filter=opgaven/reeks07).

<div class="dodona-centered-group"><iframe src="https://www.youtube.com/embed/KYkcbV66zNk" allow="autoplay; encrypted-media" allowfullscreen="" height="315" width="560"></iframe></div>
