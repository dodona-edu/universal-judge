### Opgave

Implementeer een klasse `EqualChecker`, met een constructor die één getal neemt.
De klasse moet één methode `check` hebben, die een `boolean` teruggeeft als de parameter van de methode dezelfde is als de parameter van de constructor.

### Voorbeeld {#voorbeeld}

```pycon
>>> instance = EqualChecker(5)
>>> instance.check(8)
False
>>> instance.check(5)
True
```
