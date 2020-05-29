### Opgave

Implementeer een klasse `EqualChecker`, met een constructor die één getal neemt.
De klasse moet één methode `check` hebben, die een `boolean` teruggeeft als het argument van de methode hetzelfde is als het argument dat we aan de constructor gegeven hebben.

### Voorbeeld {#voorbeeld}

```pycon
>>> instance = EqualChecker(5)
>>> instance.check(8)
False
>>> instance.check(5)
True
```
