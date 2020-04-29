Schrijf een programma `som` waaraan nul of meer argumenten kunnen doorgegeven worden. Als minstens één van de argumenten geen geheel getal is, dan moet het programma de boodschap `som: ongeldige argumenten` uitschrijven naar `stderr` en eindigen met *exit status* 1. Anders moet het programma de som van de getallen uitschrijven naar `stdout` en eindigen met *exit status* 0.

### Voorbeeld

```console?lang=bash&prompt=$
$ python ./som
0
$ python ./som 1 2 3 4 5 6 7 8 9 10
55
$ python ./som 1 -2 3 -4 5 -6 7 -8 9 -10
-5
$ python ./som a b c
som: ongeldige argumenten
$ echo $?
1
```
