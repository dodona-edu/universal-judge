// Zoals duidelijk is, kan het plan redelijk compact zijn met minimale informatie
// Het eerste voorbeeld toont wel aan dat het uiteraard mogelijk is om alles tot in details te definiëren.
plan {
	tab{
		name 'Correctheid'
		context {
			// 1. Dingen voor het uitvoeren (before), bv. argumenten (Jupyter command line misschien)

			// 2. Run testcase (uitvoeren code van studenten) + argument
			// -> input, output, etc.

			// 3. After testcases (neveneffecten) (functie/... is verplicht)

			// Exit status
			before {
				// TODO: what moet dit precies zijn? Code uitvoeren? Variabelen klaarzetten?
				//   In welke taal moet dat zijn?
			}
			input 'proscribable'
			output 'proscribable'
			run {
				// uitvoeren van de code (studenten)
			}

			testcase {
				description "Volledige testcase"
				input {
					stdin {
						data "input1.txt"
						type 'file'
					}
				}

				output {
					stdout {
						data "Hallo"
						type 'text'
					}
					stderr 'ignored'
					file {

					}
				}
				evaluators {
					stdout: {

					}
				}

			}
		}
		runArgs {
			// Configuratie voor testen die met objecten werken, zoals Java.
			classname 'Main'
		}
		test {
			input 'proscribable'
			output 'proscribable'
		}
		test {
			input 'crystallisation'
			output 'crystallisation'
		}
		test {
			input 'overfruitful'
			output 'overfruitful'
		}
		test {
			input 'sterilizable'
			output 'sterilizable'
		}
		test {
			input 'unforgivingness'
			output 'unforgivingness'
		}
		test {
			input 'snatchy'
			output 'snatchy'
		}
		test {
			input 'dropworts'
			output 'dropworts'
		}
		test {
			input 'chaufers'
			output 'chaufers'
		}
		test {
			input 'hyperbolized'
			output 'hyperbolized'
		}
		test {
			input 'bicentennially'
			output 'bicentennially'
		}
		test {
			input 'curring'
			output 'curring'
		}
		test {
			input 'ageratum'
			output 'ageratum'
		}
		test {
			input 'preallotment'
			output 'preallotment'
		}
		test {
			input 'deputize'
			output 'deputize'
		}
		test {
			input 'anthropopsychic'
			output 'anthropopsychic'
		}
		test {
			input 'flustroid'
			output 'flustroid'
		}
		test {
			input 'ketones'
			output 'ketones'
		}
		test {
			input 'cantharidian'
			output 'cantharidian'
		}
		test {
			input 'adenolymphoma'
			output 'adenolymphoma'
		}
		test {
			input 'headlongly'
			output 'headlongly'
		}
		test {
			input 'linoleums'
			output 'linoleums'
		}
		test {
			input 'pseudoracemic'
			output 'pseudoracemic'
		}
		test {
			input 'flagpole'
			output 'flagpole'
		}
		test {
			input 'extraessential'
			output 'extraessential'
		}
		test {
			input 'unclustering'
			output 'unclustering'
		}
		test {
			input 'preglobulin'
			output 'preglobulin'
		}
		test {
			input 'spiderflower'
			output 'spiderflower'
		}
		test {
			input 'uncorker'
			output 'uncorker'
		}
		test {
			input 'antithrombic'
			output 'antithrombic'
		}
		test {
			input 'lienomedullary'
			output 'lienomedullary'
		}
		test {
			input 'unshrivelled'
			output 'unshrivelled'
		}
		test {
			input 'benison'
			output 'benison'
		}
		test {
			input 'copiable'
			output 'copiable'
		}
		test {
			input 'infiltering'
			output 'infiltering'
		}
		test {
			input 'incivil'
			output 'incivil'
		}
		test {
			input 'turncoat'
			output 'turncoat'
		}
		test {
			input 'autosome'
			output 'autosome'
		}
		test {
			input 'outfight'
			output 'outfight'
		}
		test {
			input 'numbnesses'
			output 'numbnesses'
		}
		test {
			input 'reforestization'
			output 'reforestization'
		}
		test {
			input 'mastochondrosis'
			output 'mastochondrosis'
		}
		test {
			input 'ridottos'
			output 'ridottos'
		}
		test {
			input 'reapproval'
			output 'reapproval'
		}
		test {
			input 'grasping'
			output 'grasping'
		}
		test {
			input 'dioicously'
			output 'dioicously'
		}
		test {
			input 'synergia'
			output 'synergia'
		}
		test {
			input 'macrauchene'
			output 'macrauchene'
		}
		test {
			input 'schizospore'
			output 'schizospore'
		}
		test {
			input 'nonzero'
			output 'nonzero'
		}
		test {
			input 'seminomadism'
			output 'seminomadism'
		}
	}
}
