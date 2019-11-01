tab {
	name 'Correctheid'
	context {
		description "Beschrijving van de context"
		execution {
			input {
				stdin {
					type "text"
					data "invoertekst"
				}
			}
			output {
				stdout {
					type "text"
					data "invoertekst"
				}
				specialStderr "none"
			}
		}
		additional {
			input {
				function {
					name "echo"
					type "top"
					argument "test"
					argument 25
				}
				specialStdin "none"
			}
			output {
				stdout "test-25"
				result "test-25"
			}
		}
	}
}
