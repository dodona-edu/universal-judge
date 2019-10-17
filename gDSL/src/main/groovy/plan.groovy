plan {
	tab{
		name 'Correctheid'
		context {
			description "Beschrijving van de context"
			consummation {
				input {
					stdin {
						data 'proscribable'
						type 'text'
					}
				}
				test {
					output {
						stdout {
							data "Hallo"
							type 'text'
						}
						stderr 'none'
					}
				}
			}
		}
	}
}
