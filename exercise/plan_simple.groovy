plan {
	tab{
		name 'Correctheid'
		context {
			description "Beschrijving van de context"
			execution {
				input {
					stdin {
						data 'proscribable'
						type 'text'
					}
				}
				test {
					output {
						stdout {
							data "proscribable"
							type 'text'
						}
						stderr 'none'
					}
				}
			}
		}
	}
}
