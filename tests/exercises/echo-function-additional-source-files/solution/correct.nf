include { my_echo } from './echo'

process echo {
    input:
    val x
    output:
    stdout
    exec:
    my_echo(x)
}
