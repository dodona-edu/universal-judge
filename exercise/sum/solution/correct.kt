import kotlin.system.exitProcess

fun main(args: Array<String>) {
    var som = 0;
    for (arg in args) {
        val num = arg.toIntOrNull();
        if (num == null) {
            System.err.println("som: ongeldige argumenten")
            exitProcess(1)
        }
        som += num
    }
    println(som)
}