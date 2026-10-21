import kotlin.system.exitProcess

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        exitProcess(1)
    }

    val prefix = args[0]
    args.drop(1).forEach { println("$prefix: $it") }
    generateSequence(::readLine).forEach { println("$prefix: $it") }
}
