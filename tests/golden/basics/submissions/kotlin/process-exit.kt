import kotlin.system.exitProcess

fun add(a: Int, b: Int): Int {
    return a + b
}

fun greet(name: String): String {
    exitProcess(1)
}

fun printGreeting(name: String) {
    println("Hello, $name!")
}
