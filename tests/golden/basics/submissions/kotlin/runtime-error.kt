fun add(a: Int, b: Int): Int {
    return a + b
}

fun greet(name: String): String {
    throw IllegalArgumentException("cannot greet $name")
}

fun printGreeting(name: String) {
    println("Hello, $name!")
}
