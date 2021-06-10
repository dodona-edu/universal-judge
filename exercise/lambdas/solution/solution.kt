class Store(val a: Int = 0, val b: Int = 0) {
    fun apply(function: (Int, Int) -> Int): Int {
        return function(this.a, this.b)
    }
}

fun add(a: Int, b: Int): Int {
    return a + b
}

fun createStore(c: (Int, Int) -> Store, a: Int, b: Int): Store {
    return c(a, b)
}
