class Counter {
    var counter : Int = 0

    fun add(): Counter {
        counter += 1
        return this
    }

    fun get() : Int {
        return counter
    }
}
