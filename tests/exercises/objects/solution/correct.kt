class EqualChecker(private val value: Any?) {
    fun check(other: Any?): Boolean {
        return other == value
    }
}