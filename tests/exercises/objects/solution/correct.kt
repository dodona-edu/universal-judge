class EqualChecker(private val value: Any?) {

    var prop: Int = 0;

    fun check(other: Any?): Boolean {
        return other == value
    }
}
