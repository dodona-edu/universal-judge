fun isIsbn10(isbn: String?): Boolean {
    if (isbn == null || isbn.length != 10) {
        return false
    }
    var checksum = 0
    for (i in 1 until 10) {
        val char = isbn[i - 1]
        if (!char.isDigit()) {
            return false
        }
        val asNumber = char.toInt() - 48
        checksum += i * asNumber
    }
    checksum %= 11
    val checkDigit = if (isbn[9] == 'X') 10 else if (isbn[9].isDigit()) isbn[9].toInt() - 48 else -1
    return checksum == checkDigit
}

fun isIsbn13(isbn: String?): Boolean {
    if (isbn == null || isbn.length != 13) {
        return false
    }
    var o = 0
    var e = 0
    for (i in 1 until 13) {
        val char = isbn[i - 1]
        if (!char.isDigit()) {
            return false
        }
        val asNumber = char.toInt() - 48
        if (i % 2 == 0) {
            e += asNumber
        } else {
            o += asNumber
        }
    }
    val checksum = (10 - (o + 3 * e) % 10) % 10
    val checkDigit = if (isbn[12].isDigit()) isbn[12].toInt() - 48 else -1
    return checksum == checkDigit
}

fun isIsbn(isbn: Any?, isbn13: Boolean = true): Boolean {
    if (isbn !is String) {
        return false
    }
    return if (isbn13) {
        isIsbn13(isbn)
    } else {
        isIsbn10(isbn)
    }
}

fun checkIsbn(isbn: Any?, isbn13: Boolean?): Boolean {
    if (isbn !is String) {
        return false
    }
    return isIsbn(isbn, isbn13 ?: (isbn.length == 13))
}

fun areIsbn(code: List<Any?>?, isbn13: Boolean? = null): List<Boolean> {
    return code!!.asSequence()
            .map { c -> checkIsbn(c, isbn13) }
            .toList()
}

fun main() {
    // Some checks from the python doctests.
    check(!isIsbn10("0012345678"))
    check(isIsbn10("0012345679"))
    check(isIsbn10("9971502100"))
    check(!isIsbn("9971502108"))
    check(isIsbn13("9789743159664"))
    check(!isIsbn13("9787954527409"))
    check(!isIsbn13("8799743159665"))
    check(!isIsbn("9789027439642", false))
    check(isIsbn("9789027439642", true))
    check(isIsbn("9789027439642"))
    check(!isIsbn("080442957X"))
    check(isIsbn("080442957X", false))
    val codes = listOf("0012345678", "0012345679", "9971502100", "080442957X", 5, true,
            "The Practice of Computing Using Python", "9789027439642", "5486948320146");
    check(areIsbn(codes) == listOf(false, true, true, true, false, false, false, true, false))
    check(areIsbn(codes, true) == listOf(false, false, false, false, false, false, false, true, false))
    check(areIsbn(codes, false) == listOf(false, true, true, true, false, false, false, false, false))
}
