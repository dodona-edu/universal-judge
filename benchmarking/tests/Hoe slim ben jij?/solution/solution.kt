fun main() {
    // Getal inlezen
    val getal: String = readLine()!!
    val map: Map<Char, Int> = mapOf(Pair('0', 1), Pair('4', 1), Pair('6', 1), Pair('8', 2), Pair('9', 1))
    var cirkels: Int = 0

    // Aantal cirkels in de cijfers van het getal tellen
    for (i in 0 until getal.length) {
        cirkels += map.getOrDefault(getal[i], 0)
    }

    // Aantal cirkels uitschrijven
    println(cirkels)
}