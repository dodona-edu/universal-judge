import java.util.concurrent.ThreadLocalRandom

fun loterij(aantal: Int = 6, maximum: Int = 42): String {
    val randomGenerator = ThreadLocalRandom.current()
    val result: HashSet<Int> = HashSet()
    while (result.size < aantal) {
        result += randomGenerator.nextInt(1, maximum + 1)
    }
    return result.asSequence()
            .sorted()
            .joinToString(
                    separator = " - ",
                    transform = { i -> i.toString() }
            )
}