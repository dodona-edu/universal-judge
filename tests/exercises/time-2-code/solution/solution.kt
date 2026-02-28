import java.io.File

fun load(filename: String): String {
    val file = File(filename)
    return if (file.exists()) file.readText().trim() else ""
}

fun save(user: String, filename: String) {
    File(filename).writeText("$user\n")
}

fun main() {
    var user = load("datafile.txt")

    if (user.isEmpty()) {
        println("Hello, I don't believe we have met.")
        user = readln()
        save(user, "datafile.txt")
        println("Nice to meet you $user.")
    } else {
        println("It's good to see you again, $user.")
    }
}
