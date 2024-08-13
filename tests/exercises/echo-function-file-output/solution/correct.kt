import java.io.File

fun echoFunction(filename: String, stringToWrite: String) {
    File(filename).writeText(stringToWrite + "\n")
}
