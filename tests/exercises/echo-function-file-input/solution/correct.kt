import java.io.File

fun echoFile(content : String) : String {
    return File(content).readText()
}
