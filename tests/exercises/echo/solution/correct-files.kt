import java.io.File
import java.util.Scanner

fun main() {
    val sc = Scanner(System.`in`)
    val input = if (sc.hasNextLine()) sc.nextLine() else ""

    val f = File("input2.txt")
    if (f.exists()) {
        val fileTxt = f.readText().trim()
        if (fileTxt.isNotEmpty()) {
            println(fileTxt)
        } else {
            println(input)
        }
    } else {
        println(input)
    }
}
