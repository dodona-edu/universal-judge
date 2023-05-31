import java.io.PrintWriter
import java.io.StringWriter
import java.math.BigDecimal
import java.math.BigInteger

private fun convertMessage(message: EvaluationResult.Message): String {
    val builder = StringBuilder(64).append('{')
            .append("\"description\": \"").append(message.description)
            .append("\", ")
            .append("\"format\": \"").append(message.format).append("\"")
    if (message.permission != null) {
        builder.append(", \"permission\": \"").append(message.permission)
                .append('\"')
    }
    return builder.append('}').toString()
}

private fun escape(str: String): String {
    return str
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\b", "\\b")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t")
}

private fun encodeSequence(objects: Iterable<Any?>): String {
    return objects.asSequence()
            .joinToString(
                    separator = ", ",
                    prefix = "[",
                    postfix = "]",
                    transform = { o -> encode(o) }
            )
}

private fun encode(value: Any?): String {
    val typeAndData = internalEncode(value)
    if (typeAndData[2] != null) {
        return String.format("{ \"data\": %s, \"type\": \"%s\", \"diagnostic\": \"%s\"}", typeAndData[1], typeAndData[0], typeAndData[2])
    } else {
        return String.format("{ \"data\": %s, \"type\": \"%s\"}", typeAndData[1], typeAndData[0])
    }
}

private fun internalEncode(value: Any?): Array<String?> {
    val type: String
    val data: String
    var diagnostic: String? = null

    if (value == null) {
        type = "nothing"
        data = "null"
    } else if (value is Boolean) {
        type = "boolean"
        data = value.toString()
    } else if (value.javaClass.isArray) {
        type = "array"
        data = encodeSequence((value as Array<*>).asIterable())
    } else if (value is BigInteger) {
        type = "bigint"
        data = value.toString()
    } else if (value is BigDecimal) {
        type = "fixed_precision"
        data = value.toString()
    } else if (value is UByte) {
        type = "uint8"
        data = value.toString()
    } else if (value is Byte) {
        type = "int8"
        data = value.toString()
    } else if (value is UShort) {
        type = "uint16"
        data = value.toString()
    } else if (value is Short) {
        type = "int16"
        data = value.toString()
    } else if (value is UInt) {
        type = "uint32"
        data = value.toString()
    } else if (value is Int) {
        type = "int32"
        data = value.toString()
    } else if (value is ULong) {
        type = "uint64"
        data = value.toString()
    } else if (value is Long) {
        type = "int64"
        data = value.toString()
    } else if (value is Float) {
        type = "single_precision"
        if (value.isNaN()) {
            data = "\"nan\"";
        } else if (!value.isInfinite()) {
            data = value.toString();
        } else if (value < 0.0) {
            data = "\"-inf\"";
        } else {
            data = "\"inf\"";
        }
    } else if (value is Double) {
        type = "double_precision"
        if (value.isNaN()) {
            data = "\"nan\"";
        } else if (!value.isInfinite()) {
            data = value.toString();
        } else if (value < 0.0) {
            data = "\"-inf\"";
        } else {
            data = "\"inf\"";
        }
    } else if (value is Char) {
        type = "char"
        data = String.format("\"%s\"", escape(value.toString()))
    } else if (value is CharSequence) {
        type = "text"
        data = String.format("\"%s\"", escape(value.toString()))
    } else if (value is List<*>) {
        type = "list"
        data = encodeSequence(value.asIterable())
    } else if (value is Set<*>) {
        type = "set"
        data = encodeSequence(value.asIterable())
    } else if (value is Map<*, *>) {
        type = "map"
        data = value.asSequence()
                .map { e ->
                    String.format("{\"key\": %s, \"value\": %s }", encode(e.key),
                            encode(e.value))
                }
                .joinToString(separator = ", ", prefix = "[", postfix = "]")
    } else {
        type = "unknown"
        data = String.format("\"%s\"", escape(value.toString()))
        diagnostic = value::class.simpleName
    }

    return arrayOf(type, data, diagnostic)
}

fun evaluated(writer: PrintWriter, result: Boolean, expected: String?,
              actual: String?, messages: Iterable<EvaluationResult.Message>) {
    val builder = StringBuilder(64).append('{')
            .append("\"result\": \"").append(result).append("\", ")
            .append("\"readable_expected\": \"").append(expected).append("\", ")
            .append("\"readable_actual\": \"").append(actual).append("\", ")
            .append("\"messages\": [").append(messages.joinToString(separator = ", ",
                    transform = { m -> convertMessage(m) })).append("]}")
    writer.print(builder.toString())
}

fun valuesSend(writer: PrintWriter, value: Any?): Unit = writer.print(encode(value))

fun valuesSendEvaluated(writer: PrintWriter, result: EvaluationResult) {
    evaluated(writer, result.result, result.readableExpected, result.readableActual,
            result.messages)
}

fun valuesSendException(writer: PrintWriter, throwable: Throwable?) {
    if (throwable == null) {
        return
    }
    val strStackTraceWriter = StringWriter()
    throwable.printStackTrace(PrintWriter(strStackTraceWriter))
    writer.printf("{ \"message\": \"%s\", \"stacktrace\": \"%s\", \"type\": \"%s\"}",
            escape(throwable.message ?: ""),
            escape(strStackTraceWriter.toString()),
            throwable::class.simpleName
        )
}
