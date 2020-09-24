class EvaluationResult private constructor(
        val result: Boolean,
        val readableExpected: String?,
        val readableActual: String?,
        val messages: List<Message>
) {
    class Message(val description: String,
                  val format: String = "text",
                  val permission: String? = null)

    class Builder private constructor(
            private val result: Boolean,
            private var readableActual: String? = null,
            private var readableExpected: String? = null
    ) {
        private val messages: MutableList<Message> = ArrayList()

        fun withReadableActual(readableActual: String?): Builder {
            this.readableActual = readableActual
            return this
        }

        fun withReadableExpected(readableExpected: String?): Builder {
            this.readableExpected = readableExpected
            return this
        }

        fun withMessages(messages: List<Message>): Builder {
            this.messages += messages
            return this
        }

        fun withMessage(message: Message): Builder {
            this.messages += message
            return this
        }

        fun build(): EvaluationResult {
            return EvaluationResult(result, readableExpected,
                    readableActual, messages)
        }
    }
}
