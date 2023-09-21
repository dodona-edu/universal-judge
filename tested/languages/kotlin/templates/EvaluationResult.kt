class EvaluationResult private constructor(
        val result: Boolean,
        val readableExpected: String?,
        val readableActual: String?,
        val dslExpected: String?,
        val dslActual: String?,
        val messages: List<Message>
) {
    class Message(val description: String,
                  val format: String = "text",
                  val permission: String? = null)

    class Builder(
            private val result: Boolean,
            private var readableActual: String? = null,
            private var readableExpected: String? = null,
            private var dslActual: String? = null,
            private var dslExpected: String? = null
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

        fun withDslActual(dslActual: String?): Builder {
            this.dslActual = dslActual
            return this
        }

        fun withDslExpected(dslExpected: String?): Builder {
            this.dslExpected = dslExpected
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
                    readableActual, dslExpected, dslActual, messages)
        }
    }
}
