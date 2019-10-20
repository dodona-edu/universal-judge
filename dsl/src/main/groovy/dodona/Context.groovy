package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

@CompileStatic
class Context implements WithClosureResolver {
    String description
    ExecutionTestcase execution = new ExecutionTestcase()
    List<NormalTestcase> testcases = []

    def description(String description) {
        this.description = description
    }

    def execution(@DelegatesTo(value = ExecutionTestcase, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.execution = resolve(ExecutionTestcase, cl)
    }

    def additional(@DelegatesTo(value = NormalTestcase, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.testcases << resolve(NormalTestcase, cl)
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder description: this.description,
                execution: this.execution.toJson(),
                additional: this.testcases.collect { it.toJson() }
        return builder.content
    }
}
