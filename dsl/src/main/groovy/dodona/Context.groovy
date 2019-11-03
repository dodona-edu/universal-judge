package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

@CompileStatic
class Context implements WithClosureResolver {
    String description
    ExecutionTestcase execution = new ExecutionTestcase()
    List<NormalTestcase> testcases = []
    Map<String, String> before = new HashMap<>()
    Map<String, String> after = new HashMap<>()

    def description(String description) {
        this.description = description
    }

    def execution(@DelegatesTo(value = ExecutionTestcase, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.execution = resolve(ExecutionTestcase, cl)
    }

    def additional(@DelegatesTo(value = NormalTestcase, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.testcases << resolve(NormalTestcase, cl)
    }

    def before(String language, String code) {
        before.put(language, code)
    }

    def after(String language, String code) {
        after.put(language, code)
    }

    def before(@DelegatesTo(value = Code, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        Code result = resolve(Code, cl)
        before.put(result.language, result.code)
    }

    def after(@DelegatesTo(value = Code, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        Code result = resolve(Code, cl)
        after.put(result.language, result.code)
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder description: this.description,
                execution: this.execution.toJson(),
                additional: this.testcases.collect { it.toJson() },
                before: this.before,
                after: this.after
        return builder.content
    }
}
