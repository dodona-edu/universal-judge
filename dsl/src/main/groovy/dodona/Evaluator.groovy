package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

@CompileStatic
class Evaluator {
    String name
    String type
    String language
    List<String> options

    Evaluator(String name = "textComparator", String type = "builtin", String language = null, List<String> options = []) {
        this.name = name
        this.type = type
        this.language = language
        this.options = options
    }

    void name(String name) {
        this.name = name
    }

    void type(String type) {
        this.type = type
    }

    void language(String language) {
        this.language = language
    }

    void options(List<String> options) {
        this.options = options
    }
}

@CompileStatic
class Evaluators implements WithClosureResolver {
    Evaluator stdout = new Evaluator()
    Evaluator stderr = new Evaluator()
    Evaluator file = new Evaluator(name: "fileComparator")
    Evaluator result = new Evaluator(name: "valueComparator")

    def stdout(@DelegatesTo(value = Evaluator, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.stdout = resolve(Evaluator, cl)
    }

    def stderr(@DelegatesTo(value = Evaluator, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.stderr = resolve(Evaluator, cl)
    }

    def file(@DelegatesTo(value = Evaluator, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.file = resolve(Evaluator, cl)
    }

    def result(@DelegatesTo(value = Evaluator, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.result = resolve(Evaluator, cl)
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder stdout: stdout,
                stderr: stderr,
                file: file,
                result: result
        return builder.content
    }
}
