package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

@CompileStatic
class Tab implements WithClosureResolver {
    String name
    List<Context> contexts = []

    void name(String name) {
        this.name = name
    }

    void context(@DelegatesTo(value = Context, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def context = resolve(Context, cl)
        contexts << context
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder name: this.name,
                contexts: this.contexts.collect { it.toJson() }
        return builder.content
    }
}
