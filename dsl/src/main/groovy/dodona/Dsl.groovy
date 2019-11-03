package dodona

import groovy.cli.picocli.CliBuilder
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import groovy.transform.CompileDynamic
import groovy.transform.CompileStatic
import org.codehaus.groovy.control.CompilerConfiguration
import org.codehaus.groovy.runtime.typehandling.GroovyCastException

import java.nio.charset.StandardCharsets

@CompileStatic
interface JsonEnabled {
    Object toJson()
}

@CompileStatic
trait WithClosureResolver {
    /**
     * Hydrate a closure, set the resolve strategy and execute it.
     */
    @CompileStatic
    <D> D resolve(Class<D> delegate, Closure<?> closure) {
        def delegation = delegate.getDeclaredConstructor().newInstance()
        def spec = closure.rehydrate(delegation, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        return delegation
    }
}

@CompileStatic
trait WithEnums {
    def <E extends Enum<E>> E convert(String value, String name, Class<E> clazz) {
        try {
            return value.toUpperCase().asType(clazz)
        } catch (GroovyCastException | IllegalArgumentException ignored) {
            String allowed = (clazz.getEnumConstants() as Collection<E>)
                    .each { it.name().toLowerCase() }
                    .join(", ")
            throw new TestPlanException("${value} is not a valid ${name}, must be one of [${allowed}]")
        }
    }
}

/**
 * A helper that allows classes to indicate that they provide default values for children.
 */
@CompileStatic
trait WithPropagation {
    abstract List<? extends WithPropagation> children();

    List<String> offers() {
        return []
    }

    List<String> accepts() {
        return []
    }

    /**
     * Called when a parent offers a child a value.
     */
    void offered(String field, Object value) {
        if (accepts().contains(field)) {
            try {
                if (this[field] == null) {
                    this[field] = value
                }
            } catch (MissingPropertyException e) {
                println "No property with name ${field} found in ${this.class.name}!"
                e.printStackTrace()
            }
        }
    }

    void propagate() {
        // Propagate our values to our children.
        for (String offer: offers()) {
            try {
                for (WithPropagation child: this.children()) {
                    child.offered(offer, this[offer])
                }
            } catch (MissingPropertyException e) {
                println "No property with name ${offer} found in ${this.class.name}!"
                e.printStackTrace()
            }
        }
        // Let the children propagate themselves.
        this.children()*.propagate()
    }
}

@CompileStatic
class TestPlanException extends Exception {
    TestPlanException(String message) {
        super(message)
    }
}

@CompileStatic
abstract class PlanScript extends Script implements WithClosureResolver {
    List<Tab> tabs = []

    void tab(@DelegatesTo(value = Tab, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def tab = resolve(Tab, spec)
        // Check if the name is set.
        if (!tab.name?.trim()) {
            throw new TestPlanException("A tab requires a non-empty name.")
        }
        if (tab.contexts.isEmpty()) {
            throw new TestPlanException("A tab requires at least one context.")
        }
        tabs << tab
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder tabs: this.tabs.collect { it.toJson() }
        return builder.content
    }
}

@CompileStatic
class Converter {

    @CompileDynamic
    static void main(String[] args) {
        def cli = new CliBuilder()
        cli.d(type: File, longOpt: 'dsl', 'Path to DSL')
        cli.o(type: File, longOpt: 'out', 'Output path, default is stdout')
        def options = cli.parse(args)
        if (!options.d || !(options.d as File).exists()) {
            System.err.println("ERROR: Script requires valid path to DSL.")
            System.exit(-1)
        }
        String output = null
        if (options.o instanceof File) {
            output = (options.o as File).getAbsolutePath()
        }
        parseDsl((options.d as File).getAbsolutePath() as String, output)
    }

    static void parseDsl(String path, String outputPath) {
        def config = new CompilerConfiguration()
        config.scriptBaseClass = PlanScript.class.name
        def shell = new GroovyShell(this.class.classLoader, new Binding(), config)
        PlanScript script = shell.parse(new File(path)) as PlanScript
        script.run()
        def json = JsonOutput.toJson(script.toJson())
        def pretty = JsonOutput.prettyPrint(json)
        if (outputPath == null) {
            println(pretty)
        } else {
            new File(outputPath).write(pretty, StandardCharsets.UTF_8.name())
        }
    }
}
