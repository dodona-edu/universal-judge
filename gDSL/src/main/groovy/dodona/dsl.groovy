package dodona

import groovy.cli.picocli.CliBuilder
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import groovy.transform.CompileDynamic
import groovy.transform.CompileStatic
import org.codehaus.groovy.control.CompilerConfiguration

@CompileStatic
trait WithClosureResolver {

    /**
     * Hydrate a closure, set the resolve strategy and execute it.
     *
     * @param delegate
     * @param _this
     * @param closure
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
class ChannelData {
    final static Set ALLOWED = ['text', 'file'] as Set
    List<String> data = new ArrayList<>()
    String type = 'text'

    void data(String data) {
        this.data = List.of(data)
    }

    void data(List<String> data) {
        this.data = data
    }

    def type(String type) {
        if (!ALLOWED.contains(type)) {
            throw new TestPlanException("Channel type is $type, must be one of $ALLOWED")
        }
        this.type = type
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder data: this.data, type: this.type
        return builder.content
    }
}

@CompileStatic
class RunArgs {
    String classname = "Main"

    def classname(String name) {
        this.classname = name
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder classname: this.classname
        return builder.content
    }
}

@CompileStatic
class Evaluator {
    String name
    String type
    String language
    List<String> options

    Evaluator(String name = "textComparator", String type = "builtin", String language = "", List<String> options = new ArrayList<>()) {
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

    def stdout(@DelegatesTo(value = Evaluator, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def evaluator = resolve(Evaluator, cl)
        this.stdout = evaluator
    }

    def stderr(@DelegatesTo(value = Evaluator, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def evaluator = resolve(Evaluator, cl)
        this.stderr = evaluator
    }

    def file(@DelegatesTo(value = Evaluator, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def evaluator = resolve(Evaluator, cl)
        this.file = evaluator
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder stdout: this.stdout,
                stderr: this.stderr,
                file: this.file
        return builder.content
    }
}

@CompileStatic
trait WithRunArgs implements WithClosureResolver {
    RunArgs main

    def main(@DelegatesTo(value = RunArgs, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def runArgs = resolve(RunArgs, cl)
        main = runArgs
    }
}

@CompileStatic
class Input implements WithClosureResolver {
    final static Set ALLOWED = ['none'] as Set
    def stdin = 'none'

    def stdin(String state) {
        if (ALLOWED.contains("none")) {
            throw new TestPlanException("Stdin cannot be $state, must be one of $ALLOWED")
        }
        stdin = state
    }

    def stdin(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def channelData = resolve(ChannelData, cl)
        if (channelData.data.isEmpty() || !channelData.type?.trim()) {
            throw new TestPlanException("stdin requires non-empty properties type and text")
        }
        stdin = channelData
    }

    Object toJson() {
        JsonBuilder builder = new JsonBuilder()
        builder stdin: this.stdin instanceof String ? this.stdin : (this.stdin as ChannelData).toJson()
        return builder.content
    }
}

@CompileStatic
class FileOutput {
    String expected
    String actual

    def expected(String expected) {
        this.expected = expected
    }

    def actual(String actual) {
        this.actual = actual
    }
}

@CompileStatic
class Output implements WithClosureResolver {
    final static Set<String> ALLOWED = Set.of('none', 'ignored')
    def stdout = 'ignored'
    def stderr = 'none'
    FileOutput file

    def stdout(String state) {
        if (!ALLOWED.contains(state)) {
            throw new TestPlanException("Output channel stdout is $state, must be one of $ALLOWED")
        }
        this.stdout = state
    }

    def stderr(String state) {
        if (!ALLOWED.contains(state)) {
            throw new TestPlanException("Output channel stderr is $state, must be one of $ALLOWED")
        }
        this.stderr = state
    }

    def stderr(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def channelData = resolve(ChannelData, cl)
        if (channelData.data.isEmpty() || !channelData.type?.trim()) {
            throw new TestPlanException("stderr requires non-empty properties type and text")
        }
        stderr = channelData
    }

    def stdout(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def channelData = resolve(ChannelData, cl)
        if (channelData.data.isEmpty() || !channelData.type?.trim()) {
            throw new TestPlanException("stdout requires non-empty properties type and text")
        }
        stdout = channelData
    }

    def file(@DelegatesTo(value = FileOutput, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def fileOutput = resolve(FileOutput, cl)
        if (!fileOutput.expected?.trim() || !fileOutput.actual?.trim()) {
            throw new TestPlanException("file output requires non-empty properties expected and actual")
        }
        file = fileOutput
    }

    Object toJson() {
        JsonBuilder builder = new JsonBuilder()
        builder stdout: this.stdout instanceof String ? this.stdout : (this.stdout as ChannelData).toJson(),
                stderr: this.stderr instanceof String ? this.stderr : (this.stderr as ChannelData).toJson(),
                file: this.file
        return builder.content
    }
}

@CompileStatic
class Test implements WithClosureResolver {
    String description
    Output output
    Evaluators evaluators = new Evaluators()

    String description(String string) {
        this.description = string
    }

    def output(@DelegatesTo(value = Output, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def output = resolve(Output, cl)
        if (!output.stdout || !output.stderr) {
            throw new TestPlanException("Test requires stdout and stderr")
        }
        this.output = output
    }

    def output(String value) {
        // Create one ourselves.
        def channel = new ChannelData()
        channel.data(value)
        this.output = new Output()
        this.output.stdout = channel
    }

    def evaluators(@DelegatesTo(value = Evaluators, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.evaluators = resolve(Evaluators, cl)
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder description: this.description,
                output: this.output.toJson(),
                evaluators: this.evaluators.toJson()
        return builder.content
    }

}

@CompileStatic
class Testcase implements WithClosureResolver {
    String description
    List<Test> tests = []
    Input input

    def description(String description) {
        this.description = description
    }

    def input(@DelegatesTo(value = Input, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def input = resolve(Input, cl)
        if (!input.stdin) {
            throw new TestPlanException("Test requires stdin")
        }
        this.input = input
    }

    def input(String value) {
        def channel = new ChannelData()
        channel.data(value)
        this.input = new Input()
        this.input.stdin = channel
    }

    def test(@DelegatesTo(value = Test, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def test = resolve(Test, cl)
        if (!test.output) {
            throw new TestPlanException("Test requires description and output!")
        }
        tests << test
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder description: this.description,
                input: this.input.toJson(),
                tests: this.tests.collect { it.toJson() }
        return builder.content
    }
}

@CompileStatic
class Consummation extends Testcase implements WithRunArgs, WithPropagation {
    Object toJson() {
        def builder = new JsonBuilder()
        builder main: this.main.toJson(),
                *:super.toJson()
        return builder.content
    }

    @Override
    List<? extends WithPropagation> children() {
        return []
    }

    @Override
    List<String> accepts() {
        return ["main"]
    }
}

@CompileStatic
class Context implements WithRunArgs, WithPropagation {
    String description
    String preparation
    Consummation consummation
    List<Testcase> testcases = []

    def description(String description) {
        this.description = description
    }

    def consummation(@DelegatesTo(value = Consummation, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def consummation = resolve(Consummation, cl)
        if (consummation.tests.empty) {
            println "Consummation has no tests."
        }
        this.consummation = consummation
    }

    def postprocess(@DelegatesTo(value = Testcase, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def testcase = resolve(Testcase, cl)
        if (testcase.tests.empty) {
            println "Consummation has no tests."
        }
        this.testcases << testcase
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder description: this.description,
                consummation: this.consummation.toJson(),
                postprocessing: this.testcases.collect { it.toJson() }
        return builder.content
    }

    @Override
    List<String> accepts() {
        return ["main"]
    }

    @Override
    List<String> offers() {
        return ["main"]
    }

    @Override
    List<? extends WithPropagation> children() {
        return consummation == null ? [] : [consummation]
    }
}

@CompileStatic
class Tab implements WithRunArgs, WithPropagation {
    String name
    List<Context> contexts = []

    void name(String name) {
        this.name = name
    }

    void context(@DelegatesTo(value = Context, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def context = resolve(Context, cl)
        if (context.testcases.isEmpty() && context.consummation == null) {
            throw new TestPlanException("Both consummation and postprocesses are null; not sure what to do!")
        }
        contexts << context
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder name: this.name,
                contexts: this.contexts.collect { it.toJson() }
        return builder.content
    }

    @Override
    List<String> accepts() {
        return ["main"]
    }

    @Override
    List<String> offers() {
        return ["main"]
    }

    @Override
    List<Context> children() {
        return this.contexts
    }
}

@CompileStatic
class TestPlanException extends Exception {
    TestPlanException(String message) {
        super(message)
    }
}

@CompileStatic
class Plan implements WithRunArgs, WithPropagation {
    List<Tab> tabs = new ArrayList<>()

    // Default runArgs.
    RunArgs main = new RunArgs()

    void tab(@DelegatesTo(value = Tab, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def tab = resolve(Tab, spec)
        // Check if the name is set.
        if (!tab.name?.trim()) {
            throw new TestPlanException("Tabs require a name.")
        }
        tabs << tab
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder tabs: this.tabs.collect { it.toJson() }
        return builder.content
    }

    @Override
    List<String> offers() {
        return ["main"]
    }

    @Override
    List<? extends WithPropagation> children() {
        return tabs
    }
}

@CompileStatic
abstract class PlanScript extends Script implements WithClosureResolver {
    Plan plan

    void plan(@DelegatesTo(value = Plan, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        if (this.plan != null) {
            throw new IllegalStateException("You can only have one plan per file!")
        }
        this.plan = resolve(Plan, cl)
        // Propagate defaults to children.
        this.plan.propagate()
    }
}

@CompileStatic
class Converter {

    @CompileDynamic
    static void main(String[] args) {
        def cli = new CliBuilder()
        cli.d(type: File, longOpt: 'dsl', 'Path to DSL')
        def options = cli.parse(args)
        if (!options.d || !(options.d as File).exists()) {
            System.err.println("ERROR: Script requires valid path to DSL.")
            System.exit(-1)
        }
        parseDsl((options.d as File).getAbsolutePath() as String)
    }

    static void parseDsl(String path) {
        def config = new CompilerConfiguration()
        config.scriptBaseClass = PlanScript.class.name
        def shell = new GroovyShell(this.class.classLoader, new Binding(), config)
        PlanScript script = shell.parse(new File(path)) as PlanScript
        script.run()
        def json = JsonOutput.toJson(script.plan.toJson())
        println(JsonOutput.prettyPrint(json))
    }
}
