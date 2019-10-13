import groovy.cli.picocli.CliBuilder
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import groovy.transform.CompileDynamic
import groovy.transform.CompileStatic
import org.codehaus.groovy.control.CompilerConfiguration

@CompileStatic
class ChannelData {
    final static Set ALLOWED = ['text', 'file'] as Set
    String data
    String type = 'text'

    String data(String data) {
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
trait WithRunArgs {
    RunArgs runArgs

    def runArgs(@DelegatesTo(value = RunArgs, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def runArgs = new RunArgs()
        def spec = cl.rehydrate(runArgs, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        this.runArgs = runArgs
    }
}

@CompileStatic
class Input {
    final static Set ALLOWED = ['none'] as Set
    def stdin = 'none'

    def stdin(String state) {
        if (ALLOWED.contains("none")) {
            throw new TestPlanException("Stdin cannot be $state, must be one of $ALLOWED")
        }
        stdin = state
    }

    def stdin(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def channelData = new ChannelData()
        def spec = cl.rehydrate(channelData, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!channelData.data?.trim() || !channelData.type?.trim()) {
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
class Output {
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
        def channelData = new ChannelData()
        def spec = cl.rehydrate(channelData, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!channelData.data?.trim() || !channelData.type?.trim()) {
            throw new TestPlanException("stderr requires non-empty properties type and text")
        }
        stderr = channelData
    }

    def stdout(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def channelData = new ChannelData()
        def spec = cl.rehydrate(channelData, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!channelData.data?.trim() || !channelData.type?.trim()) {
            throw new TestPlanException("stdout requires non-empty properties type and text")
        }
        stdout = channelData
    }

    def file(@DelegatesTo(value = FileOutput, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def fileOutput = new FileOutput()
        def spec = cl.rehydrate(fileOutput, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
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
class Test implements WithRunArgs {
    String description
    Input input
    Output output

    String description(String string) {
        this.description = string
    }

    def input(@DelegatesTo(value = Input, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def input = new Input()
        def spec = cl.rehydrate(input, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!input.stdin) {
            throw new TestPlanException("Test requires stdin")
        }
        this.input = input
    }

    def input(String value) {
        // Create one ourselves.
        def channel = new ChannelData()
        channel.data(value)
        this.input = new Input()
        this.input.stdin = channel
    }

    def output(@DelegatesTo(value = Output, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def output = new Output()
        def spec = cl.rehydrate(output, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
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

    Object toJson() {
        def builder = new JsonBuilder()
        builder description: this.description,
                input: this.input.toJson(),
                output: this.output.toJson(),
                runArgs: this.runArgs.toJson()
        return builder.content
    }

}

@CompileStatic
class Testcase implements WithRunArgs {
    String description = ""
    List<Test> tests = new ArrayList<>()

    def description(String description) {
        this.description = description
    }

    def test(@DelegatesTo(value = Test, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def test = new Test()
        def spec = cl.rehydrate(test, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!test.input || !test.output) {
            throw new TestPlanException("Test requires description, input and output!")
        }
        // Add description if it doesn't exist.
        if (!test.description) {
            if (test.input instanceof Input) {
                if (test.input.stdin instanceof ChannelData) {
                    def input = test.input.stdin as ChannelData
                    test.description(input.data)
                } else {
                    test.description(test.input.stdin as String)
                }
            } else if (test.input instanceof String) {
                test.description(test.input as String)
            } else {
                throw new AssertionError()
            }
        }

        tests << test
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder description: this.description,
                tests: this.tests.collect { it.toJson() }
        return builder.content
    }
}

@CompileStatic
class Context implements WithRunArgs {
    String description
    List<Testcase> testcases = new ArrayList<>()

    def description(String description) {
        this.description = description
    }

    def testcase(@DelegatesTo(value = Testcase, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def testcase = new Testcase()
        def spec = cl.rehydrate(testcase, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!testcase.description?.trim() || testcase.tests.isEmpty()) {
            throw new TestPlanException("Testcase requires description and tests!")
        }
        testcases << testcase
    }

    def test(@DelegatesTo(value = Test, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        // Create test case and use that.
        def testcase = new Testcase()
        testcase.test(spec)
        testcase.description = testcase.tests[0].description
        testcases << testcase
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder description: this.description,
                testcases: this.testcases.collect { it.toJson() }
        return builder.content
    }
}

@CompileStatic
class Tab implements WithRunArgs {
    String name
    List<Context> contexts = new ArrayList<>()

    void name(String name) {
        this.name = name
    }

    void context(@DelegatesTo(value = Context, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def context = new Context()
        def spec = cl.rehydrate(context, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (context.testcases.isEmpty()) {
            throw new TestPlanException("Context requires testcases!")
        }
        contexts << context
    }

    void test(@DelegatesTo(value = Test, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        // Create test case and use that.
        def testcase = new Testcase()
        testcase.test(spec)
        testcase.description = testcase.tests[0].description
        def context = new Context()
        context.testcases << testcase
        contexts << context
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder name: this.name,
                contexts: this.contexts.collect { it.toJson() }
        return builder.content
    }
}

@CompileStatic
class TestPlanException extends Exception {
    TestPlanException(String message) {
        super(message)
    }
}

@CompileStatic
class Plan implements WithRunArgs {
    List<Tab> tabs = new ArrayList<>()

    // Default runArgs
    RunArgs runArgs = new RunArgs()

    void tab(@DelegatesTo(value = Tab, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def tab = new Tab()
        def code = spec.rehydrate(tab, this, this)
        code.resolveStrategy = Closure.DELEGATE_FIRST
        code()
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
}

@CompileStatic
abstract class PlanScript extends Script {

    Plan plan

    void plan(@DelegatesTo(value = Plan, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def plan = new Plan()
        def code = cl.rehydrate(plan, this, this)
        code.resolveStrategy = Closure.DELEGATE_FIRST
        code()
        this.plan = plan

        // Move general items to children.
        for (Tab tab: plan.tabs) {
            if (!tab.runArgs) {
                tab.runArgs = plan.runArgs
            }
            for (Context context: tab.contexts) {
                if (!context.runArgs) {
                    context.runArgs = tab.runArgs
                }
                for (Testcase testcase: context.testcases) {
                    if (!testcase.runArgs) {
                        testcase.runArgs = context.runArgs
                    }
                    for (Test test: testcase.tests) {
                        if (!test.runArgs) {
                            test.runArgs = testcase.runArgs
                        }
                    }
                }
            }
        }
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
