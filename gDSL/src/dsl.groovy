class ChannelData {
    final static Set ALLOWED = ['text', 'file']
    String data
    String type = 'text'

    def data(String data) {
        this.data = data
    }

    def type(String type) {
        if (!ALLOWED.contains(type)) {
            throw new TestPlanException("Channel type is $type, must be one of $ALLOWED")
        }
        this.type = type
    }
}

class Input {
    final static Set ALLOWED = ['none']
    def stdin = 'none'

    def stdin(String state) {
        if (ALLOWED.contains("none")) {
            throw new TestPlanException("Stdin cannot be $state, must be one of $ALLOWED")
        }
        stdin = state
    }

    def stdin(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def channelData = new ChannelData()
        spec.rehydrate(channelData, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!channelData.data?.trim() || !channelData.type?.trim()) {
            throw new TestPlanException("stdin requires non-empty properties type and text")
        }
        stdin = channelData
    }
}

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

class Output {
    final static Set ALLOWED = ['none', 'ignored']
    def stdout = 'none'
    def stderr = 'ignored'
    FileOutput file

    def stdout(String state) {
        if (!ALLOWED.contains(state)) {
            throw new TestPlanException("Output channel stdout is $type, must be one of $ALLOWED")
        }
        this.stdout = state
    }

    def stderr(String state) {
        if (!ALLOWED.contains(state)) {
            throw new TestPlanException("Output channel stderr is $type, must be one of $ALLOWED")
        }
        this.stderr = state
    }

    def stderr(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def channelData = new ChannelData()
        spec.rehydrate(channelData, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!channelData.data?.trim() || !channelData.type?.trim()) {
            throw new TestPlanException("stderr requires non-empty properties type and text")
        }
        stderr = channelData
    }

    def stdout(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def channelData = new ChannelData()
        spec.rehydrate(channelData, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!channelData.data?.trim() || !channelData.type?.trim()) {
            throw new TestPlanException("stdout requires non-empty properties type and text")
        }
        stdout = channelData
    }

    def file(@DelegatesTo(value = FileOutput, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def fileOutput = new FileOutput()
        spec.rehydrate(fileOutput, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!fileOutput.expected?.trim() || !fileOutput.actual?.trim()) {
            throw new TestPlanException("file output requires non-empty properties expected and actual")
        }
        file = fileOutput
    }
}

class Test {
    String description
    Input input
    Output output

    def description(String string) {
        this.description = string
    }

    def input(@DelegatesTo(value = Input, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def input = new Input()
        spec.rehydrate(input, this, this)
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

    def output(@DelegatesTo(value = Output, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def output = new Output()
        spec.rehydrate(output, this, this)
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

}

class Testcase {
    String description
    List<Test> tests = new ArrayList<>()

    def description(String description) {
        this.description = description
    }

    def test(@DelegatesTo(value = Test, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def test = new Test()
        spec.rehydrate(test, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!test.description?.trim() || !test.input || !test.output) {
            throw new TestPlanException("Test requires description, input and output!")
        }
        tests << test
    }
}

class Context {
    String description
    List<Testcase> testcases = new ArrayList<>()

    def description(String description) {
        this.description = description
    }

    def testcase(@DelegatesTo(value = Testcase, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def testcase = new Testcase()
        spec.rehydrate(testcase, this, this)
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
}

class Tab {
    String name
    List<Context> contexts = new ArrayList<>()

    void name(String name) {
        this.name = name
    }

    def context(@DelegatesTo(value = Context, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        def context = new Context()
        spec.rehydrate(context, this, this)
        spec.resolveStrategy = Closure.DELEGATE_FIRST
        spec()
        if (!context.testcases.isEmpty()) {
            throw new TestPlanException("Context requires testcases!")
        }
        contexts << context
    }

    def test(@DelegatesTo(value = Test, strategy = Closure.DELEGATE_FIRST) Closure<?> spec) {
        // Create test case and use that.
        def testcase = new Testcase()
        testcase.test(spec)
        testcase.description = testcase.tests[0].description
        def context = new Context()
        context.testcases << testcase
        contexts << context
    }
}

class TestPlanException extends Exception {
    TestPlanException(String message) {
        super(message)
    }
}

class Plan {
    List<Tab> tabs = new ArrayList<>()

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
}


def static plan(@DelegatesTo(value = Plan, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
    def plan = new Plan()
    def code = cl.rehydrate(plan, dsl, dsl)
    code.resolveStrategy = Closure.DELEGATE_FIRST
    print(code())
}


// Test DSL

plan {

    tab {
        name "hallo"

        test {
            input "Hallo"
            input {
                stdin {
                    data "Hallo"
                    type "text"
                }
            }
        }

    }

    tab {

    }

    tab {

    }

}
