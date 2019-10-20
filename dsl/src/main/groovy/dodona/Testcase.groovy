package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

/**
 * Parent testcase. All implementers should provide their own input field, which is required.
 */
@CompileStatic
abstract class AbstractTestcase implements WithClosureResolver {
    String description
    Output output = new Output()
    Evaluators evaluators = new Evaluators()

    def description(String description) {
        this.description = description
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
                evaluators: this.evaluators.toJson(),
                input: input.toJson()
        return builder.content
    }

    protected abstract BaseInput getInput();
}

@CompileStatic
class ExecutionTestcase extends AbstractTestcase {
    MainInput input = new MainInput()

    def input(@DelegatesTo(value = MainInput, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.input = resolve(MainInput, cl)
    }

    def input(String value) {
        def channel = new ChannelData()
        channel.data(value)
        this.input = new MainInput()
        this.input.stdin = channel
    }
}

@CompileStatic
class NormalTestcase extends AbstractTestcase {
    NormalInput input

    def input(@DelegatesTo(value = NormalInput, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def input = resolve(NormalInput, cl)
        if (!input.function) {
            throw new TestPlanException("Normal testcases require a function class as input")
        }
        this.input = input
    }
}
