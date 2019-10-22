package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

@CompileStatic
enum DataType implements JsonEnabled {
    TEXT, FILE

    @Override
    String toJson() {
        return name().toLowerCase()
    }
}

@CompileStatic
enum InputChannelState implements JsonEnabled {
    NONE

    @Override
    String toJson() {
        return name().toLowerCase()
    }
}

@CompileStatic
enum OutputChannelState implements JsonEnabled {
    NONE, IGNORED

    @Override
    String toJson() {
        return name().toLowerCase()
    }
}


@CompileStatic
class ChannelData implements WithEnums, JsonEnabled {
    String data
    DataType type = DataType.TEXT

    void data(String data) {
        this.data = data
    }

    void data(List<String> data) {
        this.data = data.join("\n")
    }

    def type(String type) {
        this.type = convert(type, "data type", DataType)
    }

    @Override
    Object toJson() {
        def builder = new JsonBuilder()
        builder data: this.data,
                type: this.type.toJson()
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
abstract class BaseInput implements WithClosureResolver, WithEnums {
    JsonEnabled stdin = InputChannelState.NONE

    def stdin(String state) {
        def channelData = new ChannelData()
        channelData.data(state)
        this.stdin = channelData
    }

    def stdin(List<String> state) {
        def channelData = new ChannelData()
        channelData.data(state)
        this.stdin = channelData
    }

    def specialStdin(String state) {
        stdin = convert(state, "stdin", InputChannelState)
    }

    def stdin(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def channelData = resolve(ChannelData, cl)
        if (channelData.data.isEmpty()) {
            throw new TestPlanException("stdin requires non-empty property data")
        }
        stdin = channelData
    }

    Object toJson() {
        JsonBuilder builder = new JsonBuilder()
        builder stdin: this.stdin.toJson()
        return builder.content
    }
}

@CompileStatic
class MainFunctionCallClosure implements WithArguments {
    String object = "Main"

    def object(String object) {
        this.object = object
    }
}

@CompileStatic
class MainInput extends BaseInput {

    FunctionCall main = FunctionCall.main("Main", new ArrayList<FunctionArg>())

    def main(@DelegatesTo(value = MainFunctionCallClosure, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def mainCall = resolve(MainFunctionCallClosure, cl)
        main = FunctionCall.main(mainCall.object, mainCall.arguments)
    }

    def main(String objectName) {
        main = FunctionCall.main(objectName, new ArrayList<FunctionArg>())
    }

    Object toJson() {
        JsonBuilder builder = new JsonBuilder()
        builder function: main.toJson(),
                *: super.toJson()
        return builder.content
    }
}

@CompileStatic
class NormalInput extends BaseInput {

    FunctionCall function

    def function(@DelegatesTo(value = FunctionCall, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        this.function = resolve(FunctionCall, cl)
    }

    Object toJson() {
        JsonBuilder builder = new JsonBuilder()
        builder function: function.toJson(),
                *: super.toJson()
        return builder.content
    }
}

@CompileStatic
class Output implements WithClosureResolver, WithEnums {
    JsonEnabled stdout = OutputChannelState.IGNORED
    JsonEnabled stderr = OutputChannelState.NONE
    FileOutput file

    def stdout(List<String> data) {
        def channelData = new ChannelData()
        channelData.data(data)
        this.stdout = channelData
    }

    def stdout(String data) {
        def channelData = new ChannelData()
        channelData.data(data)
        this.stdout = channelData
    }

    def stderr(List<String> data) {
        def channelData = new ChannelData()
        channelData.data(data)
        this.stderr = channelData
    }

    def stderr(String data) {
        def channelData = new ChannelData()
        channelData.data(data)
        this.stderr = channelData
    }

    def specialStdout(String state) {
        this.stdout = convert(state, "stdout", OutputChannelState)
    }

    def specialStderr(String state) {
        this.stderr = convert(state, "stderr", OutputChannelState)
    }

    def stderr(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def channelData = resolve(ChannelData, cl)
        if (channelData.data.isEmpty()) {
            throw new TestPlanException("stderr requires non-empty property data")
        }
        stderr = channelData
    }

    def stdout(@DelegatesTo(value = ChannelData, strategy = Closure.DELEGATE_FIRST) Closure<?> cl) {
        def channelData = resolve(ChannelData, cl)
        if (channelData.data.isEmpty()) {
            throw new TestPlanException("stdout requires non-empty property daa")
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
        builder stdout: this.stdout.toJson(),
                stderr: this.stderr.toJson(),
                file: this.file
        return builder.content
    }
}
