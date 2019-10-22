package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

@CompileStatic
enum Type implements JsonEnabled {
    INTEGER,
    RATIONAL,
    TEXT

    @Override
    Object toJson() {
        return name().toLowerCase()
    }
}

@CompileStatic
class Value implements JsonEnabled {
    Type type
    Object data

    Value(Type type, Object data) {
        this.type = type
        this.data = data
    }

    @Override
    Object toJson() {
        def builder = new JsonBuilder()
        builder type: type.toJson(),
                data: data
        return builder.content
    }
}

@CompileStatic
class FunctionArg extends Value {
    String name

    FunctionArg(Type type, Object data, String name = null) {
        super(type, data)
        this.name = null
    }

    @Override
    Object toJson() {
        def builder = new JsonBuilder()
        builder name: name,
                *: super.toJson()
        return builder.content
    }
}

@CompileStatic
enum FunctionType implements JsonEnabled {
    TOP,
    STATIC,
    INSTANCE,
    MAIN

    @Override
    Object toJson() {
        return name().toLowerCase()
    }
}

trait WithArguments {

    List<FunctionArg> arguments = []

    def arguments(String[] arguments) {
        arguments.each { argument(it) }
    }

    def arguments(Integer[] arguments) {
        arguments.each { argument(it) }
    }

    def arguments(Double[] arguments) {
        arguments.each { argument(it) }
    }

    def arguments(Float[] arguments) {
        arguments.each { argument(it) }
    }

    def argument(String value) {
        this.arguments << new FunctionArg(Type.TEXT, value)
    }

    def argument(double value) {
        this.arguments << new FunctionArg(Type.RATIONAL, value)
    }

    def argument(float value) {
        this.argument(value as double)
    }

    def argument(Integer value) {
        this.arguments << new FunctionArg(Type.INTEGER, value)
    }
}

@CompileStatic
class FunctionCall implements WithClosureResolver, WithEnums, WithArguments {
    FunctionType type
    String name
    String object = "Main"
    Value result

    def name(String name) {
        this.name = name
    }

    def type(String type) {
        this.type = convert(type, "type", FunctionType)
    }

    def object(String name) {
        this.object = object
    }

    Object toJson() {
        def builder = new JsonBuilder()
        builder type: type.toJson(),
                name: name,
                object: object,
                arguments: this.arguments*.toJson(),
                result: result?.toJson()
        return builder.content
    }

    def result(String text) {
        this.result = new Value(Type.TEXT, text)
    }

    def result(int value) {
        this.result = new Value(Type.INTEGER, value)
    }

    def result(double value) {
        this.result = new Value(Type.RATIONAL, value)
    }

    def result(float value) {
        this.result(value as double)
    }

    static FunctionCall main(String object, List<FunctionArg> args) {
        def call = new FunctionCall()
        call.type = FunctionType.MAIN
        call.object = object
        call.arguments = args
        return call
    }
}
