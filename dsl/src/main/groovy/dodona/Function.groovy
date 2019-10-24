package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

@CompileStatic
enum Type implements JsonEnabled {
    INTEGER,
    RATIONAL,
    TEXT,
    BOOLEAN

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

    def arguments(int[] arguments) {
        arguments.each { argument(it) }
    }

    def arguments(double[] arguments) {
        arguments.each { argument(it) }
    }

    def arguments(float[] arguments) {
        arguments.each { argument(it) }
    }

    def argument(boolean[] arguments) {
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

    def argument(int value) {
        this.arguments << new FunctionArg(Type.INTEGER, value)
    }

    def argument(boolean value) {
        this.arguments << new FunctionArg(Type.BOOLEAN, value)
    }
}

@CompileStatic
class FunctionCall implements WithClosureResolver, WithEnums, WithArguments {
    FunctionType type
    String name
    String object = "Main"

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
                arguments: this.arguments*.toJson()
        return builder.content
    }

    static FunctionCall main(String object, List<FunctionArg> args) {
        def call = new FunctionCall()
        call.type = FunctionType.MAIN
        call.object = object
        call.arguments = args
        return call
    }
}
