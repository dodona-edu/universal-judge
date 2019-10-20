package dodona

import groovy.json.JsonBuilder
import groovy.transform.CompileStatic

@CompileStatic
enum Type implements JsonEnabled {
    INTEGER,
    DECIMAL,
    TEXT

    @Override
    Object toJson() {
        return name().toLowerCase()
    }
}

@CompileStatic
class FunctionArg implements JsonEnabled {
    Type type
    String name
    Object data

    @Override
    Object toJson() {
        def builder = new JsonBuilder()
        builder type: type.toJson(),
                name: name,
                data: data
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

@CompileStatic
class FunctionCall implements WithClosureResolver, WithEnums {
    FunctionType type
    String name
    String object = "Main"
    List<FunctionArg> arguments = []

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
                arguments: arguments.collect { it.toJson() }
        return builder.content
    }

    def arguments(String[] arguments) {
        this.arguments.addAll(arguments.collect { s -> {
            def arg = new FunctionArg()
            arg.type = Type.TEXT
            arg.data = s
            return arg
        }})
    }

    def arguments(Integer[] arguments) {
        this.arguments.addAll(arguments.collect { s -> {
            def arg = new FunctionArg()
            arg.type = Type.INTEGER
            arg.data = s
            return arg
        }})
    }

    def arguments(Double[] arguments) {
        this.arguments.addAll(arguments.collect { s -> {
            def arg = new FunctionArg()
            arg.type = Type.DECIMAL
            arg.data = s
            return arg
        }})
    }

    def arguments(Float[] arguments) {
        this.arguments.addAll(arguments.collect { s -> {
            def arg = new FunctionArg()
            arg.type = Type.DECIMAL
            arg.data = s
            return arg
        }})
    }

    def argument(String value) {
        this.arguments([value] as String[])
    }

    def argument(Double value) {
        this.arguments([value] as Double[])
    }

    def argument(Float value) {
        this.arguments([value] as Float[])
    }

    def argument(Integer value) {
        this.arguments([value] as Integer[])
    }

    static FunctionCall main(String object, List<FunctionArg> args) {
        def call = new FunctionCall()
        call.type = FunctionType.MAIN
        call.object = object
        call.arguments = args
        return call
    }
}
