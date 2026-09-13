# Why this override exists:
# nixpkgs' ktlint wraps its jar with its own `jre_headless`, a second JDK
# closure entirely separate from the `jdk21` deps/java.toml and deps/kotlin.toml
# already pull in for javac/java/kotlinc (~590MB duplicated for nothing —
# ktlint is a CLI tool, it never needs its own JVM). Point it at the JDK
# already in the closure instead.
{ ktlint, jdk21 }:
ktlint.override { jre_headless = jdk21; }
