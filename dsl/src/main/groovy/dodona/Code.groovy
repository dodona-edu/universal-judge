package dodona

import groovy.transform.CompileStatic

import java.nio.charset.StandardCharsets

/**
 * @author Niko Strijbol
 */
@CompileStatic
class Code {
    String language
    String code

    void language(String language) {
        this.language = language
    }

    void code(String code) {
        this.code = code
    }

    void file(String path) {
        this.code = new File(path).getText(StandardCharsets.UTF_8.name())
    }
}
