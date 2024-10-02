process echoFile {
    input:
    val x
    output:
    stdout

    script:
    p = file(x)

    """
    cat "${p}"
    """
}
