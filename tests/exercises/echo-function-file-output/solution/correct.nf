process echoFunction() {
    input:
    val x1
    val x2

    """
    echo "${x2}" > "${projectDir}/${x1}"
    """
}
