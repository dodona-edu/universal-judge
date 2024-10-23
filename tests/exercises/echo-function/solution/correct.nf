process echo {
    input:
    val x

    output:
    stdout

    """
    echo -n '${x}'
    """
}
