process my_echo {
    input:
    val x
    output:
    stdout

    """
    echo ${x}
    """
}
