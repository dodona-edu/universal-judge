process readfile {
    input:
    path p
    output:
    stdout

    """
    cat "${p}"
    """
}

workflow {
    readfile(${params.p1}) | view(newLine: false)
}
