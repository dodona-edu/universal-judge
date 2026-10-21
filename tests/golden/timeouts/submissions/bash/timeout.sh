function print_greeting {
    while [ -n "$1" ]; do :; done
    echo "Hello, $1!"
}
