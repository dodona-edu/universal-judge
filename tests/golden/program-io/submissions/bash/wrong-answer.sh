if [ $# -eq 0 ]; then
    exit 1
fi

prefix=$1
shift
for name in "$@"; do
    echo "$prefix: $name"
done
while read -r line; do
    echo "$line"
done
