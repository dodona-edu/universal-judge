while getopts "vrn" n
do
  case "$n" in
    v) echo "Verbose" ;;
    r) echo "Recursive" ;;
    \?) usage;;
  esac
done
