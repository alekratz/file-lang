#!/bin/sh
TEST_DIR="$(dirname "$0")"
SPLAT="$1"
EXEC="$2"

if [[ -z "$SPLAT" || -z "$EXEC" ]]; then
    echo "usage: $0 SPLAT_PATH EXEC_PATH"
    exit 1;
fi

exit_code=0
files=($(find "$TEST_DIR" -type f -name '*.fl.splat'))

for fname in "${files[@]}"; do
    printf "%-60s" "$fname"
    "$SPLAT" "$EXEC" "$fname" >/dev/null
    if [[ "$?" == "0" ]]; then
        echo "OK"
    else
        echo "ERR"
        exit_code=1
    fi
done

exit "$exit_code"
