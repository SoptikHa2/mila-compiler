#!/bin/bash
set -euo pipefail

green() {
    echo -e '\e[32m'
}
normal() {
    echo -e '\e[39m'
}
red() {
    echo -e '\e[91m'
}

defaultInput="1\n2\n3\n4\n5\n6\n7\n8\n9\n10"
okCnt=0
badCnt=0

for file in mila/samples/*; do
    if ./compile.sh "$file" /tmp/curtest >/dev/null; then
        echo "$(green)Compiled $file$(normal):"
        /tmp/curtest <<<"$defaultInput" || true
        okCnt=$((okCnt+1))
    else
        echo "$(red)Failed to compile $file$(normal)."
        badCnt=$((badCnt+1))
    fi
done

echo "$(green)Passed $okCnt$(normal)."
echo "$(red)Failed $badCnt$(normal)."
