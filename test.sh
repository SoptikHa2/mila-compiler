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

echo "Testing parsing..."
cd mila || exit 1
for file in samples/*; do
    echo "$file"
    if stack run -- "$file" >/dev/null; then
        echo "[$(green)ok$(normal)] $file"
    else
        echo "[$(red)fail$(normal)] $file"
    fi
done
