#!/bin/bash

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
for file in samples/*; do
    if ./MilaCompiler "$file" >/dev/null; then
        echo "[$(green)ok$(normal)] $file"
    else
        echo "[$(red)fail$(normal)] $file"
    fi
done
