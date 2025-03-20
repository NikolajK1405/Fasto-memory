#!/usr/bin/env sh

set -e # Exit on first error.

if [ "$1" = "-r" ]; then
    run=true
    shift
else
    run=false
fi

base_dir="$(dirname "$0")"
prog_input="$1"
input_name=$(basename "$prog_input" .c)

# Compile .c file into .s
clang -DDEBUG=0 --target=riscv32 -S "$base_dir/../memory/$prog_input" -o  "$base_dir/../memory/${input_name}.s"

# Cleanup for rars
"$base_dir/../memory/cleanupRV.fsx" "$base_dir/../memory/${input_name}.s" "$base_dir/../memory/${input_name}clean.s"

# Run rars if given -r flag
if [ "$run" = true ]; then
    "$base_dir/../bin/rars.sh" "$base_dir/../memory/${input_name}clean.s"
fi
