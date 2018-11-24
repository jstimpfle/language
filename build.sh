#!/bin/sh
# Build script for Linux / Unix

mkdir -p build
cc -g -Wall -Wextra -o build/language -I src/ src/*.c
