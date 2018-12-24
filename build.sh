#!/bin/sh
# Build script for Linux / Unix

mkdir -p build
cc -std=c99 -g -Wall -Wextra -o build/language -I include/ src/*.c
