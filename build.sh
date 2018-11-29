#!/bin/sh
# Build script for Linux / Unix

mkdir -p build
cc -g -Wall -Wextra -Os -o build/language -I include/ src/*.c
