#!/bin/sh
# Build script for Linux / Unix

mkdir -p build
cc -o build/language src/*.c
