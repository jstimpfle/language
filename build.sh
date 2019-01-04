#!/bin/sh
# Build script for Linux / Unix

cc -std=c99 -O2 -g -Wall -Wextra -o blunt -I include/ src/*.c
