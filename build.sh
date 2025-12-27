#!/bin/bash

# compile
dune build

# move binary to current directory
cp ./_build/default/bin/main.exe ./resspec
