#!/bin/bash

# create an opam switch
opam switch create resspec 5.2.1
opam switch resspec
# install necessary packages
opam install minttea spices leaves
