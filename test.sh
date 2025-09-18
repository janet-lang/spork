#!/bin/bash

janet --uninstall spork
janet -l ./bundle -e '(clean)'
janet --install .
VERBOSE=1 janet -l ./bundle -e '(check)'
