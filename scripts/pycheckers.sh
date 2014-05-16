#!/bin/bash
# From http://stackoverflow.com/questions/1259873/how-can-i-use-emacs-flymake-mode-for-python-with-pyflakes-and-pylint-checking-co

epylint "$1" 2>/dev/null
pyflakes "$1"
pep8 --ignore=E221,E701,E202,E501 --repeat "$1"
pep257 "$1"
true
