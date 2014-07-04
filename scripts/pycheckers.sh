#!/bin/bash
# From http://stackoverflow.com/questions/1259873/how-can-i-use-emacs-flymake-mode-for-python-with-pyflakes-and-pylint-checking-co

if [ -n $2 ];
then
    . /local/home/nelsog4/venv/$2/bin/activate
fi
epylint "$1" 2>/dev/null
pyflakes "$1"
pep8 --repeat "$1"
pep257 "$1" > /dev/null
if [ -n $2 ];
then
    deactivate
fi
true
