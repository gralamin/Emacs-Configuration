#!/bin/bash
# From http://stackoverflow.com/questions/1259873/how-can-i-use-emacs-flymake-mode-for-python-with-pyflakes-and-pylint-checking-co

THIS_DIR=$( dirname "${BASH_SOURCE[0]}" )

echo "Calling $THIS_DIR/check_all.py \"$1\"" > /tmp/glendebug
python $THIS_DIR/check_all.py "$1" | tee "/tmp/glentest"
true
