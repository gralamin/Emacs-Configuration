#!/bin/bash
add-apt-repository -y ppa:ubuntu-elisp
apt-get update
DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes emacs-snapshot
pip install jedi\>=0.6.0 epc
