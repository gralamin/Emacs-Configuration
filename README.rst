===========================
Gralamin's Config for emacs
===========================

This config is based off of `prelude
<https://github.com/bbatsov/prelude>`_. In my opinion, Bozhidar Batsov
went too far.  A lot of the content of this is based off my own
workflow. I want to  to use the same keys for copy and paste
everywhere, even if it limits power.

Included is everything required to work much as I do. Customize
personal if there is anything you feel is missing.

What's new
==========
More keybinds, more modes open automatically.

Installation
============
* Create a symlink from the repository directory to .emacs.d::

    ln -s ~/git/Emacs-Configuration/ .emacs.d

* Create a symlink from __init__.el to .emacs::

    ln -s ~/git/Emacs-Configuration/elisp/__init__.el .emacs

* Make personal changes in the personal subdirectory - all files there
  are automatically loaded.
