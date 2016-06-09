# Emacs-Configuration
My Emacs Configuration (Ubuntu, Python focused)

This is an update to my emacs configuration to use more up to date packages and techniques.  It features on some
package management and ease of use for those used to windows editors.

Full documentation and implementation are in ``gral_config.org``.

## Package Requirements
These can be automatically installed on ubuntu using the `ubuntu_install_emacs.sh` and `ubuntu_install_packages.sh`.

- Emacs 25
- git
- python (2.7 or 3.5)
- python-pip
- openjdk-8-jdk
- openjdk-8-jre

## Python Requirements

`make pipinstall` will install python requirements.

## Testing the configuration (before install)

A dockerfile is provided which will mount this repository to a ubuntu image for testing. If you install docker you
can run this pretty easily:

```
make docker_build
make docker
# now inside the docker
make install
# Call emacs to download all packages automatically.
emacs
# quit emacs
git clone <some code to work on>  /code/
# edit as you want!
```

## Installing

To install:

```
rm -rf ~/.emacs.d
rm -rf ~/.emacs
ln -s ~/path/to/this/repo ~/.emacs
cd ~/.emacs
make install
emacs
```

## Features

### Project management

Using `.dir-locals.el` to set some variables, this provides options for switching between code, unit tests,
and integration tests, as well as running them.

```
((python-mode
  (fill-column . 79)
  (project-venv-name . "foo")
  (whitespace-line-column . 79)
  (gral:prj-code-path . "")
  (gral:prj-unit-test-path . "tests/unit/")
  (gral:prj-unit-test-runner . "nosetests -P")
  (gral:prj-integration-test-path . "tests/integration/")
  (gral:prj-integration-test-runner . "foo execute")
 
   ;;; prj-packages supports multiple packages
  (gral:prj-packages . ("foo" "bar"))
  )
)
```

List of keybindings:
* TODO
