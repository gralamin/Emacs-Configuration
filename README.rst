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

Prerequistes
============
Assumes pylint, pyflakes, and pep8 are installed.

`Install jslint
<http://stackoverflow.com/questions/8863888/how-do-i-install-jslint-on-ubuntu/>`_.
Create a symlink to jsl called jslint.

Installation
============
* Create a symlink from the repository directory to .emacs.d::

    ln -s ~/git/Emacs-Configuration/ .emacs.d

* Create a symlink from __init__.el to .emacs::

    ln -s ~/git/Emacs-Configuration/elisp/__init__.el .emacs

* Make personal changes in the personal subdirectory - all files there
  are automatically loaded.

Running Emacs as a Daemon
=========================
In your .bashrc add::

    export ALTERNATE_EDITOR="" EDITOR=emacsclient VISUAL=emacsclient




Suggested bash reconfigurations
===============================
I like working with these changes::

    #prompt and colours
    BLACK='\[\e[0;30m\]'
    RED='\[\e[0;31m\]'
    BOLDRED='\[\033[01;31m\]'
    GREEN='\[\e[0;32m\]'
    BOLDGREEN='\[\033[01;32m\]'
    YELLOW='\[\e[0;33m\]'
    BLUE='\[\e[0;34m\]'
    BOLDBLUE='\[\033[01;34m\]'
    PURPLE='\[\e[0;35m\]'
    BOLDCYAN='\[\033[01;36m\]'
    CYAN='\[\e[0;36m\]'
    WHITE='\[\e[0;37m\]'
    BOLDWHITE='\[\033[01;37m\]'
    BLACKBG='\[\e[40m\]'
    DEFAULT_COLOR='\[\033[00m\]'

    BACKGROUNDISWHITE="true"

    if [ "$BACKGROUNDISWHITE" = "false" ]; then
        #DEFAULT_COLOR=$WHITE;
        DIVIDER=$BOLDWHITE
        USER=$BOLDCYAN
        HOST=$BOLDRED
        DIR=$BOLDBLUE
    else
        #DEFAULT_COLOR=$BLACK;
        DIVIDER=$BLACK
        USER=$BLUE
        HOST=$RED
        DIR=$PURPLE
    fi
    END_PS1=$DEFAULT_COLOR
    export TERM=xterm-256color
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    if [ -x /usr/bin/dircolors ]; then
        ORIG_PS1="$USER\u$DIVIDER@$HOST\h$DIVIDER:$DIR\w$DIVIDER>$END_PS1 "
    fi

And these aliases::

    if [ -x /usr/bin/dircolors ]; then
        test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
        alias ls='ls --color=auto'
        #alias dir='dir --color=auto'
        #alias vdir='vdir --color=auto'

        alias grep='grep --color=auto'
        alias fgrep='fgrep --color=auto'
        alias egrep='egrep --color=auto'
    fi
    alias edit='$EDITOR'
    alias ll='ls -l'
    alias la='ls -A'
    alias l='ls -CF'
    alias vim='emacsclient -t'
    alias vi='emacsclient -t'

I've also used the following bash functions::

    function swups {
        BRANCH=`git branch | grep -e \* | tr -d \* | tr -d \ `
        if [ $2 ]; then
            currentUpStream=$2
        else
            REMOTE=`git config branch.$BRANCH.remote`
            # refs/head/branchName, sed will make branchName
            RBRANCH=`git config branch.$BRANCH.merge | sed -r "s/refs\/heads\///g"`
            if [ $REMOTE ]; then
                currentUpStream="$REMOTE/$RBRANCH"
            else
                echo "Please specify an 'upstream'"
                return 1
            fi
        fi
        if [ $1 ]; then
            newUpStreamBranch=$1
            git rebase --onto $newUpStreamBranch $currentUpStream
            git branch --set-upstream $BRANCH $newUpStreamBranch
            # renaming added here
            return 0
        else
            echo "swups <newUpstream> [oldUpstream]"
        fi
    }

    function grepc
    {
        grep "$1" -rn --color=auto --include=*.{py,rst} --exclude=tags -i *
    }

    function egrepc
    {
        egrep "$1" -rn --color=auto --include=*.{py,rst} --exclude=tags -i *
    }

    function dufa {
        du -ka "$@" | sort -n | while read size fname; do for unit in k M G T P E Z Y; do if [ $size -lt 1024 ]; then echo -e "${size}${unit}\t${fname}"; break; fi; size=$((size/1024)); done; done
    }

    function gitwhot {
        #git-who-am-i-tracking, or git-who-t
        #This could easily be expanded so it could take an argument instead.
        BRANCH=`git branch | grep -e \* | tr -d \* | tr -d \ `
        REMOTE=`git config branch.$BRANCH.remote`
        # refs/head/branchName, sed will make branchName
        RBRANCH=`git config branch.$BRANCH.merge | sed -r "s/refs\/heads\///g"`
        echo "$BRANCH is tracking $REMOTE/$RBRANCH"
        return 0
    }

    function gpyspaces {
        grep -r "([^)]* = [^)]*)" $(git show --pretty='format:' --name-only HEAD)
    }

    Also recommeneded is a way to track git branch from the terminal.


Suggested Git aliases
=====================

A few aliases that I like::

    [alias]
            lg = log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --
            rmu = !git status -sb | grep ?? | cut -d ' ' -f 2 | xargs rm
            addu = !git status -sb | grep ?? | cut -d ' ' -f 2 | xargs git add
            st = !git status -sb
            pr = !git pull --rebase
            names = !git show --pretty='format:' --name-only
