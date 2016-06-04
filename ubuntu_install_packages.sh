#!/bin/bash
add-apt-repository -y ppa:git-core/ppa
apt-get update

#Any additional packages to install here.
DEBIAN_FRONTEND=noninteractive apt-get install -y --force-yes \
               git \
               python \
               python-pip \
               openjdk-8-jdk \
               openjdk-8-jre
