FROM ubuntu:xenial

MAINTAINER glen.nelson@emc.com
RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get -y --force-yes install software-properties-common make

# Add target directory
RUN mkdir /emacs

# Setup emacs to run /code configuration automatically
RUN ln -s /emacs ~/.emacs.d

# Run make to byte compile everything
VOLUME /emacs
WORKDIR /emacs

# Install ubuntu packages
RUN /bin/bash
