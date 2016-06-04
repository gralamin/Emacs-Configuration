# Name of your emacs binary
EMACS=emacs
DIR := ${CURDIR}

install: emacs prereqs languagetool pipinstall

emacs:
	./ubuntu_install_emacs.sh

prereqs:
	./ubuntu_install_packages.sh

languagetool: src/LanguageTool-3.3
	cp -r src/LanguageTool-3.3 /opt/LanguageTool

src/LanguageTool-3.3:
	wget https://languagetool.org/download/LanguageTool-3.3.zip
	unzip LanguageTool-3.3.zip
	mv LanguageTool-3.3 src/LanguageTool-3.3

pipinstall:
	pip install 'jedi>=0.6.0' epc virtualenv argparse virtualenvwrapper

docker: docker_clean
	docker run -i -t --volume $(DIR):/emacs/ -a stdout --name emacs_config_dev emacs_config:latest

docker_build:
	docker build -t emacs_config:latest .

docker_clean:
	-docker stop emacs_config_dev
	-docker rm emacs_config_dev

docker_volume:
	docker volume create --name emacs_config_git
