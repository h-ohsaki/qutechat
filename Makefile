# make push

NAME = $(shell basename `pwd`)

all:

init:
	git init
	git remote add origin https://github.com/h-ohsaki/$(NAME).git
	git config credential.helper store

push:
	git push -u origin master
