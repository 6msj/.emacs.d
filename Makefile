all:
	git submodule update --init --recursive

ycmd:
	python ~/.emacs.d/fork/ycmd/build.py --all --system-libclang

mail:
	guile ~/.emacs.d/mail/setup_mail.scm

.PHONY: ycmd mail all
