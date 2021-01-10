all:
	git submodule update --init --recursive

mail:
	guile ~/.emacs.d/mail/setup_mail.scm

.PHONY: mail all
