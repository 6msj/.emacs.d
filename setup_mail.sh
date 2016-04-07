#!/bin/bash

if type "mbsync" > /dev/null; then
    echo "isync installed"
else
    echo "isync not installed. installing isync."
    brew install isync
fi

if type "mu" > /dev/null; then
    echo "mu installed"
else
    brew install mu --with-emacs
    echo "mu not installed. installing mu."
fi

if type "w3m" > /dev/null; then
    echo "w3m installed"
else
    brew install w3m
    echo "w3m not installed. installing w3m."
fi

if type "openssl" > /dev/null; then
    echo "openssl installed"
else
    echo "openssl not installed. installing openssl"
    brew install openssl
fi

echo "linking openssl --force"
brew unlink openssl && brew link openssl --force

if type "gpg" > /dev/null; then
    echo "gpg installed"
else
    echo "gpg not installed. installing gpg"
    brew install gpg
fi

if type "gpg2" > /dev/null; then
    echo "gpg2 installed"
else
    echo "gpg2 not installed. installing gpg2"
    brew install gpg2
fi

if type "gnutls-cli" > /dev/null; then
    echo "gnutls installed"
else
    echo "gnutls not installed. installing gnutls"
    brew install gnutls
fi

if [ ! -d $HOME/Mail ];
then
    echo "creating director $HOME/Mail"
    mkdir $HOME/Mail
fi

if [ ! -d $HOME/Mail/gmail ];
then
    echo "creating director $HOME/Mail"
    mkdir $HOME/Mail/gmail
fi

if [ -f $HOME/.authinfo.gpg ];
then
    echo "moving $HOME/.authinfo.gpg to $HOME/.authinfo.gpg_backup"
    mv $HOME/.authinfo.gpg $HOME/.authinfo.gpg_backup
fi

echo "linking $HOME/.emacs.d/.email.gpg to $HOME/.authinfo.gpg "
ln -s $HOME/.emacs.d/.email.gpg $HOME/.authinfo.gpg

if [ -f $HOME/.gnupg/gpg-agent.conf ];
then
    echo "moving $HOME/.gnupg/gpg-agent.conf to $HOME/.gnupg/gpg-agent.conf_backup"
    mv $HOME/.gnupg/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf_backup 
fi

echo "linking $HOME/.emacs.d/.gpg-agent.conf to $HOME/.gnupg/gpg-agent.conf "
ln -s $HOME/.emacs.d/.gpg-agent.conf $HOME/.gnupg/gpg-agent.conf

if [ -f $HOME/.mbsyncrc ];
then
    echo "moving $HOME/.mbsyncrc to $HOME/.mbsyncrc_backup"
    mv $HOME/.mbsyncrc $HOME/.mbsyncrc_backup
fi

echo "linking $HOME/.emacs.d/.mbsyncrc to $HOME/.mbsyncrc "
ln -s $HOME/.emacs.d/.mbsyncrc $HOME/.mbsyncrc

echo "run $ mbsync gmail"
echo "run $ mu index --maildir=~/Mail"
