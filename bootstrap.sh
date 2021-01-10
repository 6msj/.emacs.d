#!/bin/sh

# Go to top level.
cd ~/.emacs.d

# Clone all submodules.
git submodule update --init --recursive

# Link tmux configuration.
ln -s ~/.emacs.d/tools/.tmux.conf ~/.tmux.conf

# Add a file to indicate emacs has been bootstrapped.
touch ~/.emacs.d/bootstrapped
