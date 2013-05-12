Monokai Color Theme for Emacs 24
================================

Monokai color theme for Emacs. **Requires Emacs 24**.

 * Home Page: https://github.com/lvillani/el-monokai-theme
 * Reporting issues: https://github.com/lvillani/el-monokai-theme/issues




Looking for a New Maintainer
============================

Due to time constraints and the fact that I'm not using Emacs at the
moment, I am looking for people willing to co-maintain this project.
If interested drop me a line by sending an email to <lorenzo@villani.me>.



Installation
============

This package is available from [Marmalade](http://marmalade-repo.org/). Refer
to its documentation to learn how to use Marmalade and `package.el` with your
version of Emacs.

To install the theme evaluate (`C-x C-e`) this form in a scratch buffer:

    (progn
      (package-refresh-contents)
      (package-install 'monokai-theme))

Then enable it by adding this line to your `~/.emacs` or `~/.emacs.d/init.el`
file:

    (load-theme 'monokai t)




Screenshot
==========

![Screenshot](https://raw.github.com/lvillani/el-monokai-theme/master/screenshot/intel_panel.png)
