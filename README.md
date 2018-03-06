ido-ghq
-------

Introduction
============

`ido-ghq` provides interfaces of ghq with ido.

I strongly recommends to use [ido-vertical-mode](https://github.com/creichert/ido-vertical-mode.el]) or [ido-grid-mode](https://github.com/larkery/ido-grid-mode.el) in order to display the list of ghq -- long, long line.

Requirements
============

* Emacs 24.5 or higher: I use this program on Emacs 25.2 and 24.5.
* [ghq](https://github.com/motemen/ghq)

Setup and Customize
===================

``` common-lisp
(add-to-list 'load-path "somewhere")
(require 'ido-ghq)
(setq ido-ghq-short-list t)   ;;  Whether display full path or short path
```

Usage
=====

`ido-ghq`: Execute `ghq list --full-path` and Open selected directory by dired.


License
=======

This program is folk of [emacs-helm-ghq](https://github.com/masutaka/emacs-helm-ghq).
License is same as original, GPL-3+.
