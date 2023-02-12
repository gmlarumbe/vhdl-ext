[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/ERT/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build.yml)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/melpazoid/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/melpazoid.yml)

# vhdl-ext.el - VHDL Extensions for Emacs #

This package includes some extensions on top of the great Emacs `vhdl-mode`.

* Tree-sitter support (requires Emacs 29)
* Improve syntax highlighting
* LSP configuration for `lsp-mode` and `eglot`
* Additional options for `flycheck` linters
* Improve `imenu`, detect instances
* Navigate through instances in a entity
* Jump to definition/reference of entity at point via `ggtags` and `xref`
* Templates insertion via `hydra`

## Installation ##

### Requirements ###

#### Binaries and Emacs Lisp packages ####

`vhdl-ext` makes use of several binaries as backend engines to support IDE-like functionality. In addition, some third party Emacs Lisp packages serve as frontends for those binaries.

List of required binaries:
- Definitions and references navigation: `global`, `gtags`, `universal-ctags`, `python`, `pygments`
- Jump to parent entity: `ag`, `ripgrep`
- Linting: `ghdl`
- LSP: `vhdl-ls`, `ghdl-ls`, `vhdl-tool`, `hdl_checker`

Installation of required Emacs-lisp packages:
```emacs-lisp
(use-package projectile)
(use-package ggtags)
(use-package ag)
(use-package ripgrep)
(use-package hydra)
(use-package outshine)
(use-package flycheck)
(use-package lsp-mode)
(use-package eglot)
```

### vhdl-ext ###

#### straight.el ####

For the time being `vhdl-ext` is still work in progress and is not yet available at [MELPA](https://melpa.org/).
To install it via [straight](https://github.com/radian-software/straight.el):

```emacs-lisp
(straight-use-package 'use-package)
(use-package
    :straight (:repo "gmlarumbe/vhdl-ext"))
```

#### Manually ####
```shell
$ cd ~/.emacs.d
$ git clone https://github.com/gmlarumbe/vhdl-ext
```
And add the following snippet to your `.emacs` or `init.el`:
```emacs-lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vhdl-ext"))
(require 'vhdl-ext)
```

### tree-sitter ###
Requires Emacs 29, installation of `tree-sitter` and VHDL grammar.

To install `tree-sitter` there are different options:

* Via [npm](https://www.npmjs.com/package/tree-sitter)
* Manually:
```shell
$ git clone https://github.com/tree-sitter/tree-sitter.git
$ cd tree-sitter
$ make && sudo make install
```

Installation of grammar can be automated through the script:
```shell
$ .github/scripts/install-ts-grammar.sh
```
That will install `libtree-sitter-vhdl.so` at `$HOME/.emacs.d/tree-sitter`.


## Basic config ##
By default `vhdl-ext` does not create any keybindings. Following snippet shows a configuration example with `use-package`:
```emacs-lisp
(use-package vhdl-ext
  :straight (:host github :repo "gmlarumbe/vhdl-ext")
  :after vhdl-mode
  :demand
  :mode (("\\.vhd\\'" . vhdl-ts-mode))
  :bind (:map vhdl-mode-map
         ("C-M-d"   . vhdl-ext-find-entity-instance-fwd)
         ("C-M-u"   . vhdl-ext-find-entity-instance-bwd)
         ("C-M-."   . vhdl-ext-jump-to-parent-entity)
         ("M-."     . vhdl-ext-jump-to-entity-at-point-def)
         ("M-?"     . vhdl-ext-jump-to-entity-at-point-ref)
         ("C-c C-t" . vhdl-ext-hydra/body))
  :config
  ;; LSP
  (vhdl-ext-lsp-set-server 'ghdl-ls)
  (vhdl-ext-eglot-set-server 'ghdl-ls)
  ;; Flycheck
  (setq vhdl-ext-flycheck-ghdl-work-lib "~/my_ghdl_workdir"))
```

# Features #

## Tree-sitter ##
The package includes the major-mode `vhdl-ts-mode` for syntax highligting and indentation.
There is some WIP, e.g. Imenu or navigation functions.

## Syntax highlighting ##
Improved fontification via:

  * Tree-sitter: requires Emacs 29
  * Font-lock override

<img src="https://user-images.githubusercontent.com/51021955/215353070-8a21f758-407d-4455-bdac-bf92310c59e4.gif" width=400 height=300>

For face customization: <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `vhdl-ext-faces`


## Language Server Protocol ##

Auto-configure various VHDL language servers for `lsp-mode` and `eglot`:

- [rust_hdl](https://github.com/VHDL-LS/rust_hdl.git)
- [ghdl_language_server](https://github.com/ghdl/ghdl-language-server.git)
- [vhdl-tool](http://vhdltool.com)
- [hdl_checker](https://github.com/suoto/hdl_checker)

Make sure that Language Server binary is in the $PATH:
```shell
$ which vhdl_ls
/usr/local/bin/vhdl_ls
```

Interactively:
<kbd>M-x</kbd> `vhdl-ext-lsp-set-server`<kbd>RET</kbd> `ve-vhdl-ls`

Programatically:
```elisp
;; For `lsp-mode':
(vhdl-ext-lsp-set-server 've-vhdl-ls)
;; For `eglot':
(vhdl-ext-eglot-set-server 've-vhdl-ls)
```

## Linting ##
Enhanced version of [GHDL](https://github.com/ghdl/ghdl) flycheck checker.

* Allows setting name of current work library name
* Automatically include directories of open VHDL buffers


## Imenu ##
Support detection of instances

<img src="https://user-images.githubusercontent.com/51021955/215353082-9a187daf-7f76-4c9b-8563-7beba6e1aa6a.gif" width=400 height=300>

* `imenu-list` is a recommended package to visualize different levels of nesting in the hierarchy.

## Navigation ##

### Instance navigation ###
Navigate through instances inside a entity forward/backwards.

Jump to parent entity via `ag`/`ripgrep`.

<img src="https://user-images.githubusercontent.com/51021955/215353135-446b678b-e3be-42f3-8009-5d5bd7c5e5bd.gif" width=400 height=300>


Functions:

* `vhdl-ext-find-entity-instance-fwd`
* `vhdl-ext-find-entity-instance-bwd`
* `vhdl-ext-jump-to-parent-entity`
* `vhdl-ext-instance-at-point`

### Jump to definition/reference ###
Jump to definition/reference of entity at point via `ggtags` and `xref`.

Functions:

* `vhdl-ext-jump-to-entity-at-point`
* `vhdl-ext-jump-to-entity-at-point-def`
* `vhdl-ext-jump-to-entity-at-point-ref`


## Snippets ##
Snippet selection via `hydra`.

<img src="https://user-images.githubusercontent.com/51021955/215353124-7e374754-cd91-4924-9b4b-3c6a29cad921.gif" width=400 height=300>

Functions:

* `vhdl-ext-hydra/body`


# Contributing #

Contributions are welcome! Just stick to common Elisp conventions and run the ERT suite after testing your changes and before submitting a new PR.

For new functionality add new ERT tests if possible.

## ERT Tests setup ###

To run the ERT test suite change directory to the `vhdl-ext` root and run the `ert-tests.sh` script:

```shell
$ cd ~/.emacs.d/vhdl-ext
$ .github/scripts/ert-tests.sh
```

If there is a missing dependency, check the file `.github/scripts/setup-env.sh` used by GitHub Actions to configure your environment.


## Other packages

* [verilog-ext](https://github.com/gmlarumbe/verilog-ext): SystemVerilog Extensions for Emacs
  * Analog package to edit Verilog/SystemVerilog sources
