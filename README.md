[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/ERT/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions)

# vhdl-ext.el - VHDL Extensions for Emacs #

This package includes some extensions on top of the great Emacs `vhdl-mode`.

* Tree-sitter support (requires Emacs 29)
* Improve syntax highlighting
* LSP configuration for `lsp-mode` and `eglot`
* Additional options for `flycheck` linters
* Improve `imenu`: detect instances
* Navigate through instances in a module
* Jump to definition/reference of module at point via `ggtags` and `xref`
* Templates insertion via `hydra`

## Installation ##

### Requirements ###

#### Binaries and Emacs Lisp packages ####

`vhdl-ext` makes use of several binaries as backend engines to support IDE-like functionality. In addition, some third party Emacs Lisp packages serve as frontends for those binaries.

List of required binaries:
- Definitions and references navigation: `global`, `gtags`, `universal-ctags`, `python`, `pygments`
- Jump to parent module: `ag`, `ripgrep`
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
         ("C-M-."   . vhdl-ext-jump-to-parent-module)
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

For face customization: <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `vhdl-ext-faces`



## Language Server Protocol ##

Auto-configure various VHDL language servers for `lsp-mode` and `eglot`:

- [rust_hdl](https://github.com/VHDL-LS/rust_hdl.git)
- [ghdl_language_server](https://github.com/ghdl/ghdl-language-server.git)
- [vhdl-tool](http://vhdltool.com)
- [hdl_checker](https://github.com/suoto/hdl_checker)

Functions:

* `vhdl-ext-lsp-set-server`
* `vhdl-ext-eglot-set-server`


## Linting ##
Enhanced version of [GHDL](https://github.com/ghdl/ghdl) `flycheck` checker.

* Allows setting name of current work library name
* Automatically include directories of open VHDL buffers


## Imenu ##
Support detection of instances

* `imenu-list` is a recommended package to visualize different levels of nesting in the hierarchy.


## Navigation ##

### Instance navigation ###
Navigate through instances inside a entity forward/backwards.
Jump to parent entity via `ag`/`ripgrep`.

<img src="https://user-images.githubusercontent.com/51021955/208782492-b2ff09b3-f662-4d22-a46c-64eb69f9f7b9.gif" width=400 height=300>

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
