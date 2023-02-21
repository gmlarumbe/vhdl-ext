[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/ERT/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build.yml)
[![MELPA](https://melpa.org/packages/vhdl-ext-badge.svg)](https://melpa.org/#/vhdl-ext)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/melpazoid/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/melpazoid.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# vhdl-ext.el - VHDL Extensions for Emacs #

This package includes some extensions on top of the great Emacs `vhdl-mode`.

* Tree-sitter powered `vhdl-ts-mode`
* Improve syntax highlighting
* LSP configuration for `lsp-mode` and `eglot`
* Additional options for `flycheck` linters
* Improve `imenu`, detect instances
* Navigate through instances in a entity
* Jump to definition/reference of entity at point
* Templates insertion via `hydra`

## Installation ##

### MELPA ###

`vhdl-ext` is available on MELPA.

See [Getting Started](https://melpa.org/partials/getting-started.html) for instructions on how to setup and download packages.

`vhdl-ts-mode` is not yet available on MELPA. See [notes](https://github.com/gmlarumbe/vhdl-ext/wiki/Tree-sitter#notes) for more info.


### straight.el ###

To install it via [straight](https://github.com/radian-software/straight.el) with `use-package`:

```emacs-lisp
(straight-use-package 'use-package)

(use-package vhdl-ext
  :straight (:host github :repo "gmlarumbe/vhdl-ext"
             :files ("vhdl-ext.el" "vhdl-ts-mode.el"")))
```

### Manually ###

First download `vhdl-ext` in the desired directory (e.g. `~/.emacs.d`):

```shell
$ cd ~/.emacs.d
$ git clone https://github.com/gmlarumbe/vhdl-ext
```

And add the following snippet to your `.emacs` or `init.el`:

```emacs-lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vhdl-ext"))
(require 'vhdl-ext)
```

## Basic config ##

The most basic configuration just requires setup of the minor-mode and to add it as a hook for `vhdl-mode`:

```elisp
(vhdl-ext-mode-setup)
(add-hook 'vhdl-mode-hook #'vhdl-ext-mode)
```

If installed and loaded via `use-package`:

```elisp
(use-package vhdl-ext
  :after vhdl-mode
  :demand
  :hook ((vhdl-mode . vhdl-ext-mode))
  :config
  (vhdl-ext-mode-setup))
```

## Keybindings ##

Enabling of `verilog-ext-mode` minor-mode creates the following keybindings:

  * <kbd>C-M-u</kbd> `vhdl-ext-find-entity-instance-bwd`
  * <kbd>C-M-d</kbd> `vhdl-ext-find-entity-instance-fwd`
  * <kbd>C-M-.</kbd> `vhdl-ext-jump-to-parent-entity`
  * <kbd>C-c C-t</kbd> `vhdl-ext-hydra/body`


# Features #

## Tree-sitter ##
The package provides the major-mode `vhdl-ts-mode` for syntax highligting and indentation. It is derived from `vhdl-mode` making all `vhdl-mode` functionality still available.

`vhdl-ts-mode` is still work in progress and aims to provide the same functionality as `vhdl-ext` but much faster and efficiently.

For more information see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Tree-sitter).


## Syntax highlighting ##

<img src="https://user-images.githubusercontent.com/51021955/215353070-8a21f758-407d-4455-bdac-bf92310c59e4.gif" width=400 height=300>

For face customization: <kbd>M-x</kbd> `customize-group` <kbd>RET</kbd> `vhdl-ext-faces`


## Language Server Protocol ##

Auto-configure various VHDL language servers for `lsp-mode` and `eglot`:

- [rust_hdl](https://github.com/VHDL-LS/rust_hdl.git)
- [ghdl_language_server](https://github.com/ghdl/ghdl-language-server.git)
- [vhdl-tool](http://vhdltool.com)
- [hdl_checker](https://github.com/suoto/hdl_checker)

For configuration instructions, see the [wiki](https://github.com/gmlarumbe/verilog-ext/wiki/Language-Server-Protocol)


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
