[![MELPA](https://melpa.org/packages/vhdl-ext-badge.svg)](https://melpa.org/#/vhdl-ext)
[![MELPA Stable](https://stable.melpa.org/packages/vhdl-ext-badge.svg)](https://stable.melpa.org/#/vhdl-ext)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/ERT-straight/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_straight.yml)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/ERT-package-el/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_package.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# vhdl-ext.el - VHDL Extensions for Emacs #

This package includes some extensions on top of the great Emacs `vhdl-mode`.

* [Tree-sitter powered `vhdl-ts-mode`](#tree-sitter)
* [Improve syntax highlighting](#syntax-highlighting)
* [LSP configuration for `lsp-mode` and `eglot`](#language-server-protocol)
* [Support for many linters via `flycheck`](#linting)
* [Navigate through instances in a entity](#navigation)
* [Templates insertion via `hydra`](#templates)
* [Compilation with colored errors/warnings and jump to file/line](#compilation)
* [Improve `imenu`, detect instances](#imenu)
* [Auto-configure `time-stamp`](#time-stamp)
* [Auto-configure `company-keywords`](#company-keywords)

## Installation ##

### MELPA ###

`vhdl-ext` is available on MELPA.

`vhdl-ts-mode` is not yet available on MELPA. See the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Tree-sitter) for more info.


### straight.el ###

To install it via [straight](https://github.com/radian-software/straight.el) with `use-package`:

```emacs-lisp
(straight-use-package 'use-package)
(use-package vhdl-ext)
```

## Basic config ##

The most basic configuration just requires choosing which features you
want to load, setup the minor-mode and add it as a hook for `vhdl-mode`.
By default all features are enabled:

```elisp
;; Can also be set through `M-x RET customize-group RET vhdl-ext':
;;  - Vhdl Ext Feature List (provides info of different features)
;; Comment out/remove the ones you do not need
(setq vhdl-ext-feature-list
      '(font-lock
        eglot
        lsp
        flycheck
        navigation
        template
        compilation
        imenu
        time-stamp
        company-keywords))
(vhdl-ext-mode-setup)
(add-hook 'vhdl-mode-hook #'vhdl-ext-mode)
```

If installed and loaded via `use-package`:

```elisp
(use-package vhdl-ext
  :after vhdl-mode
  :demand
  :hook ((vhdl-mode . vhdl-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET vhdl-ext':
  ;;  - Vhdl Ext Feature List (provides info of different features)
  ;; Comment out/remove the ones you do not need
  (setq vhdl-ext-feature-list
        '(font-lock
          eglot
          lsp
          flycheck
          navigation
          template
          compilation
          imenu
          time-stamp
          company-keywords))
  :config
  (vhdl-ext-mode-setup))
```

## Keybindings ##

Enabling of `vhdl-ext-mode` minor-mode creates the following keybindings:

  * <kbd>C-M-u</kbd> `vhdl-ext-find-entity-instance-bwd`
  * <kbd>C-M-d</kbd> `vhdl-ext-find-entity-instance-fwd`
  * <kbd>C-M-.</kbd> `vhdl-ext-jump-to-parent-entity`
  * <kbd>C-c M-.</kbd> `vhdl-ext-jump-to-entity-at-point-def`
  * <kbd>C-c M-?</kbd> `vhdl-ext-jump-to-entity-at-point-ref`
  * <kbd>C-c C-t</kbd> `vhdl-ext-hydra/body`
  * <kbd>C-c C-f</kbd> `vhdl-ext-flycheck-mode`
  * <kbd>C-c \<f5\></kbd> `vhdl-ext-compile-ghdl-project`


# Features #

## Tree-sitter ##
The package provides the major-mode `vhdl-ts-mode` for syntax highligting and indentation. It is derived from `vhdl-mode` making all `vhdl-mode` functionality still available.

`vhdl-ts-mode` is still work in progress and aims to provide the same functionality as `vhdl-ext` but much faster and efficiently.

For more information see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Tree-sitter).


## Syntax highlighting ##

<img src="https://user-images.githubusercontent.com/51021955/215353070-8a21f758-407d-4455-bdac-bf92310c59e4.gif" width=400 height=300>

For configuration information, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Syntax-highlighting).


## Language Server Protocol ##

Auto-configure various VHDL language servers for `lsp-mode` and `eglot`:

- [rust_hdl](https://github.com/VHDL-LS/rust_hdl.git)
- [ghdl_language_server](https://github.com/ghdl/ghdl-language-server.git)
- [vhdl-tool](http://vhdltool.com)
- [hdl_checker](https://github.com/suoto/hdl_checker)

For configuration instructions, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Language-Server-Protocol).


## Linting ##

Support via `flycheck` for the following linters:

* [GHDL](https://github.com/ghdl/ghdl)
* [vhdl_lang](https://github.com/VHDL-LS/rust_hdl)
* [vhdl-tool](http://vhdltool.com)

For configuration and usage instructions, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Linting)


## Navigation ##

<img src="https://user-images.githubusercontent.com/51021955/215353135-446b678b-e3be-42f3-8009-5d5bd7c5e5bd.gif" width=400 height=300>

* Navigate instances inside an entity
* Jump to definition/references of entity at point
* Jump to parent entity

For detailed info see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Navigation).


## Templates ##
Snippet selection via `hydra`.

<img src="https://user-images.githubusercontent.com/51021955/215353124-7e374754-cd91-4924-9b4b-3c6a29cad921.gif" width=400 height=300>

* `vhdl-ext-hydra/body`: <kbd>C-c C-t</kbd>


## Compilation ##

Provides functions to perform compilations with syntax highlighting
and jump to error:

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/845980ab-c54b-4e89-b53f-056140be87a7" width=400 height=300>

* `vhdl-ext-compile-ghdl-project`: <kbd>C-c \<f5\></kbd>

See more info in the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Compilation).


## Imenu ##
Support detection of instances.

<img src="https://user-images.githubusercontent.com/51021955/215353082-9a187daf-7f76-4c9b-8563-7beba6e1aa6a.gif" width=400 height=300>


## Time-stamp ##

Automatic update of header timestamp after file saving.

   - `vhdl-ext-time-stamp-mode`

For configuration see [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Time-stamp)


## Company keywords ##

Setup `company` to complete with VHDL keywords.

# Contributing #

Contributions are welcome! Just stick to common Elisp conventions and run the ERT suite after testing your changes and before submitting a new PR.

For new functionality add new ERT tests if possible.

Consider [sponsoring](https://github.com/sponsors/gmlarumbe) to help
maintaining the project and for the development of new features. *Thank you!*

## ERT Tests setup ###

To run the whole ERT test suite change directory to the `vhdl-ext` root and run the `test` target:

```shell
$ cd ~/.emacs.d/vhdl-ext
$ make
```

To run a subset of tests (e.g. imenu):

```shell
$ cd ~/.emacs.d/vhdl-ext
$ test/scripts/ert-tests.sh recompile_run imenu::
```

## Other packages ##

* [verilog-ext](https://github.com/gmlarumbe/verilog-ext): SystemVerilog Extensions for Emacs
  * Analog package to edit Verilog/SystemVerilog sources
* [fpga](https://github.com/gmlarumbe/fpga): FPGA & ASIC Utilities for Emacs
  * Utilities for tools of major vendors of FPGA & ASIC
