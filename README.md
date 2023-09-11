[![MELPA](https://melpa.org/packages/vhdl-ext-badge.svg)](https://melpa.org/#/vhdl-ext)
[![MELPA Stable](https://stable.melpa.org/packages/vhdl-ext-badge.svg)](https://stable.melpa.org/#/vhdl-ext)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/ERT-straight/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_straight.yml)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/package-el-basic/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_package_melpa_basic.yml)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/workflows/ERT-MELPA-Stable/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_package_melpa_stable.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# vhdl-ext.el - VHDL Extensions for Emacs #

This package includes some extensions on top of the great Emacs `vhdl-mode`.

* [Tree-sitter powered `vhdl-ts-mode`](#tree-sitter)
* [Improve syntax highlighting](#syntax-highlighting)
* [Hierarchy extraction and navigation](#hierarchy-extraction)
* [LSP configuration for `lsp-mode` and `eglot`](#language-server-protocol)
* [Support for many linters via `flycheck`](#linting)
* [Beautify blocks and instances](#beautify-blocks-and-instances)
* [Navigate through instances in a entity](#navigation)
* [Templates insertion via `hydra`](#templates)
* [Compilation with colored errors/warnings and jump to file/line](#compilation)
* [Improve `imenu`, detect instances](#imenu)
* [Enhanced support for `which-func`](#which-func)
* [Improve code folding via `hideshow`](#code-folding)
* [Auto-configure `time-stamp`](#time-stamp)
* [Port connection utilities](#port-connections)

## Installation ##

### MELPA ###

`vhdl-ext` and `vhdl-ts-mode` are available on MELPA. `vhdl-ext` includes `vhdl-ts-mode` as a dependency.

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
        hierarchy
        eglot
        lsp
        flycheck
        beautify
        navigation
        template
        compilation
        imenu
        which-func
        hideshow
        time-stamp
        ports))
(vhdl-ext-mode-setup)
(add-hook 'vhdl-mode-hook #'vhdl-ext-mode)
;; To use `vhdl-ts-mode' as the default major-mode also add the line below:
(add-to-list 'auto-mode-alist '("\\.vhdl?\\'" . vhdl-ts-mode))
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
          hierarchy
          eglot
          lsp
          flycheck
          beautify
          navigation
          template
          compilation
          imenu
          which-func
          hideshow
          time-stamp
          ports))
  :config
  (vhdl-ext-mode-setup))

;; To use `vhdl-ts-mode' as the default major-mode also add the lines below:
(use-package vhdl-ts-mode
  :mode (("\\.vhdl?\\'" . vhdl-ts-mode))
```

## Keybindings ##

Enabling of `vhdl-ext-mode` minor-mode creates the following keybindings:

  * <kbd>C-M-f</kbd> `vhdl-ext-forward-sexp`
  * <kbd>C-M-b</kbd> `vhdl-ext-backward-sexp`
  * <kbd>C-M-u</kbd> `vhdl-ext-find-entity-instance-bwd`
  * <kbd>C-M-d</kbd> `vhdl-ext-find-entity-instance-fwd`
  * <kbd>C-M-.</kbd> `vhdl-ext-jump-to-parent-entity`
  * <kbd>C-c M-.</kbd> `vhdl-ext-jump-to-entity-at-point-def`
  * <kbd>C-c M-?</kbd> `vhdl-ext-jump-to-entity-at-point-ref`
  * <kbd>C-M-i</kbd> `vhdl-ext-beautify-block-at-point`
  * <kbd>C-c M-i</kbd> `vhdl-ext-beautify-instance-at-point`
  * <kbd>C-c C-t</kbd> `vhdl-ext-hydra/body`
  * <kbd>C-c C-f</kbd> `vhdl-ext-flycheck-mode`
  * <kbd>C-c \<f5\></kbd> `vhdl-ext-compile-ghdl-project`
  * <kbd>C-c C-c t</kbd> `vhdl-ext-ports-toggle-connect`
  * <kbd>C-c C-c r</kbd> `vhdl-ext-ports-connect-recursively`
  * <kbd>C-c C-v</kbd> `vhdl-ext-hierarchy-current-buffer`


# Features #

## Tree-sitter ##
The package `vhdl-ts-mode` provides syntax highlighting,
indentation and a backend for hierarchy extraction, definitions and
references navigation, and some other features implemented in
`vhdl-ext`. Using tree-sitter as a backend is recommended as it is
much faster and efficient than internal Emacs lisp parsing.

`vhdl-ts-mode` is derived from `vhdl-mode` making all `vhdl-mode`
functionality still available.

For more information see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Tree-sitter).


## Syntax highlighting ##

<img src="https://user-images.githubusercontent.com/51021955/215353070-8a21f758-407d-4455-bdac-bf92310c59e4.gif" width=400 height=300>

For configuration information, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Syntax-highlighting).


## Hierarchy extraction ##

Hierarchy extraction of entity at current buffer.

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/79018dc4-833a-4ce6-9f2b-3195ba75481d" width=400 height=300>

For configuration information, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Hierarchy).


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


## Beautify blocks and instances ##

Beautify block and instances at point:

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/cffb0c9d-3a39-4e58-b422-fb275db124a8" width=400 height=300>

Interactive functions:

* `vhdl-ext-beautify-block-at-point`: <kbd>C-M-i</kbd>
* `vhdl-ext-beautify-instance-at-point`: <kbd>C-c M-i</kbd>

Batch-mode functions:

* `vhdl-ext-beautify-files`
* `vhdl-ext-beautify-dir-files`


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


## Which-func ##

Enhanced `which-func` support: show current block/instance at point in the mode-line


## Code folding ##

Improve code folding via `hideshow`: add support for if/else/elsif blocks.


## Time-stamp ##

Automatic update of header timestamp after file saving.

   - `vhdl-ext-time-stamp-mode`

For configuration see [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Time-stamp)


## Port connections ##

Toggle connections of ports under instance at point:

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/14536463-a6c4-410f-a890-081f7deb668e" width=400 height=400>

  * `vhdl-ext-ports-toggle-connect`: <kbd>C-c C-c t</kbd>
  * `vhdl-ext-ports-connect-recursively`: <kbd>C-c C-c r</kbd>


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
$ cd ~/.emacs.d/verilog-ext
$ make subset TESTS=imenu
```

## Other packages ##

* [verilog-ext](https://github.com/gmlarumbe/verilog-ext): SystemVerilog Extensions for Emacs
  * Analog package to edit Verilog/SystemVerilog sources
* [fpga](https://github.com/gmlarumbe/fpga): FPGA & ASIC Utilities for Emacs
  * Utilities for tools of major vendors of FPGA & ASIC
* [wavedrom-mode](https://github.com/gmlarumbe/wavedrom-mode): Wavedrom integration for Emacs
  * Edit and render WaveJSON files to create timing diagrams
* [vunit-mode](https://github.com/embed-me/vunit-mode.git): VUnit Mode for Emacs
  * Integration of [VUnit](https://github.com/VUnit/vunit) workflow.
