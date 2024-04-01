[![MELPA](https://melpa.org/packages/vhdl-ext-badge.svg)](https://melpa.org/#/vhdl-ext)
[![MELPA Stable](https://stable.melpa.org/packages/vhdl-ext-badge.svg)](https://stable.melpa.org/#/vhdl-ext)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_straight.yml/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_straight.yml)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_package_melpa_basic.yml/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_package_melpa_basic.yml)
[![Build Status](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_package_melpa_stable.yml/badge.svg)](https://github.com/gmlarumbe/vhdl-ext/actions/workflows/build_package_melpa_stable.yml)


# vhdl-ext.el - VHDL Extensions for Emacs #

This package provides useful extensions on top of [`vhdl-mode`](https://iis-people.ee.ethz.ch/~zimmi/emacs/vhdl-mode.html)
and [`vhdl-ts-mode`](https://github.com/gmlarumbe/vhdl-ts-mode).

* [Tree-sitter `vhdl-ts-mode` support](#tree-sitter)
* [Project management](#project-management)
* [Improve syntax highlighting](#syntax-highlighting)
* [Find definitions and references](#find-definitions-and-references)
* [Auto-completion](#auto-completion)
* [Hierarchy extraction and navigation](#hierarchy-extraction)
* [LSP configuration for `lsp-bridge`, `lsp-mode` and `eglot`](#language-server-protocol)
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

## Requirements ##

- Emacs 29.1+
- Feature-specific binaries

Tree-sitter is optional but recommended and only required if using `vhdl-ts-mode` for some of the features above.

For more info, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Requirements).


## Installation ##

### MELPA ###

`vhdl-ext` is available on MELPA.

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
;; Comment out/remove the ones you do not need
(setq vhdl-ext-feature-list
      '(font-lock
        xref
        capf
        hierarchy
        eglot
        lsp
        lsp-bridge
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
(require 'vhdl-ext)
(vhdl-ext-mode-setup)
(add-hook 'vhdl-mode-hook #'vhdl-ext-mode)
```

If installed and loaded via `use-package`:

```elisp
(use-package vhdl-ext
  :hook ((vhdl-mode . vhdl-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET vhdl-ext':
  ;; Comment out/remove the ones you do not need
  (setq vhdl-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          lsp
          lsp-bridge
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
```

## Keybindings ##

Enabling of `vhdl-ext-mode` minor-mode creates the following keybindings:

* Features
  * <kbd>C-M-i</kbd> `vhdl-ext-beautify-block-at-point`
  * <kbd>C-c C-v</kbd> `vhdl-ext-hierarchy-current-buffer`
  * <kbd>C-c C-t</kbd> `vhdl-ext-hydra/body`
  * <kbd>C-c C-f</kbd> `vhdl-ext-flycheck-mode`
  * <kbd>C-c C-u></kbd> `vhdl-ext-tags-get`
  * <kbd>C-c \<f5\></kbd> `vhdl-ext-compile-project-ghdl`

* Navigation
  * <kbd>C-M-f</kbd> `vhdl-ext-forward-sexp`
  * <kbd>C-M-b</kbd> `vhdl-ext-backward-sexp`
  * <kbd>C-M-u</kbd> `vhdl-ext-find-entity-instance-bwd`
  * <kbd>C-M-d</kbd> `vhdl-ext-find-entity-instance-fwd`
  * <kbd>C-M-.</kbd> `vhdl-ext-jump-to-parent-entity`
  * <kbd>C-c M-.</kbd> `vhdl-ext-jump-to-entity-at-point-def`
  * <kbd>C-c M-?</kbd> `vhdl-ext-jump-to-entity-at-point-ref`

* Port connections
  * <kbd>C-c C-c t</kbd> `vhdl-ext-ports-toggle-connect`
  * <kbd>C-c C-c r</kbd> `vhdl-ext-ports-connect-recursively`


# Features #

## Tree-sitter ##

Some of the features that `vhdl-ext` provides are based either on
builtin `vhdl-mode` Emacs lisp parsing or on tree-sitter
`vhdl-ts-mode`:

- Hierarchy extraction can use both builtin Elisp parsing and tree-sitter
   - Using tree-sitter as a backend is recommended as it is much faster, efficient and accurate
- Tags collection for completion and navigation of definitions and references requires tree-sitter

For information about installation of `vhdl-ts-mode` check its
[repo](https://github.com/gmlarumbe/vhdl-ts-mode).


## Project management ##

The package provides the variable `vhdl-ext-project-alist` to
select which files belong to a specific project:

  ```elisp
  (setq vhdl-ext-project-alist
        `(("axi_if_converter" ; Project name
           :root "/home/gonz/Repos/larumbe/axi_if_converter" ; supports remote dirs via Tramp
           :files ("src/my_block.vhd"
                   "src/*.vhd") ; Multiple files can be specified through the glob pattern
           :dirs ("src/tb"
                  "-r src/rtl" ; -r to add directories recursively
                  "src/syn/*_block"
                  "src/**/netlists") ; add all dirs that begin with "src" and end with "netlists"
           :ignore-dirs ("src/ignored_ip")
           :ignore-files ("src/some_ip/ignored_sim_netlist.vhd")
           ;; The ones below are used for GHDL-related features
           :worklib "xil_defaultlib"                     ; Defaults to `work' if not set
           :workdir "library/xil_defaultlib"             ; Output compilation directory for worklib
           :lib-search-path ("/opt/ghdl_Xilinx_lib/")))) ; Extra directories to look for compiled libraries
  ```

The different properties for each project entry determine which files will be used
for some features of the package, such as completion, xref navigation, hierarchy extraction and compilation.


## Syntax highlighting ##

<img src="https://user-images.githubusercontent.com/51021955/215353070-8a21f758-407d-4455-bdac-bf92310c59e4.gif" width=80%>

For configuration information, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Syntax-highlighting).


## Find definitions and references ##

`vhdl-ext` provides a builtin `xref` backend to navigate definitions and references of current project in `vhdl-ext-project-alist`.

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/7fde4bc6-0c84-4da0-8e37-6a0e0dac36f6" width=80%>

For configuration information, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Xref).


## Auto-completion ##

Complete with tags from current VHDL project.

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/4c908a17-df9e-4fb3-8263-e5e302700ac9" width=80%>

For configuration information, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Completion).


## Hierarchy extraction ##

Hierarchy extraction of entity at current buffer.

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/79018dc4-833a-4ce6-9f2b-3195ba75481d" width=80%>

For configuration information, see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Hierarchy).


## Language Server Protocol ##

Auto-configure various VHDL language servers for `lsp-bridge`, `lsp-mode` and `eglot`:

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

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/cffb0c9d-3a39-4e58-b422-fb275db124a8" width=80%>

Interactive functions:

* `vhdl-ext-beautify-block-at-point`: <kbd>C-M-i</kbd>
* `vhdl-ext-beautify-instance-at-point`

Batch-mode functions:

* `vhdl-ext-beautify-files`
* `vhdl-ext-beautify-dir-files`: uses tree-sitter if run with prefix arg <kbd>C-u</kbd>


## Navigation ##

<img src="https://user-images.githubusercontent.com/51021955/215353135-446b678b-e3be-42f3-8009-5d5bd7c5e5bd.gif" width=80%>

* Navigate instances inside an entity
* Jump to definition/references of entity at point
* Jump to parent entity

For detailed info see the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Navigation).


## Templates ##
Snippet selection via `hydra`.

<img src="https://user-images.githubusercontent.com/51021955/215353124-7e374754-cd91-4924-9b4b-3c6a29cad921.gif" width=80%>

* `vhdl-ext-hydra/body`: <kbd>C-c C-t</kbd>


## Compilation ##

Provides functions to perform compilations with syntax highlighting
and jump to error:

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/845980ab-c54b-4e89-b53f-056140be87a7" width=80%>

* `vhdl-ext-compile-project-ghdl`: <kbd>C-c \<f5\></kbd>

See more info in the [wiki](https://github.com/gmlarumbe/vhdl-ext/wiki/Compilation).


## Imenu ##
Support detection of instances.

<img src="https://user-images.githubusercontent.com/51021955/215353082-9a187daf-7f76-4c9b-8563-7beba6e1aa6a.gif" width=80%>


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

<img src="https://github.com/gmlarumbe/vhdl-ext/assets/51021955/14536463-a6c4-410f-a890-081f7deb668e" width=70%>

  * `vhdl-ext-ports-toggle-connect`: <kbd>C-c C-c t</kbd>
  * `vhdl-ext-ports-connect-recursively`: <kbd>C-c C-c r</kbd>


# Contributing #

Contributions are welcome! Just stick to common Elisp conventions and run the ERT suite after testing your changes and before submitting a new PR.

For new functionality add new ERT tests if possible.

Consider [sponsoring](https://github.com/sponsors/gmlarumbe) to help
maintaining the project and for the development of new features. *Thank you!*

### Setup ###

To run the whole ERT test suite change directory to the `vhdl-ext`
root and make sure `test-hdl` Git submodule has been loaded:

```shell
git submodule update --init
```

### Targets ###

Then run the default target:

```shell
$ make
```

To run a subset of tests (e.g. navigation):

```shell
$ make TESTS=navigation
```

To regenerate all the expected outputs for the tests:

```shell
$ make gen
```

To regenerate the expected outputs for a group of tests (e.g. navigation):

```shell
$ make gen TESTS=navigation
```

## Other packages ##
* [verilog-ts-mode](https://github.com/gmlarumbe/verilog-ts-mode): SystemVerilog Tree-sitter mode
* [vhdl-ts-mode](https://github.com/gmlarumbe/vhdl-ts-mode): VHDL Tree-sitter mode
* [verilog-ext](https://github.com/gmlarumbe/verilog-ext): SystemVerilog Extensions
* [fpga](https://github.com/gmlarumbe/fpga): FPGA & ASIC Utilities for tools of major vendors and open source
* [wavedrom-mode](https://github.com/gmlarumbe/wavedrom-mode): edit and render WaveJSON files to create timing diagrams
* [vunit-mode](https://github.com/embed-me/vunit-mode.git): Integration of [VUnit](https://github.com/VUnit/vunit) workflow
