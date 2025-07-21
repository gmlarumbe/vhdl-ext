;;; vhdl-ext.el --- VHDL Extensions -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/vhdl-ext
;; Version: 0.6.1
;; Keywords: VHDL, IDE, Tools
;; Package-Requires: ((emacs "29.1") (vhdl-ts-mode "0.3.1") (lsp-mode "8.0.0") (ag "0.48") (ripgrep "0.4.0") (hydra "0.15.0") (flycheck "32") (async "1.9.7"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for VHDL Mode:
;;
;;  - Tree-sitter `vhdl-ts-mode' support
;;  - Project management
;;  - Improved syntax highlighting for `vhdl-mode'
;;  - Find definitions and references with builtin `xref' backend
;;  - Auto-completion with semantic completion
;;  - Hierarchy extraction and navigation
;;  - LSP configuration for `lsp-bridge', `lsp-mode', `eglot' and `lspce'
;;  - Support for many linters via `flycheck'
;;  - Beautify blocks and instances
;;  - Code navigation functions
;;  - Templates insertion via `hydra'
;;  - Compilation-based utilities
;;  - Improve `imenu': detect instances
;;  - Add support for `which-func'
;;  - Improve code folding via `hideshow'
;;  - Project tags and caching
;;  - `time-stamp' auto-configuration
;;  - Port connections utilities

;;; Code:

;;; Customization
(defgroup vhdl-ext nil
  "VHDL Extensions."
  :group 'languages
  :group 'vhdl-mode)

(defcustom vhdl-ext-feature-list '(font-lock
                                   xref
                                   capf
                                   hierarchy
                                   eglot
                                   lsp
                                   lsp-bridge
                                   lspce
                                   flycheck
                                   beautify
                                   navigation
                                   template
                                   compilation
                                   imenu
                                   which-func
                                   hideshow
                                   time-stamp
                                   ports)
  "Which `vhdl-ext' features to enable."
  :type '(set (const :tag "Improved syntax highlighting via `font-lock'."
                font-lock)
              (const :tag "Xref backend to navigate definitions/references in current project."
                xref)
              (const :tag "Completion at point builtin function with semantic completion."
                capf)
              (const :tag "Hierarchy extraction and visualization."
                hierarchy)
              (const :tag "Setup LSP servers for `eglot'."
                eglot)
              (const :tag "Setup LSP servers for `lsp-mode'."
                lsp)
              (const :tag "Setup LSP servers for `lsp-bridge'."
                lsp-bridge)
              (const :tag "Setup LSP servers for `lspce'."
                lspce)
              (const :tag "Setup linters for `flycheck'."
                flycheck)
              (const :tag "Code beautifying functions."
                beautify)
              (const :tag "Code Navigation functions."
                navigation)
              (const :tag "Custom templates via `hydra'."
                template)
              (const :tag "Compilation functions."
                compilation)
              (const :tag "Improved `imenu'."
                imenu)
              (const :tag "Support for `which-function-mode'."
                which-func)
              (const :tag "`hideshow' configuration."
                hideshow)
              (const :tag "`time-stamp' configuration."
                time-stamp)
              (const :tag "Port connections utilities."
                ports))
  :group 'vhdl-ext)

(defmacro vhdl-ext-when-feature (features &rest body)
  "Macro to run BODY if `vhdl-ext' feature is enabled.
FEATURES can be a single feature or a list of features."
  (declare (indent 1) (debug 1))
  `(let (enabled)
     (if (listp ,features)
         (dolist (feature ,features)
           (when (member feature vhdl-ext-feature-list)
             (setq enabled t)))
       ;; Else
       (when (member ,features vhdl-ext-feature-list)
         (setq enabled t)))
     (when enabled
       ,@body)))

;;; Features
(require 'vhdl-ext-hs)
(require 'vhdl-ext-time-stamp)
(require 'vhdl-ext-utils)
(require 'vhdl-ext-compile)
(require 'vhdl-ext-nav)
(require 'vhdl-ext-font-lock)
(require 'vhdl-ext-imenu)
(require 'vhdl-ext-which-func)
(require 'vhdl-ext-ports)
(require 'vhdl-ext-beautify)
(require 'vhdl-ext-template)
(require 'vhdl-ext-hierarchy)
(require 'vhdl-ext-tags)
(require 'vhdl-ext-capf)
(require 'vhdl-ext-xref)
(require 'vhdl-ext-flycheck)
(require 'vhdl-ext-eglot)
(require 'vhdl-ext-lsp)
(require 'vhdl-ext-lsp-bridge)
(require 'vhdl-ext-lspce)


;;; Major-mode
(defvar vhdl-ext-mode-map
  (let ((map (make-sparse-keymap)))
    (vhdl-ext-when-feature '(capf xref)
      (define-key map (kbd "C-c C-u") 'vhdl-ext-tags-get))
    (vhdl-ext-when-feature 'hierarchy
      (define-key map (kbd "C-c C-v") 'vhdl-ext-hierarchy-current-buffer))
    (vhdl-ext-when-feature 'flycheck
      (define-key map (kbd "C-c C-f") 'vhdl-ext-flycheck-mode))
    (vhdl-ext-when-feature 'template
      (define-key map (kbd "C-c C-t") 'vhdl-ext-hydra/body))
    (vhdl-ext-when-feature 'beautify
      (define-key map (kbd "C-M-i") 'vhdl-ext-beautify-block-at-point))
    (vhdl-ext-when-feature 'navigation
      (define-key map (kbd "C-M-.") 'vhdl-ext-jump-to-parent-entity)
      (define-key map (kbd "C-c M-.") 'vhdl-ext-jump-to-entity-at-point-def)
      (define-key map (kbd "C-c M-?") 'vhdl-ext-jump-to-entity-at-point-ref)
      (define-key map (kbd "C-M-f") 'vhdl-ext-forward-sexp)
      (define-key map (kbd "C-M-b") 'vhdl-ext-backward-sexp)
      (define-key map (kbd "C-M-u") 'vhdl-ext-find-entity-instance-bwd)
      (define-key map (kbd "C-M-d") 'vhdl-ext-find-entity-instance-fwd))
    (vhdl-ext-when-feature 'compilation
      (define-key map (kbd "C-c <f5>") 'vhdl-ext-compile-project-ghdl))
    (vhdl-ext-when-feature 'ports
      (define-key map (kbd "C-c C-c t") 'vhdl-ext-ports-toggle-connect)
      (define-key map (kbd "C-c C-c r") 'vhdl-ext-ports-connect-recursively))
    map)
  "Keymap for `vhdl-ext'.")

;;;###autoload
(defun vhdl-ext-mode-setup ()
  "Setup `vhdl-ext-mode' depending on enabled features."
  (interactive)
  ;; Features
  (vhdl-ext-when-feature 'font-lock
    (vhdl-ext-font-lock-setup))
  (vhdl-ext-when-feature '(capf xref)
    (vhdl-ext-tags-setup))
  (vhdl-ext-when-feature 'hierarchy
    (vhdl-ext-hierarchy-setup))
  (vhdl-ext-when-feature 'eglot
    (vhdl-ext-eglot-set-server vhdl-ext-eglot-default-server))
  (vhdl-ext-when-feature 'lsp
    (vhdl-ext-lsp-setup)
    (vhdl-ext-lsp-set-server vhdl-ext-lsp-mode-default-server))
  (vhdl-ext-when-feature 'lsp-bridge
    (vhdl-ext-lsp-bridge-set-server vhdl-ext-lsp-bridge-default-server))
  (vhdl-ext-when-feature 'lspce
    (vhdl-ext-lspce-set-server vhdl-ext-lspce-default-server))
  (vhdl-ext-when-feature 'flycheck
    (vhdl-ext-flycheck-setup))
  (vhdl-ext-when-feature 'hideshow
    (vhdl-ext-hs-setup))
  ;; Jump to parent module ag/ripgrep hooks
  (add-hook 'ag-search-finished-hook #'vhdl-ext-navigation-ag-rg-hook)
  (add-hook 'ripgrep-search-finished-hook #'vhdl-ext-navigation-ag-rg-hook))

;;;###autoload
(define-minor-mode vhdl-ext-mode
  "Minor mode for editing VHDL files.

\\{vhdl-ext-mode-map}"
  :lighter " vX"
  :global nil
  (if vhdl-ext-mode
      (progn
        ;; Common
        (vhdl-ext-update-buffer-file-and-dir-list)
        (add-hook 'kill-buffer-hook #'vhdl-ext-kill-buffer-hook nil :local)
        ;; Features
        (vhdl-ext-when-feature 'xref
          (vhdl-ext-xref-set))
        (vhdl-ext-when-feature 'capf
          (vhdl-ext-capf-set))
        (vhdl-ext-when-feature 'flycheck
          (if vhdl-ext-flycheck-use-open-buffers
              (progn (setq vhdl-ext-flycheck-dirs vhdl-ext-dir-list)
                     (setq vhdl-ext-flycheck-files vhdl-ext-file-list))
            (setq vhdl-ext-flycheck-dirs nil)
            (setq vhdl-ext-flycheck-files nil))
          (setq flycheck-ghdl-language-standard (vhdl-ext-get-standard)) ; Global for all projects
          (setq flycheck-ghdl-workdir (vhdl-ext-proj-workdir))           ; Project dependant
          (setq flycheck-ghdl-work-lib (vhdl-ext-proj-worklib)))         ; Project dependant
        (vhdl-ext-when-feature 'time-stamp
          (vhdl-ext-time-stamp-mode))
        ;; `vhdl-mode'-only customization (exclude `vhdl-ts-mode')
        (when (eq major-mode 'vhdl-mode)
          ;; Font-lock
          ;;   It's not possible to add font-lock keywords to minor-modes.
          ;;   The workaround consists in add/remove keywords to the major mode when
          ;;   the minor mode is loaded/unloaded.
          ;;   https://emacs.stackexchange.com/questions/60198/font-lock-add-keywords-is-not-working
          (vhdl-ext-when-feature 'font-lock
            (font-lock-flush)
            (setq-local font-lock-multiline nil))
          ;; Imenu
          (vhdl-ext-when-feature 'imenu
            (advice-add 'vhdl-index-menu-init :override #'vhdl-ext-index-menu-init))
          ;; Which-func
          (vhdl-ext-when-feature 'which-func
            (vhdl-ext-which-func))))
    ;; Cleanup
    (remove-hook 'kill-buffer-hook #'vhdl-ext-kill-buffer-hook :local)
    (vhdl-ext-when-feature 'xref
      (vhdl-ext-xref-set :disable))
    (vhdl-ext-when-feature 'capf
      (vhdl-ext-capf-set :disable))
    (vhdl-ext-when-feature 'imenu
      (advice-remove 'vhdl-index-menu-init #'vhdl-ext-index-menu-init))
    (vhdl-ext-when-feature 'time-stamp
      (vhdl-ext-time-stamp-mode -1))))


;;; Provide
(provide 'vhdl-ext)

;;; vhdl-ext.el ends here

