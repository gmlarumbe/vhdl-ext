;;; vhdl-ext-flycheck.el --- VHDL Flycheck -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/vhdl-ext

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

;; Add support for the following linters in `flycheck':
;;  - ghdl (overrides default parameters)
;;  - vhdl_lang
;;  - vhdl-tool

;;; Code:

(require 'flycheck)
(require 'vhdl-mode)
(require 'vhdl-ext-utils)


;;; Custom
(defgroup vhdl-ext-flycheck nil
  "Vhdl-ext flycheck."
  :group 'vhdl-ext)

(defcustom vhdl-ext-flycheck-use-open-buffers nil
  "Set to non-nil to use list of open VHDL buffers/dirs for linters."
  :type 'boolean
  :group 'vhdl-ext-flycheck)

(defcustom vhdl-ext-flycheck-ghdl-include-path nil
  "List of include paths for GHDL linter."
  :type '(repeat directory)
  :group 'vhdl-ext-flycheck)

(defcustom vhdl-ext-flycheck-ghdl-extra-args nil
  "List of extra arguments for GHDL linter."
  :type '(repeat string)
  :group 'vhdl-ext-flycheck)


;;; Vars
(defvar vhdl-ext-flycheck-linter 'vhdl-ghdl
  "Vhdl-ext flycheck linter.")

(defconst vhdl-ext-flycheck-linters '(vhdl-ghdl
                                      vhdl-lang
                                      vhdl-tool)
  "List of supported linters.")

(defvar vhdl-ext-flycheck-dirs nil "List of open dirs for `vhdl-ext-flycheck'.")
(defvar vhdl-ext-flycheck-files nil "List of open files for `vhdl-ext-flycheck'.")

;;; Linters
;;;; GHDL
(flycheck-def-option-var flycheck-ghdl-work-lib vhdl-default-library vhdl-ghdl
  "Work library name to be used for GHDL."
  :type '(choice (const :tag "Default library" vhdl-default-library)
                 (string :tag "Custom work library"))
  :safe #'stringp
  :package-version '(flycheck . "32"))
(make-variable-buffer-local 'flycheck-ghdl-work-lib)

(flycheck-define-checker vhdl-ghdl
  "A VHDL syntax checker using GHDL.
See URL `https://github.com/ghdl/ghdl'."
  :command ("ghdl"
            "-s" ; only do the syntax checking
            (option "--std=" flycheck-ghdl-language-standard concat) ; Set by `vhdl-ext' based on `vhdl-standard'
            (option "--workdir=" flycheck-ghdl-workdir concat) ; Set by `vhdl-ext' based on `vhdl-ext-project-alist'
            (option "--ieee=" flycheck-ghdl-ieee-library concat)
            ;; Additional options
            (option "--work=" flycheck-ghdl-work-lib concat) ; Set by `vhdl-ext' based on `vhdl-ext-project-alist'
            (eval vhdl-ext-flycheck-ghdl-extra-args)
            ;; Extra dirs and files
            (option-list "-P" vhdl-ext-flycheck-ghdl-include-path concat)
            (option-list "-P" vhdl-ext-flycheck-dirs concat)
            source)
  :error-patterns
  ((info    line-start (file-name) ":" line ":" column ":note: "    (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":warning: " (message) line-end)
   (error   line-start (file-name) ":" line ":" column ":error: "   (message) line-end))
  :modes (vhdl-mode vhdl-ts-mode))


;;;; vhdl_lang
(flycheck-def-config-file-var flycheck-vhdl-lang-config-file vhdl-lang "vhdl_ls.toml")

(flycheck-define-checker vhdl-lang
  "Rust_hdl VHDL Language Frontend.

See URL `https://github.com/VHDL-LS/rust_hdl'."
  :command ("vhdl_lang"
            (config-file "--config" flycheck-vhdl-lang-config-file))
  :standard-input t
  :error-patterns
  ((info    line-start "hint: "    (message) "\n" "   --> " (file-name) ":" line line-end)
   (warning line-start "warning: " (message) "\n" "   --> " (file-name) ":" line line-end)
   (error   line-start "error: "   (message) "\n" "   --> " (file-name) ":" line line-end))
  :modes (vhdl-mode vhdl-ts-mode))


;;;; vhdl-tool
(defun vhdl-ext-flycheck-vhdl-tool-directory-fn (_checker)
  "Return directory where vhdl-tool is executed.
Needed to keep in sync where the server and the client are run."
  (vhdl-ext-buffer-proj-root))

;; https://git.vhdltool.com/vhdl-tool/configs/src/master/emacs
(flycheck-define-checker vhdl-tool
  "A VHDL syntax checker, type checker and linter using VHDL-Tool.

See URL `http://vhdltool.com'."
  :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source)
  :standard-input t
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
   (error   line-start (file-name) ":" line ":" column ":e:" (message) line-end))
  :modes (vhdl-mode vhdl-ts-mode)
  :working-directory vhdl-ext-flycheck-vhdl-tool-directory-fn)


;;; Functions
(defun vhdl-ext-flycheck-setup-linter (linter)
  "Setup LINTER before enabling flycheck."
  (pcase linter
    ('vhdl-lang
     (unless (locate-dominating-file buffer-file-name flycheck-vhdl-lang-config-file)
       (error "Could not find \"vhdl_ls.toml\" in project root!")))
    ('vhdl-tool
     ;; Start "vhdl-tool server" process, creating vhdltool.sock pipe file in project root
     ;;  - It might be necessary to reload the vhdl-tool linter to wait until previous process has been set up
     (let ((buf (concat " *vhdl-tool-server@" (vhdl-ext-buffer-proj-root) "*")))
       (unless (get-buffer-process buf)
         (start-process-shell-command buf buf (mapconcat #'identity `("cd" ,(vhdl-ext-buffer-proj-root) "&&" "vhdl-tool" "server") " "))
         (message "Started process @ %s" buf))))))

(defun vhdl-ext-flycheck-set-linter (&optional linter)
  "Set LINTER as default and enable it if flycheck is on."
  (interactive)
  (unless linter
    (setq linter (intern (completing-read "Select linter: " vhdl-ext-flycheck-linters nil t))))
  (unless (member linter vhdl-ext-flycheck-linters)
    (error "Linter %s not available" linter))
  ;; Set it at the head of the list
  (delete linter flycheck-checkers)
  (add-to-list 'flycheck-checkers linter)
  (vhdl-ext-flycheck-setup-linter linter)
  (setq vhdl-ext-flycheck-linter linter) ; Save state for reporting
  ;; Refresh linter if in a vhdl buffer
  (when (member major-mode '(vhdl-mode vhdl-ts-mode))
    (when (eq linter 'vhdl-tool)
      (sit-for 0.5)) ; Wait until "vhdl-tool server" process has been properly set up
    (flycheck-select-checker linter))
  (message "Linter set to: %s " linter))

(defun vhdl-ext-flycheck-setup ()
  "Add available linters from `vhdl-ext-flycheck-linters' and set default one."
  (interactive)
  (dolist (checker vhdl-ext-flycheck-linters)
    (add-to-list 'flycheck-checkers checker))
  (vhdl-ext-flycheck-set-linter vhdl-ext-flycheck-linter))

(defun vhdl-ext-flycheck-mode (&optional uarg)
  "`flycheck-mode' VHDL wrapper function.
If called with UARG select among available linters and enable flycheck."
  (interactive "P")
  (let (enable)
    (when buffer-read-only
      (error "Flycheck does not work on read-only buffers!"))
    (if uarg
        (progn
          (vhdl-ext-flycheck-set-linter)
          (setq enable t))
      (unless flycheck-mode
        (setq enable t)))
    (when (flycheck-disabled-checker-p vhdl-ext-flycheck-linter)
      (user-error "[%s] Current checker is disabled by flycheck.\nEnable it with C-u M-x `flycheck-disable-checker'" vhdl-ext-flycheck-linter))
    (if enable
        (progn
          (flycheck-mode 1)
          (message "[%s] Flycheck enabled" vhdl-ext-flycheck-linter))
      (flycheck-mode -1)
      (message "Flycheck disabled"))))


(provide 'vhdl-ext-flycheck)

;;; vhdl-ext-flycheck.el ends here
