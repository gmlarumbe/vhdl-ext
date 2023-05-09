;;; vhdl-ext-flycheck.el --- VHDL Flycheck -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/vhdl-ext
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;; Flycheck additional linters

;;; Code:

(require 'flycheck)
(require 'vhdl-mode)

;;;; GHDL
(defcustom vhdl-ext-flycheck-extra-include nil
  "Extra includes for GHDL flycheck."
  :type '(repeat (directory))
  :group 'vhdl-ext)

;; Overriding of `vhdl-ghdl' syntax checker to add more options
(flycheck-def-option-var vhdl-ext-flycheck-ghdl-include-path nil vhdl-ghdl
  "A list of include directories for GHDL.

List of strings where each is a directory to be added to the include path of
GHDL."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var vhdl-ext-flycheck-ghdl-work-lib vhdl-default-library vhdl-ghdl
  "Work library name to be used for GHDL."
  :type '(choice (const :tag "Default library" vhdl-default-library)
                 (string :tag "Custom work library"))
  :safe #'stringp
  :package-version '(flycheck . "32"))

(flycheck-def-args-var vhdl-ext-flycheck-ghdl-extra-args vhdl-ghdl
  :package-version '(flycheck . "32"))

(flycheck-define-checker vhdl-ghdl
  "A VHDL syntax checker using GHDL.
See URL `https://github.com/ghdl/ghdl'."
  :command ("ghdl"
            "-s" ; only do the syntax checking
            (option "--std=" flycheck-ghdl-language-standard concat)
            (option "--workdir=" flycheck-ghdl-workdir concat)
            (option "--ieee=" flycheck-ghdl-ieee-library concat)
            ;; Additional options
            (option-list "-P" vhdl-ext-flycheck-ghdl-include-path concat)
            (option "--work=" vhdl-ext-flycheck-ghdl-work-lib concat)
            ;; Extra args
            (eval vhdl-ext-flycheck-ghdl-extra-args)
            source)
  :error-patterns
  ((info    line-start (file-name) ":" line ":" column ":note: "    (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":warning: " (message) line-end)
   (error   line-start (file-name) ":" line ":" column ": "         (message) line-end))
  :modes (vhdl-mode vhdl-ts-mode))


;;;; vhdl_lang
(flycheck-def-config-file-var flycheck-vhdl-lang-config-file vhdl-lang "vhdl_lang.toml")

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
;; https://git.vhdltool.com/vhdl-tool/configs/src/master/emacs
;; INFO: Requires running following command in the background:
;;  $ vhdl-tool server
(flycheck-define-checker vhdl-tool
  "A VHDL syntax checker, type checker and linter using VHDL-Tool.

See URL `http://vhdltool.com'."
  :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source)
  :standard-input t
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
   (error   line-start (file-name) ":" line ":" column ":e:" (message) line-end))
  :modes (vhdl-mode vhdl-ts-mode))


(provide 'vhdl-ext-flycheck)

;;; vhdl-ext-flycheck.el ends here
