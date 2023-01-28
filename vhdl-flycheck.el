;;; vhdl-flycheck.el --- VHDL Flycheck  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

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
;;
;; Overriding of `vhdl-ghdl' syntax checker to add more options
;;
;;; Code:


(require 'flycheck)
(require 'vhdl-mode)
(require 'vhdl-utils)


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
            source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes (vhdl-mode vhdl-ts-mode))



(provide 'vhdl-flycheck)

;;; vhdl-flycheck.el ends here
