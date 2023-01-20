;;; vhdl-flycheck.el --- VHDL Flycheck  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/verilog-ext

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


(defun vhdl-ext-flycheck-dirs-of-open-buffers ()
  "Return a list of directories from current VHDL opened files.
Used for `ghdl' linter flycheck include directories."
  (let ((vhdl-opened-dirs nil))
    (dolist ($buf (buffer-list (current-buffer)))
      (with-current-buffer $buf
        (when (string-equal major-mode "vhdl-mode")
          (setq vhdl-opened-dirs (push default-directory vhdl-opened-dirs)))))
    vhdl-opened-dirs))

(defun vhdl-ext-flycheck-get-standard ()
  "Get current standard as a string from `vhdl-standard'."
  (let ((std (car vhdl-standard)))
    (if (equal std 8)
        (format "0%s" std)
      (format "%s" std))))


(flycheck-def-option-var flycheck-ghdl-include-path nil vhdl-ghdl
  "A list of include directories for GHDL.

List of strings where each is a directory to be added to the include path of
GHDL."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "32"))

(flycheck-def-option-var flycheck-ghdl-work-lib vhdl-default-library vhdl-ghdl
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
            (option-list "-P" flycheck-ghdl-include-path concat)
            (option "--work=" flycheck-ghdl-work-lib concat)
            source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) line-end))
  :modes vhdl-mode)


(defun vhdl-ext-flycheck-ghdl-hook ()
  "Set GHDL flycheck options based on context."
  (setq flycheck-ghdl-language-standard (vhdl-ext-flycheck-get-standard))
  (setq flycheck-ghdl-include-path (vhdl-ext-flycheck-dirs-of-open-buffers))
  (setq flycheck-ghdl-work-lib (vhdl-work-library)))



(provide 'vhdl-flycheck)

;;; vhdl-flycheck.el ends here
