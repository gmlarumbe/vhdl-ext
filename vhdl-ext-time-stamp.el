;;; vhdl-ext-time-stamp.el --- Vhdl-ext Time-stamp  -*- lexical-binding: t -*-

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

;; `time-stamp' setup.

;; Serves the same function as `vhdl-modify-date-on-saving' but with extra
;; parameters and built-in `time-stamp' package for more flexibility.

;;; Code:

(require 'vhdl-mode)


(defgroup vhdl-ext-time-stamp nil
  "Vhdl-ext time-stamp."
  :group 'vhdl-ext)

(defcustom vhdl-ext-time-stamp-enable t
  "Enable `vhdl-ext-time-stamp-mode'."
  :type 'boolean
  :group 'vhdl-ext-time-stamp)

(defcustom vhdl-ext-time-stamp-regex "^-- Last update: "
  "`time-stamp' regexp."
  :type 'string
  :group 'vhdl-ext-time-stamp)

(defcustom vhdl-ext-time-stamp-pattern (concat vhdl-ext-time-stamp-regex "%%$")
  "`time-stamp' pattern.  See `time-stamp-pattern'."
  :type 'string
  :group 'vhdl-ext-time-stamp)

(defcustom vhdl-ext-time-stamp-format  "%:y-%02m-%02d"
  "`time-stamp' format.  See `time-stamp-format'."
  :type 'string
  :group 'vhdl-ext-time-stamp)

(defcustom vhdl-ext-time-stamp-start nil
  "If using `time-stamp-start' and `time-stamp-end':
`'time-stamp' deletes the text between the first match of `time-stamp-start'.
and the following match of `time-stamp-end', then writes the time stamp
specified by `time-stamp-format' between them."
  :type 'string
  :group 'vhdl-ext-time-stamp)

(defcustom vhdl-ext-time-stamp-end nil
  "If using `time-stamp-start' and `time-stamp-end':
`time-stamp' deletes the text between the first match of `time-stamp-start'.
and the following match of `time-stamp-end', then writes the time stamp
specified by `time-stamp-format' between them."
  :type 'string
  :group 'vhdl-ext-time-stamp)


(defun vhdl-ext-time-stamp-hook ()
  "Intermediary hook to preserve correct order of local hook value setting.

This is needed when mixing Emacs local hooks in file local variables and using
`add-hook' with the local argument."
  (when (derived-mode-p 'vhdl-mode)
    (add-hook 'before-save-hook #'time-stamp nil :local)))

(define-minor-mode vhdl-ext-time-stamp-mode
  "Setup `time-stamp' format for Vhdl files.
By default `time-stamp' looks for the pattern in the first 8 lines.
This can be changed by setting the local variables `time-stamp-start'
and `time-stamp-end' for custom scenarios.

Skip configuration if `vhdl-modify-date-on-saving' is non-nil.
Use only one method of timestamp update."
  :global nil
  (if vhdl-modify-date-on-saving
      (warn "vhdl-ext-time-stamp incompatible with `vhdl-modify-date-on-saving'.  Disable one of both.")
    (setq-local time-stamp-pattern vhdl-ext-time-stamp-pattern)
    (setq-local time-stamp-format vhdl-ext-time-stamp-format)
    (setq-local time-stamp-start vhdl-ext-time-stamp-start)
    (setq-local time-stamp-end vhdl-ext-time-stamp-end)
    (if vhdl-ext-time-stamp-mode
        (add-hook 'hack-local-variables-hook #'vhdl-ext-time-stamp-hook)
      (remove-hook 'hack-local-variables-hook #'vhdl-ext-time-stamp-hook))))


(provide 'vhdl-ext-time-stamp)

;;; vhdl-ext-time-stamp.el ends here
