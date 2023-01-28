;;; vhdl-ext-tests.el --- vhdl-ext ERT tests  -*- lexical-binding: t -*-

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
;; ERT Tests
;;
;;; Code:


;;;; Performance utils
(require 'profiler)

(defun vhdl-ext-profile-file (file)
  "Use Emacs profiler in FILE."
  (profiler-start 'cpu+mem)
  (find-file file)
  (profiler-stop)
  (profiler-report))

;;;; Tests
(require 'vhdl-ext)

(defvar vhdl-ext-tests-root-dir (file-name-directory (locate-library "vhdl-ext")))
(defvar vhdl-ext-tests-test-dir (if (string-prefix-p (expand-file-name straight-base-dir) vhdl-ext-tests-root-dir)
                                       (vhdl-ext-path-join (expand-file-name straight-base-dir) "straight/repos/vhdl-ext/tests")
                                     (vhdl-ext-path-join vhdl-ext-tests-root-dir "tests")))
(defvar vhdl-ext-tests-examples-dir (vhdl-ext-path-join vhdl-ext-tests-test-dir "examples"))
(defvar vhdl-ext-tests-faceup-dir (vhdl-ext-path-join vhdl-ext-tests-test-dir "examples/faceup"))

(unless (member vhdl-ext-tests-test-dir load-path)
  (add-to-list 'load-path vhdl-ext-tests-test-dir))

(require 'vhdl-ext-tests-imenu)
(require 'vhdl-ext-tests-navigation)
(require 'vhdl-ext-tests-font-lock)
(require 'vhdl-ext-tests-utils)

(message "Emacs version: %s" emacs-version)
(if (< emacs-major-version 29)
    (message "Skipping vhdl-ext-tests-tree-sitter...")
  ;; Else
  (message "(treesit-available-p): %s" (treesit-available-p))
  (when (treesit-available-p)
    (require 'treesit)
    (message "(treesit-language-available-p 'vhdl): %s" (treesit-language-available-p 'vhdl))
    (when (treesit-language-available-p 'vhdl)
      (require 'vhdl-ext-tests-tree-sitter))))


(provide 'vhdl-ext-tests)

;;; vhdl-ext-tests.el ends here
