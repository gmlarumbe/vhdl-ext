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

;;;; Native compile
(defun vhdl-ext-compile-dir (dir)
  "Compile DIR.
Native compile if native compilation is available.
Otherwise, byte-compile."
  (if (native-comp-available-p)
      (dolist (file (directory-files-recursively dir "\.el$"))
        (message "Native compiling %s" file)
        (native-compile file))
    ;; Nix Emacs images might still lack native compilation support, so byte-compile them
    (message "Byte-compiling %s" dir)
    (byte-recompile-directory dir 0)))

;;;; Tests
(require 'vhdl-ext)

(defvar vhdl-ext-tests-test-dir (if (bound-and-true-p straight-base-dir)
                                    (file-name-concat (expand-file-name straight-base-dir) "straight/repos/vhdl-ext/test")
                                  (file-name-directory (or load-file-name (buffer-file-name)))))
(defvar vhdl-ext-tests-files-dir (file-name-concat vhdl-ext-tests-test-dir "files"))
(defvar vhdl-ext-tests-common-dir (file-name-concat vhdl-ext-tests-files-dir "common"))
(defvar vhdl-ext-tests-faceup-dir (file-name-concat vhdl-ext-tests-files-dir "faceup"))
(defvar vhdl-ext-tests-jump-parent-dir (file-name-concat vhdl-ext-tests-files-dir "jump-parent"))

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
      (message "(functionp 'vhdl-ts-mode): %s" (functionp 'vhdl-ts-mode))
      (when (functionp 'vhdl-ts-mode)
        (require 'vhdl-ext-tests-tree-sitter)))))


(provide 'vhdl-ext-tests)

;;; vhdl-ext-tests.el ends here
