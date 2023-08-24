;;; vhdl-ext-tests-indent.el --- Vhdl-Ext ERT Indent tests  -*- lexical-binding: t -*-

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
;; ERT indent tests:
;;
;;; Code:


(defvar vhdl-ext-tests-indent-test-files
  (append (directory-files vhdl-ext-tests-common-dir t ".vhdl?$")
          (directory-files (file-name-concat vhdl-ext-tests-indent-dir "tree-sitter") t ".vhdl?$")))
(defvar vhdl-ext-tests-indent-dump-dir (file-name-concat vhdl-ext-tests-indent-dir "dump"))
(defvar vhdl-ext-tests-indent-dump-diff-on-error t)


(defun vhdl-ext-tests-indent-ref-file-from-orig (file &optional tree-sitter)
  (concat (file-name-nondirectory (file-name-sans-extension file))
          (when tree-sitter
            ".ts")
          ".indent.vhd"))

(defun vhdl-ext-test-indent-buffer (&optional tree-sitter)
  "Perform indentation of current buffer for indentation tests."
  (let ((debug nil))
    (when debug
      (clone-indirect-buffer-other-window "*debug*" t))
    ;; De-indent original file
    (goto-char (point-min))
    (while (< (point) (point-max))
      (delete-horizontal-space)
      (forward-line 1))
    ;; Re-indent buffer
    (if tree-sitter
        (progn
          (vhdl-ts-mode)
          (indent-region (point-min) (point-max)))
      ;; Else, vhdl-mode and syntax-table overriding
      (vhdl-mode)
      (vhdl-ext-indent-region (point-min) (point-max)))
    ;; Untabify and clean whitespace
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))))

(defun vhdl-ext-test-indent-gen-expected-files (&optional tree-sitter)
  "Update .indent files manually."
  (save-window-excursion
    (dolist (file vhdl-ext-tests-indent-test-files)
      (let ((indented-file (file-name-concat vhdl-ext-tests-indent-dir
                                             (vhdl-ext-tests-indent-ref-file-from-orig file tree-sitter))))
        (message "Processing %s" file)
        (with-temp-file indented-file
          (insert-file-contents file)
          (vhdl-ext-test-indent-buffer tree-sitter))))))

(defun vhdl-ext-test-indent-file (file &optional tree-sitter)
  "Expects FILE absolute path."
  (let* ((verbose nil)
         (dump-file (file-name-concat vhdl-ext-tests-indent-dump-dir
                                           (file-name-nondirectory file))))
    (when verbose
      (message "Indenting %s..." file))
    (cl-letf (((symbol-function 'message)
               (lambda (FORMAT-STRING &rest ARGS)
                 nil))) ; Mock `message' to silence all the indentation reporting
      (with-temp-file dump-file
        (insert-file-contents file)
        (vhdl-ext-test-indent-buffer tree-sitter)
        dump-file))))

(defun vhdl-ext-test-indent-compare (file &optional tree-sitter)
  "Compare FILE absolute path for indentation.
Reference indented version: file.indent.sv in indent dir."
  (let* ((verbose nil)
         (filename-indented (vhdl-ext-test-indent-file file tree-sitter))
         (filename-ref (file-name-concat vhdl-ext-tests-indent-dir
                                              (vhdl-ext-tests-indent-ref-file-from-orig file tree-sitter))))
    (when verbose
      (message "Comparing %s" file))
    ;; Comparison
    (if (equal (with-temp-buffer
                 (insert-file-contents filename-indented)
                 (buffer-substring-no-properties (point-min) (point-max)))
               (with-temp-buffer
                 (insert-file-contents filename-ref)
                 (buffer-substring-no-properties (point-min) (point-max))))
        (progn
          (delete-file filename-indented)
          t)
      ;; Dump on error if enabled
      (when vhdl-ext-tests-indent-dump-diff-on-error
        (shell-command (concat
                        "diff " filename-ref " " filename-indented " > " (concat (file-name-sans-extension filename-indented)) ".diff")))
      nil)))



(provide 'vhdl-ext-tests-indent)

;;; vhdl-ext-tests-indent.el ends here
