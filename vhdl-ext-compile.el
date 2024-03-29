;;; vhdl-ext-compile.el --- Vhdl-ext Compilation Utils -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Gonzalo Larumbe

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

;; Compilation related VHDL functions.
;;
;; This file provides functions to perform compilations with syntax highlighting
;; and jump to error based on `compilation-mode'.
;;
;; - Interactive functions examples:
;;   - `vhdl-ext-compile-project-ghdl': compile with GHDL current project of `vhdl-ext-project-alist'
;;
;; - Non-interactive function usage examples:
;;   - (vhdl-ext-compile-ghdl (concat "ghdl -s " buffer-file-name))
;;
;;; Code:


(require 'vhdl-ext-utils)


;;; Faces
(defgroup vhdl-ext-compile nil
  "Vhdl-ext compilation."
  :group 'vhdl-ext)

(defconst vhdl-ext-compile-msg-code-face 'vhdl-ext-compile-msg-code-face)
(defface vhdl-ext-compile-msg-code-face
  '((t (:inherit font-lock-comment-face)))
  "Face for compilation message codes."
  :group 'vhdl-ext-compile)

(defconst vhdl-ext-compile-bin-face 'vhdl-ext-compile-bin-face)
(defface vhdl-ext-compile-bin-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for compilation binaries."
  :group 'vhdl-ext-compile)

;;; Macros
(defmacro vhdl-ext-compile-define-mode (name &rest args)
  "Macro to define a compilation derived mode for a Vhdl error regexp.

NAME is the name of the created compilation mode.

ARGS is a property list with :desc, :docstring, :compile-re and :buf-name."
  (declare (indent 1) (debug 1))
  (let ((desc (plist-get args :desc))
        (docstring (plist-get args :docstring))
        (compile-re (plist-get args :compile-re))
        (buf-name (plist-get args :buf-name)))
    `(define-compilation-mode ,name ,desc ,docstring
       (setq-local compilation-error-regexp-alist (mapcar #'car ,compile-re))
       (setq-local compilation-error-regexp-alist-alist ,compile-re)
       (when ,buf-name
         (rename-buffer ,buf-name))
       (setq truncate-lines t)
       (goto-char (point-max)))))

(defmacro vhdl-ext-compile-define-fn (name &rest args)
  "Macro to define a function to compile with error regexp highlighting.

Function will be callable by NAME.

ARGS is a property list."
  (declare (indent 1) (debug 1))
  (let ((docstring (plist-get args :docstring))
        (buf (plist-get args :buf))
        (comp-mode (plist-get args :comp-mode)))
    `(defun ,name (command)
       ,docstring
       (when (and ,buf (get-buffer ,buf))
         (if (y-or-n-p (format "Buffer %s is in use, kill its process and start new compilation?" ,buf))
             (kill-buffer ,buf)
           (user-error "Aborted")))
       (compile command)
       (,comp-mode))))

;;; Compilation-re
(defconst vhdl-ext-compile-filename-re "[a-zA-Z0-9-_\\.\\/]+")

(defconst vhdl-ext-compile-ghdl-re
  `((ghdl-info    ,(concat "\\(?1:" vhdl-ext-compile-filename-re "\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\):note: ")    1 2 3 0 nil)
    (ghdl-warning ,(concat "\\(?1:" vhdl-ext-compile-filename-re "\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\):warning: ") 1 2 3 1 nil)
    (ghdl-error   ,(concat "\\(?1:" vhdl-ext-compile-filename-re "\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): ")         1 2 3 2 nil)))

(defvar vhdl-ext-compile-ghdl-buf "*ghdl*")

;;; Compilation-modes and functions
(vhdl-ext-compile-define-mode vhdl-ext-compile-ghdl-mode
  :desc "GHDL"
  :docstring "GHDL Compilation mode."
  :compile-re vhdl-ext-compile-ghdl-re
  :buf-name vhdl-ext-compile-ghdl-buf)

(vhdl-ext-compile-define-fn vhdl-ext-compile-ghdl
  :docstring "Compile GHDL COMMAND with error regexp highlighting."
  :buf vhdl-ext-compile-ghdl-buf
  :comp-mode vhdl-ext-compile-ghdl-mode)

(defun vhdl-ext-compile-project-ghdl ()
  "Compile current project from `vhdl-ext-project-alist' using GHDL.

Files in `vhdl-ext-project-alist' need to be in the correct order.

According to GHDL documentation:
 - GHDL analyzes each filename in the given order, and stops the analysis in
case of error (the following files are not analyzed)."
  (interactive)
  (let* ((proj (vhdl-ext-buffer-proj))
         (root (vhdl-ext-buffer-proj-root proj))
         (workdir (vhdl-ext-proj-workdir proj))
         ;; Get files and analyze up to current buffer file
         (files (vhdl-ext-proj-files proj))
         (files-filtered (seq-take-while (lambda (elm) (not (string= elm buffer-file-name))) files)))
    ;; Create workdir if it does not exist
    (unless (and (file-exists-p workdir)
                 (file-directory-p workdir))
      (make-directory workdir :parents))
    ;; Compile current buffer
    (vhdl-ext-compile-ghdl (mapconcat #'identity
                                      `("cd" ,root "&&"
                                        "ghdl" "-a"
                                        ,(vhdl-ext-ghdl-proj-args)
                                        ,(mapconcat #'identity files-filtered " ")
                                        ,buffer-file-name)
                                      " "))))


(provide 'vhdl-ext-compile)

;;; vhdl-ext-compile.el ends here
