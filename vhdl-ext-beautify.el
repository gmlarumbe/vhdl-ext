;;; vhdl-ext-beautify.el --- Vhdl-ext Beautify  -*- lexical-binding: t -*-

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

;; - Beautify block/instance at point
;; - Batch processing for:
;;    - List of files
;;    - Files of current directory

;;; Code:

(require 'vhdl-ext-nav)

(defun vhdl-ext-beautify-instance-at-point ()
  "Indent current instance at point."
  (interactive)
  (let* ((data (vhdl-ext-instance-at-point))
         (block (car data))
         (name (cadr data))
         start-pos end-pos)
    (unless data
      (user-error "Not inside an instance"))
    (save-excursion
      (goto-char (match-beginning 0))
      (setq start-pos (line-beginning-position))
      (goto-char (match-end 0))
      (setq end-pos (line-end-position))
      (vhdl-beautify-region start-pos end-pos)
      (message "Beautified %s : %s" block name))))

(defun vhdl-ext-beautify-block-at-point ()
  "Beautify/indent block at point.

If block is an instance, also align parameters and ports."
  (interactive)
  ;; Precedence is relevant in the subsequent conditional clause
  (cond (;; Tree-sitter implementation
         (eq major-mode 'vhdl-ts-mode)
         (vhdl-ts-beautify-block-at-point))
        (;; Instance
         (vhdl-ext-instance-at-point)
         (vhdl-ext-beautify-instance-at-point))
        (t ;; Block
         (let* ((data (vhdl-ext-block-at-point))
                (block (alist-get 'type data))
                (name (alist-get 'name data))
                start-pos end-pos)
           (unless data
             (user-error "Not inside a block"))
           (save-excursion
             (goto-char (alist-get 'beg-point data))
             (setq start-pos (line-beginning-position))
             (goto-char (alist-get 'end-point data))
             (setq end-pos (line-end-position))
             (vhdl-beautify-region start-pos end-pos)
             (message "Beautified %s : %s" block name))))))

(defun vhdl-ext-beautify-files (files ts-mode)
  "Beautify VHDL FILES.

FILES is a list of strings containing the filepaths.

If TS-MODE is non-nil use tree-sitter implementation if `vhdl-ts-mode' is
available."
  (dolist (file files)
    (unless (file-exists-p file)
      (error "File %s does not exist! Aborting!" file)))
  (save-window-excursion
    (dolist (file files)
      (message "Processing %s..." file)
      (with-temp-file file
        (insert-file-contents file)
        (if ts-mode
            (progn
              (vhdl-ts-mode)
              (vhdl-ts-beautify-buffer))
          (vhdl-ext-with-no-hooks
            (vhdl-mode))
          (vhdl-beautify-buffer))))))

(defun vhdl-ext-beautify-dir-files (dir &optional ts-mode)
  "Beautify VHDL files on DIR.

Include subdirectory files recursively.

With prefix arg, or if TS-MODE is non-nil, use `vhdl-ts-mode' beautifying
implementation."
  (interactive "DDirectory: \nP")
  (let ((files (vhdl-ext-dir-files dir :recursive)))
    (vhdl-ext-beautify-files files ts-mode)))


(provide 'vhdl-ext-beautify)

;;; vhdl-ext-beautify.el ends here
