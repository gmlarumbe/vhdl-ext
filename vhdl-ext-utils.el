;;; vhdl-ext-utils.el --- VHDL Utils -*- lexical-binding: t -*-

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

;; Utils

;;; Code:

(require 'project)
(require 'vhdl-mode)

(defconst vhdl-ext-blank-optional-re "[[:blank:]\n]*")
(defconst vhdl-ext-blank-mandatory-re "[[:blank:]\n]+")
(defconst vhdl-ext-identifier-re "[a-zA-Z_][a-zA-Z0-9_-]*")
(defconst vhdl-ext-arch-identifier-opt-re (concat "\\(\\s-*(\\s-*" vhdl-ext-identifier-re ")\\s-*\\)?"))
(defconst vhdl-ext-instance-re
  (concat "^\\s-*\\(?1:" vhdl-ext-identifier-re "\\)\\s-*:" vhdl-ext-blank-optional-re ; Instance name
          "\\(?2:\\(?3:component\\s-+\\|configuration\\s-+\\|\\(?4:entity\\s-+\\(?5:" vhdl-ext-identifier-re "\\)\\.\\)\\)\\)?"
          "\\(?6:" vhdl-ext-identifier-re "\\)" vhdl-ext-arch-identifier-opt-re vhdl-ext-blank-optional-re ; Entity name
          "\\(--[^\n]*" vhdl-ext-blank-mandatory-re "\\)*\\(generic\\|port\\)\\s-+map\\>"))
(defconst vhdl-ext-entity-re "^\\s-*\\(entity\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-function-re "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)")
(defconst vhdl-ext-procedure-re "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)procedure\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)")
(defconst vhdl-ext-component-re "^\\s-*\\(component\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-process-re "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)")
(defconst vhdl-ext-block-re "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(block\\)")
(defconst vhdl-ext-package-re "^\\s-*\\(package\\( body\\|\\)\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-configuration-re "^\\s-*\\(configuration\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-architecture-re "^\\s-*\\(architecture\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-context-re "^\\s-*\\(context\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)")

(defvar vhdl-ext-buffer-list nil)
(defvar vhdl-ext-dir-list nil)

(defconst vhdl-ext-lsp-available-servers
  '((ve-hdl-checker . ("hdl_checker" "--lsp"))
    (ve-rust-hdl    . "vhdl_ls")
    (ve-ghdl-ls     . "ghdl-ls")
    (ve-vhdl-tool   . ("vhdl-tool" "lsp")))
  "Vhdl-ext available LSP servers.")
(defconst vhdl-ext-lsp-server-ids
  (mapcar #'car vhdl-ext-lsp-available-servers))


(defun vhdl-ext-replace-regexp (regexp to-string start end)
  "Wrapper function for programatic use of `replace-regexp'.
Replace REGEXP with TO-STRING from START to END."
  (let* ((marker (make-marker))
         (endpos (when end (set-marker marker end))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp endpos t)
        (replace-match to-string)))))

(defun vhdl-ext-replace-regexp-whole-buffer (regexp to-string)
  "Replace REGEXP with TO-STRING on whole `current-buffer'."
  (vhdl-ext-replace-regexp regexp to-string (point-min) nil))

(defun vhdl-ext-replace-string (string to-string start end &optional fixedcase)
  "Wrapper function for programatic use of `replace-string'.
Replace STRING with TO-STRING from START to END.

If optional arg FIXEDCASE is non-nil, do not alter the case of
the replacement text (see `replace-match' for more info)."
  (let* ((marker (make-marker))
         (endpos (when end (set-marker marker end))))
    (save-excursion
      (goto-char start)
      (while (search-forward string endpos t)
        (replace-match to-string fixedcase)))))

(defun vhdl-ext-scan-buffer-entities ()
  "Find entities in current buffer.
Return list with found entities or nil if not found."
  (let (entities)
    (save-excursion
      (goto-char (point-min))
      (while (vhdl-re-search-forward vhdl-ext-entity-re nil t)
        (push (match-string-no-properties 2) entities)))
    (delete-dups entities)))

(defun vhdl-ext-read-file-entities (&optional file)
  "Find entities in current buffer.
Find entities in FILE if optional arg is non-nil.
Return list with found entities or nil if not found."
  (let ((buf (if file
                 (get-file-buffer file)
               (current-buffer)))
        (debug nil))
    (if buf
        (with-current-buffer buf
          (vhdl-ext-scan-buffer-entities))
      ;; If FILE buffer is not being visited, use a temporary buffer
      (with-temp-buffer
        (when debug
          (clone-indirect-buffer-other-window "*debug*" t))
        (insert-file-contents file)
        (vhdl-ext-scan-buffer-entities)))))

(defun vhdl-ext-select-file-entity (&optional file)
  "Select file entity from FILE.
If only one entity was found return it as a string.
If more than one entity was found, select between available ones.
Return nil if no entity was found."
  (let ((entities (vhdl-ext-read-file-entities file)))
    (if (cdr entities)
        (completing-read "Select entity: " entities)
      (car entities))))

(defun vhdl-ext-project-root ()
  "Find current project root, depending on available packages."
  (or (and (project-current)
           (project-root (project-current)))
      default-directory))

(defun vhdl-ext-update-buffer-and-dir-list ()
  "Update Vhdl-mode opened buffers and directories lists."
  (let (vhdl-buffers vhdl-dirs)
    (dolist (buf (buffer-list (current-buffer)))
      (with-current-buffer buf
        (when (or (eq major-mode 'vhdl-mode)
                  (eq major-mode 'vhdl-ts-mode))
          (push buf vhdl-buffers)
          (unless (member default-directory vhdl-dirs)
            (push default-directory vhdl-dirs)))))
    (setq vhdl-ext-buffer-list vhdl-buffers)
    (setq vhdl-ext-dir-list vhdl-dirs)))

(defun vhdl-ext-get-standard ()
  "Get current standard as a string from `vhdl-standard'."
  (let ((std (car vhdl-standard)))
    (if (equal std 8)
        (format "0%s" std)
      (format "%s" std))))

(defun vhdl-ext-kill-buffer-hook ()
  "VHDL hook to run when killing a buffer."
  (setq vhdl-ext-buffer-list (remove (current-buffer) vhdl-ext-buffer-list)))

(defun vhdl-ext-workdir ()
  "Return the working library directory according to current project.
Check `vhdl-project-alist'."
  (let ((root (nth 1 (vhdl-aget vhdl-project-alist vhdl-project)))
        (dir  (nth 7 (vhdl-aget vhdl-project-alist vhdl-project))))
    (when (and root dir)
      (file-name-concat root dir))))


(provide 'vhdl-ext-utils)

;;; vhdl-ext-utils.el ends here
