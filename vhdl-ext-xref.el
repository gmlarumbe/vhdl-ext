;;; vhdl-ext-xref.el --- Vhdl-ext Xref Backend  -*- lexical-binding: t -*-

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

;; Find definitions and references `xref' backend.

;;; Code:

(require 'xref)
(require 'vhdl-ext-tags)


(defgroup vhdl-ext-xref nil
  "Vhdl-ext xref customization."
  :group 'vhdl-ext)

(defface vhdl-ext-xref-match-face '((t :inherit match))
  "Vhdl-ext face used to highlight matches in xref."
  :group 'vhdl-ext-xref)


(defun vhdl-ext-xref--find-symbol (symbol type)
  "Return list of TYPE xref objects for SYMBOL."
  (let* ((proj (vhdl-ext-buffer-proj))
         (table (cond ((eq type 'def)
                       (vhdl-aget vhdl-ext-tags-defs-table proj))
                      ((eq type 'ref)
                       (vhdl-aget vhdl-ext-tags-refs-table proj))
                      (t
                       (error "Wrong table"))))
         (table-entry (if table
                          (gethash symbol table)
                        (error "Tags table empty.  Run first `vhdl-ext-tags-get' or `vhdl-ext-tags-get-async'")))
         (entry-locs (plist-get table-entry :locs))
         (entry-locs-grouped (seq-group-by (lambda (loc)
                                             (equal buffer-file-name (plist-get loc :file)))
                                           entry-locs))
         ;; Locs in current file should show up first
         (entry-locs-sorted (append (alist-get nil entry-locs-grouped)
                                    (alist-get t entry-locs-grouped)))
         file line column desc xref-entries)
    (when entry-locs-sorted
      (dolist (loc entry-locs-sorted)
        (setq file (plist-get loc :file))
        (setq line (plist-get loc :line))
        (setq column (plist-get loc :col))
        (setq desc (replace-regexp-in-string (concat "\\_<" symbol "\\_>")
                                             (propertize symbol 'face 'vhdl-ext-xref-match-face)
                                             (plist-get loc :desc)
                                             :fixedcase))
        (push (xref-make desc (xref-make-file-location file line column)) xref-entries)))
    xref-entries))

(defun vhdl-ext-xref-backend ()
  "Vhdl-ext backend for Xref."
  (let (proj symbol proj-table)
    (and (setq proj (vhdl-ext-buffer-proj))
         (setq symbol (thing-at-point 'symbol :no-props))
         (or (and (setq proj-table (vhdl-aget vhdl-ext-tags-defs-table proj))
                  (gethash symbol proj-table))
             (and (setq proj-table (vhdl-aget vhdl-ext-tags-refs-table proj))
                  (gethash symbol proj-table)))
         'vhdl-ext)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql vhdl-ext)))
  "Implementation of `xref-backend-identifier-at-point' for vhdl-ext-xref."
  (thing-at-point 'symbol :no-props))

(cl-defmethod xref-backend-definitions ((_backend (eql vhdl-ext)) symbol)
  "Implementation of `xref-backend-definitions' for vhdl-ext-xref.
Find definitions of SYMBOL."
  (vhdl-ext-xref--find-symbol symbol 'def))

(cl-defmethod xref-backend-references ((_backend (eql vhdl-ext)) symbol)
  "Implementation of `xref-backend-references' for vhdl-ext-xref.
Find references of SYMBOL."
  (vhdl-ext-xref--find-symbol symbol 'ref))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql vhdl-ext)))
  "Implementation of `xref-backend-identifier-completion-table'."
  nil)

(defun vhdl-ext-xref-backend-enable ()
  "Enable `vhdl-ext' backend on current buffer."
  (setq-local xref-backend-functions `(vhdl-ext-xref-backend ,@xref-backend-functions)))

(defun vhdl-ext-xref-set (&optional disable)
  "Setup `vhdl-ext' to use builtin `xref' backend.

If optional arg DISABLE is provided, remove the hook that enabled the backend.

Removes the rest of xref backends by being a hook for `vhdl-ext-mode' instead of
to `vhdl-mode', since the first one is loaded later and overwrites the hook
value.  Otherwise, hooks are not ran in a specific order, and rely on the
priority argument."
  (if disable
      (remove-hook 'vhdl-ext-mode-hook #'vhdl-ext-xref-backend-enable)
    (add-hook 'vhdl-ext-mode-hook #'vhdl-ext-xref-backend-enable)))



(provide 'vhdl-ext-xref)

;;; vhdl-ext-xref.el ends here
