;;; vhdl-ext-capf.el --- Vhdl-ext Completion at point  -*- lexical-binding: t -*-

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

;; Completion at point utils

;;; Code:

(require 'vhdl-ext-tags)

(defconst vhdl-ext-capf-library-packages
  '(("ieee" ("fixed_float_types" "fixed_generic_pkg" "fixed_pkg"
             "float_generic_pkg" "float_pkg" "ieee_bit_context"
             "ieee_std_context" "math_complex" "math_real" "numeric_bit"
             "numeric_bit_unsigned" "numeric_std" "numeric_std_unsigned"
             "std_logic_1164" "std_logic_textio"))
    ("std" ("standard" "textio"))
    ("synopsys" ("std_logic_arith" "std_logic_misc" "std_logic_signed" "std_logic_textio" "std_logic_unsigned"))
    ("upf" ("upf"))
    ("vital" ("vital_primitives" "vital_timing" "vital_memory")))
  "Alist with libraries and their corresponding packages.")

(defun vhdl-ext-capf-get-table-entry (table &optional tag)
  "Get symbol at point entry from capf TABLE.

If TAG is nil, search for the entry that corresponds to `symbol-at-point'.
Otherwise search for TAG entry."
  (unless tag
    (setq tag (thing-at-point 'symbol :no-props)))
  (gethash tag table))

(defun vhdl-ext-capf-get-entry-items (entry)
  "Get items from tags table ENTRY."
  (plist-get entry :items))

(defun vhdl-ext-capf-get-entry-type (entry)
  "Get type from tags table ENTRY.

Only returns the type of the first occurrence in the :locs property of ENTRY."
  (let ((locs (plist-get entry :locs)))
    (plist-get (car locs) :type)))

(defun vhdl-ext-capf-get-completions (table &optional tag)
  "Get completion candidates from TABLE.

If optional arg TAG is nil, get completions for symbol at point."
  (vhdl-ext-capf-get-entry-items (vhdl-ext-capf-get-table-entry table tag)))

(defun vhdl-ext-capf--dot-completion-bounds ()
  "Return bounds of dot completion for `completion-at-point'.

Return value is a cons with (start . end) bounds."
  (let (start end)
    (cond ((eq (preceding-char) ?.)
           (setq start (point))
           (setq end (point)))
          ((save-excursion
             (skip-chars-backward vhdl-ext-identifier-sym-re)
             (setq start (point))
             (eq (preceding-char) ?.))
           (setq end (point)))
          (t
           nil))
    (when (and start end)
      (cons start end))))

(defun vhdl-ext-capf--library-completion-bounds ()
  "Return bounds of library completion for `completion-at-point'.

Return value is a cons with (start . end) bounds."
  (let (start end identifier is-lib)
    (setq end (point))
    (save-excursion
      (skip-chars-backward vhdl-ext-identifier-sym-re)
      (setq start (point))
      (skip-chars-backward "[ \t]")
      (setq identifier (thing-at-point 'symbol :no-props))
      (when (and identifier
                 (or (string= identifier "library")
                     (string= identifier "use")))
        (setq is-lib t)))
    (when is-lib
      (cons start end))))

(defun vhdl-ext-capf--library-pkg-completion-lib ()
  "Return library completion for `completion-at-point'."
  (save-excursion
    (skip-chars-backward vhdl-ext-identifier-sym-re)
    (when (eq (preceding-char) ?.)
      (backward-char)
      (thing-at-point 'symbol :no-props))))

(defun vhdl-ext-capf-annotation-function (cand)
  "Completion annotation function for candidate CAND.

Get candidate type from `vhdl-ext-tags-defs-table' or if not found, from
`vhdl-ext-tags-inst-table'.

See available types in `vhdl-ext-tags-definitions-ts-re'."
  (let* ((proj (vhdl-ext-buffer-proj))
         (defs-table (alist-get proj vhdl-ext-tags-defs-table nil nil #'string=))
         (inst-table (alist-get proj vhdl-ext-tags-inst-table nil nil #'string=))
         (entry (or (and defs-table (gethash cand defs-table))
                    (and inst-table (gethash cand inst-table))))
         (locs (plist-get entry :locs))
         (type (plist-get (car locs) :type))) ; INFO: Getting the type of the first appearance
    (pcase type
      ("entity_declaration"                "<ent>")
      ("architecture_body"                 "<arch>")
      ("package_declaration"               "<pkg>")
      ("package_body"                      "<pkg_body>")
      ("component_declaration"             "<cmp>")
      ("constant_interface_declaration"    "<generic>")
      ("signal_interface_declaration"      "<port>")
      ("signal_declaration"                "<signal>")
      ("constant_declaration"              "<constant>")
      ("full_type_declaration"             "<type>")
      ("element_declaration"               "<elm>")
      ("variable_declaration"              "<var>")
      ("procedure_declaration"             "<pcd>")
      ("function_declaration"              "<fun>")
      ("function_body"                     "<fun_body>")
      ("procedure_body"                    "<pcd_body>")
      ("configuration_declaration"         "<cfg>")
      ("context_declaration"               "<ctx>")
      ("component_instantiation_statement" "<instance>")
      (_ type))))

(defun vhdl-ext-capf ()
  "Complete with identifiers present in various hash tables.

Tables used: `vhdl-ext-tags-defs-table', `vhdl-ext-tags-inst-table' and
`vhdl-ext-tags-refs-table'.

Show annotations using function `vhdl-ext-capf-annotation-function'."
  (interactive)
  (let* ((proj (vhdl-ext-buffer-proj))
         (defs-table (alist-get proj vhdl-ext-tags-defs-table nil 'remove #'string=))
         (refs-table (alist-get proj vhdl-ext-tags-refs-table nil 'remove #'string=))
         (inst-table (alist-get proj vhdl-ext-tags-inst-table nil 'remove #'string=))
         (annotation-fn #'vhdl-ext-capf-annotation-function)
         bounds lib start end completions)
    (cond (;; library/use completion
           (setq bounds (vhdl-ext-capf--library-completion-bounds))
           (setq completions (mapcar #'car vhdl-ext-capf-library-packages)))
          ;; Dot completion for library package completion and hierarchical references
          ((setq bounds (vhdl-ext-capf--dot-completion-bounds))
           (cond (;; Library package completion
                  (setq lib (vhdl-ext-capf--library-pkg-completion-lib))
                  (setq completions (cadr (assoc lib vhdl-ext-capf-library-packages))))
                 ;; Default (hierarchical references)
                 (t
                  (let (table-entry-value block-type)
                    (save-excursion
                      (goto-char (car bounds))
                      (backward-char)
                      (while (eq (preceding-char) ?\)) ; Skip array indexes
                        (vhdl-ext-backward-sexp))
                      (setq table-entry-value (or (vhdl-ext-capf-get-table-entry defs-table)
                                                  (vhdl-ext-capf-get-table-entry inst-table))) ; Search for definitions of objects and instances
                      (when table-entry-value
                        (setq block-type (vhdl-ext-capf-get-entry-type table-entry-value))
                        (setq completions (append (vhdl-ext-capf-get-completions defs-table block-type)
                                                  (vhdl-ext-capf-get-completions inst-table block-type)))))))))
          (t ; Fallback, all project completions and lang keywords
           (setq bounds (bounds-of-thing-at-point 'symbol))
           (dolist (table `(,defs-table ,inst-table ,refs-table))
             (when table
               (maphash (lambda (key _value)
                          (push key completions))
                        table)))
           (delete-dups completions)
           (setq completions `(,@completions ,@vhdl-keywords))))
    ;; Return value for `completion-at-point'
    (setq start (car bounds))
    (setq end (cdr bounds))
    (list start end completions
          :annotation-function annotation-fn
          :company-docsig #'identity)))

(defun vhdl-ext-capf-set (&optional disable)
  "Enable or DISABLE builtin capf function."
  (if disable
      (remove-hook 'completion-at-point-functions #'vhdl-ext-capf :local)
    (add-hook 'completion-at-point-functions #'vhdl-ext-capf nil :local)))



(provide 'vhdl-ext-capf)

;;; vhdl-ext-capf.el ends here


