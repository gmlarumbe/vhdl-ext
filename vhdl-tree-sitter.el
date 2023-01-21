;;; vhdl-tree-sitter.el --- VHDL Extensions for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/vhdl-ext
;; Version: 0.0.0
;; Keywords: VHDL, IDE, Tools
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

;; VHDL Tree Sitter
;;
;; For more queries, check:
;;  - https://github.com/alemuller/tree-sitter-vhdl/blob/main/queries/highlights.scm

;;; Code:


(require 'vhdl-mode)
(require 'treesit)


(defconst vhdl-ts-keywords (append vhdl-02-keywords vhdl-08-keywords))

(defconst vhdl-ts-types (append vhdl-02-types vhdl-08-types vhdl-math-types))
(defconst vhdl-ts-types-regexp (concat "\\<\\(" (regexp-opt vhdl-ts-types) "\\)\\>"))

(defconst vhdl-ts-attributes (append vhdl-02-attributes vhdl-08-attributes))

(defconst vhdl-ts-enum-values vhdl-02-enum-values)

(defconst vhdl-ts-constants vhdl-math-constants)

(defconst vhdl-ts-functions (append vhdl-02-functions vhdl-08-functions vhdl-math-functions))
(defconst vhdl-ts-functions-regexp (concat "\\<\\(" (regexp-opt vhdl-ts-functions) "\\)\\>"))

(defconst vhdl-ts-packages (append vhdl-02-packages vhdl-08-packages vhdl-math-packages))

(defconst vhdl-ts-directives vhdl-08-directives)


(defconst vhdl-ts-operators-relational
  '("="
    "/="
    "<"
    ">"
    "<=" ; Less or equal/signal assignment
    "=>" ; Greater or equal/port connection
    ":=" ; Not an operator, but falls better here
    ))

(defconst vhdl-ts-operators-arithmetic
  '("+"
    "-"
    "*"
    "/"
    "**"
    "&"))

(defconst vhdl-ts-punctuation
  '(";"
    ":"
    ","
    "'"
    "("
    ")"
    "["
    "]"
    "|"
    "."
    "!"
    "?"))


(defvar vhdl--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'vhdl
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'vhdl
   '(((string_literal) @font-lock-string-face)
     ((bit_string_literal) @font-lock-string-face)
     ((character_literal) @font-lock-string-face))

   ;; Place before 'keywords to override things like "downto" in ranges
   :feature 'all
   :language 'vhdl
   '(;; Library
     (library_clause
      (logical_name_list (simple_name) @font-lock-builtin-face))
     (use_clause
      (selected_name
       (selected_name (simple_name) @font-lock-function-name-face)))
     ;; Package
     (package_declaration
      (identifier) @font-lock-function-name-face)
     (package_body
      (simple_name) @font-lock-function-name-face)
     ;; Entity
     (entity_declaration
      name: (identifier) @font-lock-function-name-face)
     (entity_declaration
      at_end: (simple_name) @font-lock-function-name-face)
     ;; Architecture
     (architecture_body
      (identifier) @font-lock-function-name-face
      (simple_name) @font-lock-function-name-face)
     ;; Component
     (component_declaration
      name: (identifier) @font-lock-function-name-face)
     ;; Generate
     (if_generate_statement
      (label (identifier) @font-lock-constant-face))
     (for_generate_statement
      (label (identifier) @font-lock-constant-face))
     ;; Process
     (process_statement
      (label (identifier) @font-lock-constant-face))
     (process_statement
      (sensitivity_list (simple_name) @font-lock-constant-face))
     ;; Instances
     (component_instantiation_statement
      (label
       (identifier) @vhdl-ext-font-lock-instance-face)
      (entity_instantiation
       (selected_name
        prefix: (simple_name) @vhdl-ext-font-lock-instance-lib-face
        suffix: (simple_name) @vhdl-ext-font-lock-entity-face)))
     (component_instantiation_statement
      (label (identifier) @vhdl-ext-font-lock-instance-face)
      (component_instantiation (simple_name) @vhdl-ext-font-lock-entity-face))
     ;; Port connections
     (association_list
      (named_association_element
       formal_part: (simple_name) @vhdl-ext-font-lock-port-connection-face))
     (association_list
      (named_association_element
       formal_part: (selected_name
                     prefix: (simple_name) @vhdl-ext-font-lock-instance-lib-face
                     suffix: (simple_name) @vhdl-ext-font-lock-port-connection-face)))
     ;; Ranges
     (descending_range
      high: (simple_expression) @vhdl-ext-font-lock-braces-content-face)
     (descending_range
      low: (simple_expression) @vhdl-ext-font-lock-braces-content-face)
     (expression_list
      (expression (integer_decimal) @vhdl-ext-font-lock-braces-content-face))
     (["downto" "to"] @vhdl-ext-font-lock-instance-lib-face)
     ;; Constants
     (constant_declaration
      (identifier_list (identifier) @font-lock-constant-face))
     ;; Enum labels
     (enumeration_type_definition
      literal: (identifier) @font-lock-constant-face)
     ;; Record members
     (selected_name
      prefix: (simple_name) @vhdl-ext-font-lock-instance-lib-face)
     ;; clk'event
     (attribute_name
      prefix: (simple_name) @font-lock-builtin-face
      (predefined_designator) @font-lock-builtin-face)
     )

   :feature 'keyword
   :language 'vhdl
   `([,@vhdl-ts-keywords] @font-lock-keyword-face)

   :feature 'operator
   :language 'vhdl
   `(([,@vhdl-ts-operators-relational] @vhdl-ext-font-lock-punctuation-face)
     ([,@vhdl-ts-operators-arithmetic] @vhdl-ext-font-lock-punctuation-bold-face))

   :feature 'punctuation
   :language 'vhdl
   `([,@vhdl-ts-punctuation] @vhdl-ext-font-lock-punctuation-face)

   :feature 'types
   :language 'vhdl
   `((full_type_declaration
      name: (identifier) @font-lock-type-face)
     ((ambiguous_name
       prefix: (simple_name) @font-lock-type-face)
      (:match ,vhdl-ts-types-regexp @font-lock-type-face))
     (subtype_indication
      (type_mark
       (simple_name) @font-lock-type-face)))

   :feature 'function
   :language 'vhdl
   '(;; Procedure
     (procedure_declaration
      (identifier) @font-lock-function-name-face)
     (procedure_body
      (identifier) @font-lock-function-name-face)
     ;; Function
     (function_body (identifier) @font-lock-function-name-face))

   :feature 'builtin
   :language 'vhdl
   `(((ambiguous_name
       prefix: (simple_name) @font-lock-builtin-face)
      (:match ,vhdl-ts-functions-regexp @font-lock-builtin-face)))
   ))



(define-derived-mode vhdl-ts-mode vhdl-mode "VHDLts"
  "Major mode for editing VHDL files, using tree-sitter library."
  :syntax-table vhdl-mode-syntax-table
  ;; Treesit
  (when (treesit-ready-p 'vhdl)
    (treesit-parser-create 'vhdl)
    ;; Font-lock.
    (setq font-lock-defaults nil) ; Disable `vhdl-mode' font-lock/indent config
    (setq-local treesit-font-lock-feature-list
                '((comment string all keyword operator punctuation function builtin types)))
    (setq-local treesit-font-lock-settings vhdl--treesit-settings)
    ;; Setup
    (treesit-major-mode-setup)))


(provide 'vhdl-tree-sitter)


;;; vhdl-tree-sitter.el ends here

