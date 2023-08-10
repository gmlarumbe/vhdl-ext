;;; vhdl-ext-font-lock.el --- VHDL Font Lock -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/vhdl-ext
;; Version: 0.2.0
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

;; Improved syntax highlighting based on `font-lock' keywords overriding.
;;
;; Multiline Font Locking has reliability limitations in Emacs.
;;  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiline-Font-Lock.html
;;  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Multiline.html
;;
;; One way to ensure reliable rehighlighting of multiline font-lock constructs
;; is by using the `font-lock-multiline' text property.
;; - The `font-lock-multiline' variable might seem to be working but is not reliable.
;; - Using the `font-lock-multiline' property might apply to a few lines (such is the case).
;;   For longer sections it is necessary to create font lock custom functions and gets
;;   more complicated.
;;
;; Search based fontification:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html

;;; Code:

(require 'vhdl-mode)
(require 'vhdl-ext-nav)

;;;; Faces
(defgroup vhdl-ext-font-lock nil
  "Vhdl-ext faces."
  :group 'vhdl-ext)

(defvar vhdl-ext-font-lock-then-face 'vhdl-ext-font-lock-then-face)
(defface vhdl-ext-font-lock-then-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for if-else grouping keyword: then."
  :group 'verilog-ext-font-lock)

(defvar vhdl-ext-font-lock-punctuation-face 'vhdl-ext-font-lock-punctuation-face)
(defface vhdl-ext-font-lock-punctuation-face
  '((t (:inherit font-lock-punctuation-face)))
  "Face for punctuation symbols:
!,;:?'=<>*"
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-operator-face 'vhdl-ext-font-lock-operator-face)
(defface vhdl-ext-font-lock-operator-face
  '((t (:inherit font-lock-operator-face)))
  "Face for operator symbols, such as &^~+-/|."
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-brackets-face 'vhdl-ext-font-lock-brackets-face)
(defface vhdl-ext-font-lock-brackets-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for brackets []."
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-parenthesis-face 'vhdl-ext-font-lock-parenthesis-face)
(defface vhdl-ext-font-lock-parenthesis-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for parenthesis ()."
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-curly-braces-face 'vhdl-ext-font-lock-curly-braces-face)
(defface vhdl-ext-font-lock-curly-braces-face
  '((t (:inherit font-lock-bracket-face)))
  "Face for curly braces {}."
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-brackets-content-face 'vhdl-ext-font-lock-brackets-content-face)
(defface vhdl-ext-font-lock-brackets-content-face
  '((t (:inherit font-lock-number-face)))
  "Face for content between brackets: arrays, bit vector width and indexing."
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-port-connection-face 'vhdl-ext-font-lock-port-connection-face)
(defface vhdl-ext-font-lock-port-connection-face
  '((t (:inherit font-lock-constant-face)))
  "Face for port connections of instances.
portA => signalA,
portB => signalB
);"
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-entity-face 'vhdl-ext-font-lock-entity-face)
(defface vhdl-ext-font-lock-entity-face
  '((t (:inherit font-lock-function-call-face)))
  "Face for entity names."
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-instance-face 'vhdl-ext-font-lock-instance-face)
(defface vhdl-ext-font-lock-instance-face
  '((t (:inherit font-lock-variable-use-face)))
  "Face for instance names."
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-instance-lib-face 'vhdl-ext-font-lock-instance-lib-face)
(defface vhdl-ext-font-lock-instance-lib-face
  '((t (:inherit font-lock-property-name-face)))
  "Face for instances lib prefix."
  :group 'vhdl-ext-font-lock)

(defvar vhdl-ext-font-lock-translate-off-face 'vhdl-ext-font-lock-translate-off-face)
(defface vhdl-ext-font-lock-translate-off-face
  '((t (:slant italic)))
  "Face for pragmas between comments, e.g:
* translate_off / * translate_on"
  :group 'vhdl-ext-font-lock)


;;;; Regexps
(defconst vhdl-ext-font-lock-punctuation-re "\\([!,;:?'=<>]\\|\\*\\)")
(defconst vhdl-ext-font-lock-operator-re "\\([&^~+-]\\||\\|\\.\\|\\/\\)")
(defconst vhdl-ext-font-lock-parenthesis-re "[()]")
(defconst vhdl-ext-font-lock-curly-braces-re "[{}]")
(defconst vhdl-ext-font-lock-brackets-re "\\(\\[\\|\\]\\)")
(defconst vhdl-ext-font-lock-common-constructs-re
  (concat "^\\s-*\\(\\w+\\)\\s-*:[ \t\n\r\f]*\\(\\("
          "assert\\|block\\|case\\|exit\\|for\\|if\\|loop\\|next\\|null\\|"
          "postponed\\|process\\|"
          "with\\|while"
          "\\)\\>\\|\\w+\\s-*\\(([^\n]*)\\|\\.\\w+\\)*\\s-*<=\\)"))
(defconst vhdl-ext-font-lock-labels-in-block-and-components-re
  (concat "^\\s-*for\\s-+\\(\\w+\\(,\\s-*\\w+\\)*\\)\\>\\s-*"
          "\\(:[ \t\n\r\f]*\\(\\w+\\)\\|[^i \t]\\)"))
(defconst vhdl-ext-font-lock-brackets-content-range-re "\\(?1:(\\)\\(?2:[ )+*/$0-9a-zA-Z:_-]*\\)\\s-+\\(?3:\\(down\\)?to\\)\\s-+\\(?4:[ (+*/$0-9a-zA-Z:_-]*\\)\\(?5:)\\)")
(defconst vhdl-ext-font-lock-brackets-content-index-re "\\(?1:(\\)\\s-*\\(?2:[0-9]+\\)\\s-*\\(?3:)\\)")
(defconst vhdl-ext-font-lock-directive-keywords-re
  (eval-when-compile
    (regexp-opt '("psl" "pragma" "synopsys" "synthesis") 'symbols)))
(defconst vhdl-ext-font-lock-highlight-variable-declaration-names nil)
(defconst vhdl-ext-then-regexp "\\_<then\\_>")


;;;; Functions
(defun vhdl-ext-font-lock-entity-instance-fontify (limit)
  "Search based fontification function of VHDL entities/instances.
Bound search by LIMIT."
  (let (start-line-pos end-line-pos)
    (when (vhdl-ext-find-entity-instance-fwd limit)
      (setq start-line-pos (save-excursion
                             (goto-char (match-beginning 0))
                             (line-beginning-position)))
      (setq end-line-pos (save-excursion
                           (goto-char (match-end 0))
                           (line-end-position)))
      (unless (get-text-property (point) 'font-lock-multiline)
        (put-text-property start-line-pos end-line-pos 'font-lock-multiline t))
      (point))))

(defun vhdl-ext-font-lock-within-translate-off ()
  "Similar as analogous `vhdl-within-translate-off' function.
Take `vhdl-ext-font-lock-directive-keywords-re' words into account instead of
only pragma."
  (and (save-excursion
         (re-search-backward
          (concat
           "^\\s-*--\\s-*" vhdl-ext-font-lock-directive-keywords-re "\\s-*translate_\\(on\\|off\\)\\s-*\n") nil t))
       (equal "off" (match-string 1))
       (point)))

(defun vhdl-ext-font-lock-start-translate-off (limit)
  "Similar as analogous `vhdl-start-translate-off' function.
Take `vhdl-ext-font-lock-directive-keywords-re' words into account instead of
only pragma.
Regex search bound to LIMIT."
  (when (re-search-forward
         (concat
          "^\\s-*--\\s-*" vhdl-ext-font-lock-directive-keywords-re "\\s-*translate_off\\s-*\n") limit t)
    (match-beginning 0)))

(defun vhdl-ext-font-lock-end-translate-off (limit)
  "Similar as analogous `vhdl-end-translate-off' function.
Take `vhdl-ext-font-lock-directive-keywords-re' words into account instead of
only pragma.
Regex search bound to LIMIT."
  (re-search-forward
   (concat "^\\s-*--\\s-*" vhdl-ext-font-lock-directive-keywords-re "\\s-*translate_on\\s-*\n") limit t))

(defun vhdl-ext-font-lock-match-translate-off (limit)
  "Similar as analogous `vhdl-match-translate-off' function.
Take `vhdl-ext-font-lock-directive-keywords-re' words into account instead of
only pragma.
Regex search bound to LIMIT."
  (when (< (point) limit)
    (let ((start (or (vhdl-ext-font-lock-within-translate-off)
                     (vhdl-ext-font-lock-start-translate-off limit)))
          (case-fold-search t))
      (when start
        (let ((end (or (vhdl-ext-font-lock-end-translate-off limit) limit)))
          (put-text-property start end 'font-lock-multiline t)
          (set-match-data (list start end))
          (goto-char end))))))

(defun vhdl-ext-font-lock-match-common-constructs-fontify (limit)
  "Search based fontification function for VHDL common constructs.
Adds the `font-lock-multiline' text property.
Regex search bound to LIMIT."
  (while (re-search-forward vhdl-ext-font-lock-common-constructs-re limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-multiline t)
    (point)))

(defun vhdl-ext-font-lock-match-labels-block-comp-fontify (limit)
  "Search based fontification function for VHDL labels in blocks and components.
Adds the `font-lock-multiline' text property.
Regex search bound to LIMIT."
  (while (re-search-forward vhdl-ext-font-lock-labels-in-block-and-components-re limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-multiline t)
    (point)))


;;;; Font-lock keywords
(defconst vhdl-ext-font-lock-keywords-0
  (list
   (list (concat "\\(^\\|[ \t(.']\\)\\(<" vhdl-template-prompt-syntax ">\\)")
         2 'vhdl-font-lock-prompt-face t)
   (list (concat "--\\s-*" vhdl-ext-font-lock-directive-keywords-re "\\s-+\\(.*\\)$")
         '(0 'vhdl-ext-font-lock-translate-off-face prepend)
         '(1 'vhdl-ext-font-lock-preprocessor-face prepend))
   ;; highlight c-preprocessor directives
   (list "^#[ \t]*\\(\\w+\\)\\([ \t]+\\(\\w+\\)\\)?"
         '(1 font-lock-builtin-face)
         '(3 font-lock-variable-name-face nil t))))

(defconst vhdl-ext-font-lock-keywords-1
  (list
   ;; highlight keywords and standardized types, attributes, enumeration, values, and subprograms
   (list (concat "'" vhdl-attributes-regexp)
         1 'vhdl-font-lock-attribute-face)
   (list vhdl-types-regexp       1 'font-lock-type-face)
   (list vhdl-functions-regexp   1 'font-lock-builtin-face)
   (list vhdl-packages-regexp    1 'font-lock-builtin-face)
   (list vhdl-enum-values-regexp 1 'vhdl-font-lock-enumvalue-face)
   (list vhdl-constants-regexp   1 'font-lock-constant-face)
   (list vhdl-ext-then-regexp    0 'vhdl-ext-font-lock-then-face) ; Place before `vhdl-keywords-regexp' to override its face
   (list vhdl-keywords-regexp    1 'font-lock-keyword-face)))

(defconst vhdl-ext-font-lock-keywords-2
  (append
   (list
    ;; highlight names of units, subprograms, and components when declared
    (list
     (concat
      "^\\s-*\\("
      "architecture\\|configuration\\|context\\|entity\\|package"
      "\\(\\s-+body\\)?\\|"
      "\\(\\(impure\\|pure\\)\\s-+\\)?function\\|procedure\\|component"
      "\\)\\s-+\\(\\w+\\)")
     5 'font-lock-function-name-face)

    ;; highlight entity names of architectures and configurations
    (list
     "^\\s-*\\(architecture\\|configuration\\)\\s-+\\w+\\s-+of\\s-+\\(\\w+\\)"
     2 'font-lock-function-name-face)

    ;; highlight labels of common constructs (function-based search to add `font-lock-multiline' property)
    '(vhdl-ext-font-lock-match-common-constructs-fontify
      (1 'font-lock-function-name-face))

    ;; highlight label and component name of every instantiation (configuration, component and entity)
    '(vhdl-ext-font-lock-entity-instance-fontify
      (1 'vhdl-ext-font-lock-instance-face)
      ;; 3rd argument nil avoids font-locking in case there is no match (instantiation declaring component)
      (5 'vhdl-ext-font-lock-instance-lib-face nil t)
      (6 'vhdl-ext-font-lock-entity-face))

    ;; highlight names and labels at end of constructs
    (list
     (concat
      "^\\s-*end\\s-+\\(\\("
      "architecture\\|block\\|case\\|component\\|configuration\\|context\\|"
      "entity\\|for\\|function\\|generate\\|if\\|loop\\|package"
      "\\(\\s-+body\\)?\\|procedure\\|\\(postponed\\s-+\\)?process\\|"
      "units"
      "\\)\\s-+\\)?\\(\\w*\\)")
     5 'font-lock-function-name-face)

    ;; highlight labels in exit and next statements
    (list
     (concat
      "^\\s-*\\(\\w+\\s-*:\\s-*\\)?\\(exit\\|next\\)\\s-+\\(\\w*\\)")
     3 'font-lock-function-name-face)

    ;; highlight entity name in attribute specifications
    (list
     (concat
      "^\\s-*attribute\\s-+\\w+\\s-+of\\s-+\\(\\w+\\(,\\s-*\\w+\\)*\\)\\s-*:")
     1 'font-lock-function-name-face)

    ;; highlight labels in block and component specifications
    '(vhdl-ext-font-lock-match-labels-block-comp-fontify
      (1 font-lock-function-name-face) (4 font-lock-function-name-face nil t))

    ;; highlight names in library clauses
    (list "^\\s-*library\\>"
          '(vhdl-font-lock-match-item nil nil (1 font-lock-function-name-face)))

    ;; highlight names in use clauses
    (list
     (concat
      "\\<\\(context\\|use\\)\\s-+\\(\\(entity\\|configuration\\)\\s-+\\)?"
      "\\(\\w+\\)\\(\\.\\(\\w+\\)\\)?\\((\\(\\w+\\))\\)?")
     '(4 font-lock-function-name-face) '(6 font-lock-function-name-face nil t)
     '(8 font-lock-function-name-face nil t))

    ;; highlight attribute name in attribute declarations/specifications
    (list
     (concat
      "^\\s-*attribute\\s-+\\(\\w+\\)")
     1 'vhdl-font-lock-attribute-face)

    ;; highlight type/nature name in (sub)type/(sub)nature declarations
    (list
     (concat
      "^\\s-*\\(\\(sub\\)?\\(nature\\|type\\)\\|end\\s-+\\(record\\|protected\\)\\)\\s-+\\(\\w+\\)")
     5 'font-lock-type-face)

    ;; highlight formal parameters in component instantiations and subprogram
    ;; calls
    (list "\\(=>\\)"
          '(vhdl-font-lock-match-item
            (progn (goto-char (match-beginning 1))
                   (skip-syntax-backward " ")
                   (while (= (preceding-char) ?\)) (backward-sexp))
                   (skip-syntax-backward "w_")
                   (skip-syntax-backward " ")
                   (when (memq (preceding-char) '(?n ?N ?|))
                     (goto-char (point-max))))
            (goto-char (match-end 1)) (1 vhdl-ext-font-lock-port-connection-face)))

    ;; highlight alias/group/quantity declaration names and for-loop/-generate
    ;; variables
    (list "\\<\\(alias\\|for\\|group\\|quantity\\)\\s-+\\w+\\s-+\\(across\\|in\\|is\\)\\>"
          '(vhdl-font-lock-match-item
            (progn (goto-char (match-end 1)) (match-beginning 2))
            nil (1 font-lock-variable-name-face)))

    ;; highlight tool directives
    (list
     (concat
      "^\\s-*\\(`\\w+\\)")
     1 'font-lock-preprocessor-face))

   ;; highlight signal/variable/constant declaration names
   (when vhdl-ext-font-lock-highlight-variable-declaration-names
     (list "\\(:[^=]\\)"
           '(vhdl-font-lock-match-item
             (progn (goto-char (match-beginning 1))
                    (skip-syntax-backward " ")
                    (skip-syntax-backward "w_")
                    (skip-syntax-backward " ")
                    (while (= (preceding-char) ?,)
                      (backward-char 1)
                      (skip-syntax-backward " ")
                      (skip-syntax-backward "w_")
                      (skip-syntax-backward " ")))
             (goto-char (match-end 1)) (1 vhdl-ext-font-lock-variable-name-face))))))


;; highlight words with special syntax.
(defconst vhdl-ext-font-lock-keywords-3
  (let ((syntax-alist vhdl-special-syntax-alist) ; "generic/constant" "type" "variable"
        keywords)
    (while syntax-alist
      (setq keywords
            (cons
             (list (concat "\\(" (nth 1 (car syntax-alist)) "\\)") 1
                   (vhdl-function-name
                    "vhdl-font-lock" (nth 0 (car syntax-alist)) "face")
                   (nth 4 (car syntax-alist)))
             keywords))
      (setq syntax-alist (cdr syntax-alist)))
    keywords))

;; highlight additional reserved words
(defconst vhdl-ext-font-lock-keywords-4
  (list (list vhdl-reserved-words-regexp 1
              'vhdl-font-lock-reserved-words-face)))

;; highlight translate_off regions
(defconst vhdl-ext-font-lock-keywords-5
  '((vhdl-ext-font-lock-match-translate-off
     (0 vhdl-ext-font-lock-translate-off-face prepend))))

;; Punctuation and other symbols
(defconst vhdl-ext-font-lock-keywords-6
  (list
   ;; Bit range
   (list vhdl-ext-font-lock-brackets-content-range-re
         '(2 vhdl-ext-font-lock-brackets-content-face)
         '(4 vhdl-ext-font-lock-brackets-content-face)
         '(3 vhdl-ext-font-lock-instance-lib-face))
   ;; Bit index
   (list vhdl-ext-font-lock-brackets-content-index-re
         '(2 vhdl-ext-font-lock-brackets-content-face))
   ;; Punctuation
   (list vhdl-ext-font-lock-punctuation-re 0 vhdl-ext-font-lock-punctuation-face)
   (list vhdl-ext-font-lock-operator-re 0 vhdl-ext-font-lock-operator-face)
   ;; Braces and brackets
   (list vhdl-ext-font-lock-brackets-re 0 vhdl-ext-font-lock-brackets-face)
   (list vhdl-ext-font-lock-parenthesis-re 0 vhdl-ext-font-lock-parenthesis-face)
   (list vhdl-ext-font-lock-curly-braces-re 0 vhdl-ext-font-lock-curly-braces-face)))

;; highlight everything together
(defconst vhdl-ext-font-lock-keywords
  (append
   vhdl-ext-font-lock-keywords-6
   vhdl-ext-font-lock-keywords-0
   vhdl-ext-font-lock-keywords-1
   vhdl-ext-font-lock-keywords-4 ; Empty by default
   vhdl-ext-font-lock-keywords-3
   vhdl-ext-font-lock-keywords-2
   vhdl-ext-font-lock-keywords-5))

;;;; Setup
(defun vhdl-ext-font-lock-setup ()
  "Setup syntax highlighting of VHDL buffers.
Add `vhdl-ext-mode' font lock keywords before running
`vhdl-mode' in order to populate `font-lock-keywords-alist'
before `font-lock' is loaded."
  (font-lock-add-keywords 'vhdl-mode vhdl-ext-font-lock-keywords 'set))


(provide 'vhdl-ext-font-lock)

;;; vhdl-ext-font-lock.el ends here
