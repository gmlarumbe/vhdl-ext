;;; vhdl-ext-templates.el --- VHDL Templates -*- lexical-binding: t -*-

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

;; Templates

;;; Code:

(require 'hydra)
(require 'vhdl-ext-utils)

(defun vhdl-ext-entity-from-file (file &optional port-copy)
  "Find first entity declaration in FILE.
If optional arg PORT-COPY is non-nil, get generic and port information from an
entity or component declaration via `vhdl-port-copy'."
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward vhdl-ext-entity-re nil t)
      (when port-copy
        (save-match-data
          (vhdl-port-copy)))
      (match-string-no-properties 2))))

(defun vhdl-ext-insert-instance-from-file (file)
  "Insert entity instance from FILE."
  (interactive "FSelect entity from file:")
  (let* ((entity-name (vhdl-ext-entity-from-file file :port-copy))
         (instance-name (read-string "Instance-name: " (concat "I_" (upcase entity-name)))))
    (vhdl-port-paste-instance instance-name)))

(defun vhdl-ext-insert-testbench-from-file (file outfile)
  "Create testbench from entity of selected FILE in OUTFILE."
  (interactive "FSelect entity from file:\nFOutput file: ")
  (when (file-exists-p outfile)
    (error "File %s exists" outfile))
  (vhdl-ext-entity-from-file file :port-copy)
  (find-file outfile)
  (vhdl-port-paste-testbench))


;;;; Hydra
(defhydra vhdl-ext-hydra (:color blue
                          :hint nil)
  ("ac"  (vhdl-template-architecture) "architecture" :column "A-C")
  ("al"  (vhdl-template-alias)        "alias")
  ("as"  (vhdl-template-assert)       "assert")
  ("at"  (vhdl-template-attribute)    "attribute")
  ("cc"  (vhdl-template-case)         "case")
  ("cp"  (vhdl-template-component)    "component")
  ("ct"  (vhdl-template-constant)     "constant")
  ("en"  (vhdl-template-entity)       "entity" :column "E-G")
  ("fl"  (vhdl-template-file)         "file")
  ("fn"  (vhdl-template-function)     "function")
  ("for" (vhdl-template-for)          "for")
  ("ge"  (vhdl-template-generic)      "generic")
  ("gn"  (vhdl-template-generate)     "generate")
  ("hd"  (vhdl-template-header)       "header" :column "H-P")
  ("if"  (vhdl-template-if-then)      "if-then")
  ("pc"  (vhdl-template-process-comb) "process comb")
  ("pb"  (vhdl-template-package-body) "package body")
  ("pd"  (vhdl-template-package-decl) "package decl")
  ("pkg" (call-interactively #'vhdl-template-insert-package) "library package")
  ("pr"  (vhdl-template-procedure)    "procedure")
  ("ps"  (vhdl-template-process-seq)  "process seq")
  ("rp"  (vhdl-template-report)       "report" :column "R-W")
  ("sg"  (vhdl-template-signal)       "signal")
  ("ty"  (vhdl-template-type)         "type")
  ("va"  (vhdl-template-variable)     "variable")
  ("wh"  (vhdl-template-while-loop)   "while")

  ("@"   (vhdl-template-clocked-wait) "clocked wait" :column "Others")
  ("IS"  (call-interactively #'vhdl-ext-insert-instance-from-file) "Instance")
  ("TS"  (call-interactively #'vhdl-ext-insert-testbench-from-file) "Testbench")

  ;;;;;;;;;;
  ;; Exit ;;
  ;;;;;;;;;;
  ("q"   nil nil :color blue)
  ("C-g" nil nil :color blue))


(provide 'vhdl-ext-templates)

;;; vhdl-ext-templates.el ends here

;; Silence Hydra byte-compiler docstring warnings
;;
;; Local Variables:
;; byte-compile-warnings: (not docstrings)
;; End:
