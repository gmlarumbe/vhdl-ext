;;; vhdl-ext-hs.el --- Vhdl-ext Hideshow  -*- lexical-binding: t -*-

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

;; `hideshow' configuration for code folding:
;;   - Add support for code folding of if/else blocks

;;; Code:

(require 'vhdl-mode)
(require 'hideshow)

(defconst vhdl-ext-hs-start-regexp
  (concat "\\(\\_<\\(then\\|else\\)\\_>\\|" vhdl-hs-start-regexp "\\)"))

(defun vhdl-ext-hs-forward-sexp-func (count)
  "Wrapper for `vhdl-hs-forward-sexp-func' adding hideshow for if/else blocks.

Move forward COUNT s-expressions."
  (cond ((looking-at "\\_<then\\_>") ; if/elsif
         (vhdl-forward-sexp)
         (cond ((looking-at "\\s-*;") ; if w/o else/elsif
                (goto-char (match-end 0)))
               ((looking-at "\\_<else\\_>") ; if with else
                (point))
               ((save-excursion
                  (backward-word)
                  (looking-at "\\_<elsif\\_>")) ; if with elsif
                (backward-word)
                (point))))
        ((looking-at "\\_<else\\_>") ; else
         (vhdl-forward-sexp)
         (vhdl-re-search-forward "if\\s-*;" nil t))
        (t
         (vhdl-hs-forward-sexp-func count))))

(defun vhdl-ext-hs-setup ()
  "Configure `hideshow'."
  (dolist (mode '((vhdl-mode    . vhdl-ext-hs-forward-sexp-func)
                  (vhdl-ts-mode . vhdl-ts-forward-sexp)))
    (add-to-list 'hs-special-modes-alist `(,(car mode)
                                           ,vhdl-ext-hs-start-regexp
                                           nil
                                           "--\\( \\|$\\)"
                                           ,(cdr mode))))
  (dolist (hook '(vhdl-mode-hook vhdl-ts-mode-hook))
    (add-hook hook #'hs-minor-mode))
  ;; Workaround to enable `hideshow' on first file visit with lazy loading using
  ;; :config section with `use-package'
  (when (member major-mode '(vhdl-mode vhdl-ts-mode))
    (hs-minor-mode 1)))


(provide 'vhdl-ext-hs)

;;; vhdl-ext-hs.el ends here
