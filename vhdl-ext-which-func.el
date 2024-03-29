;;; vhdl-ext-which-func.el --- Vhdl-ext Which Func  -*- lexical-binding: t -*-

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

;; `which-func' integration.

;;; Code:

(require 'which-func)
(require 'vhdl-ext-nav)

(defvar-local vhdl-ext-which-func-extra nil
  "Variable to hold extra information for `which-func'.")

(defun vhdl-ext-which-func-shorten-block (block-type)
  "Return shortened name of BLOCK-TYPE, if possible."
  (cond ((string= "entity"        block-type) "ent")
        ((string= "architecture"  block-type) "arch")
        ((string= "package"       block-type) "pkg")
        ((string= "configuration" block-type) "cfg")
        ((string= "context"       block-type) "ctx")
        ((string= "function"      block-type) "func")
        ((string= "procedure"     block-type) "pcd")
        ((string= "component"     block-type) "comp")
        ((string= "process"       block-type) "proc")
        ((string= "generate"      block-type) "gen")
        (t block-type)))

(defun vhdl-ext-which-func-function ()
  "Retrieve `which-func' candidates."
  (let (data)
    (cond ((setq data (vhdl-ext-instance-at-point))
           (setq vhdl-ext-which-func-extra (cadr data))
           (car data))
          ((setq data (vhdl-ext-block-at-point))
           (setq vhdl-ext-which-func-extra (alist-get 'name data))
           (vhdl-ext-which-func-shorten-block (alist-get 'type data)))
          (t
           (setq vhdl-ext-which-func-extra nil)
           ""))))

(defun vhdl-ext-which-func ()
  "Hook for `vhdl-mode' to enable `which-func'."
  (setq-local which-func-functions '(vhdl-ext-which-func-function))
  (setq-local which-func-format
              `("["
                (:propertize which-func-current
                 face (which-func :weight bold)
                 mouse-face mode-line-highlight)
                ":"
                (:propertize vhdl-ext-which-func-extra
                 face which-func
                 mouse-face mode-line-highlight)
                "]")))


(provide 'vhdl-ext-which-func)

;;; vhdl-ext-which-func.el ends here
