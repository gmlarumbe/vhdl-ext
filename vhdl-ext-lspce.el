;;; vhdl-ext-lspce.el --- VHDL lspce setup  -*- lexical-binding: t -*-

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

;; Support for various VHDL language servers:
;;     - rust_hdl: https://github.com/VHDL-LS/rust_hdl.git
;;     - ghdl_language_server: https://github.com/ghdl/ghdl-language-server.git
;;     - hdl_checker: https://github.com/suoto/hdl_checker
;;     - vhdl-tool: http://vhdltool.com (INFO: Untested since LSP seems a premium feature)

;;; Code:

(require 'lspce nil :noerror) ; Set to :noerror since `lspce' is not available in MELPA
(require 'vhdl-ext-utils)

(defvar vhdl-ext-lspce-default-server 've-rust-hdl)

;;; Set server
(defun vhdl-ext-lspce-set-server (server-id)
  "Configure VHDL for `lspce' with SERVER-ID server.
Override any previous configuration for `vhdl-mode' and `vhdl-ts-mode'."
  (interactive (list (intern (completing-read "Server-id: " vhdl-ext-lsp-server-ids nil t))))
  (let* ((cmd (car (alist-get server-id vhdl-ext-lsp-available-servers))))
    (unless (featurep 'lspce)
      (user-error "lspce not available: check README.md on https://github.com/zbelial/lspce"))
    (unless cmd
      (error "%s not recognized as a supported server" server-id))
    (if (not (executable-find (if (listp cmd)
                                  (car cmd)
                                cmd)))
        (message "%s not in $PATH, skipping config..." server-id)
      ;; Else configure available server
      (setq lspce-server-programs (assoc-delete-all "vhdl" lspce-server-programs))
      (if (listp cmd)
          (push `("vhdl" ,(car cmd) ,(cadr cmd)) lspce-server-programs)
        (push `("vhdl" ,cmd "") lspce-server-programs))
      ;; Some reporting
      (message "[vhdl-ext lspce]: %s" server-id))))


(provide 'vhdl-ext-lspce)

;;; vhdl-ext-lspce.el ends here

;; Silence all the lspce byte-compiler warnings:
;;
;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:

