;;; vhdl-ext-lsp-bridge.el --- vhdl-ext lsp-bridge setup -*- lexical-binding: t -*-

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

(require 'lsp-bridge nil :noerror) ; Set to :noerror since `lsp-bridge' is not available in MELPA
(require 'vhdl-ext-utils)

(defconst vhdl-ext-lsp-bridge-langserver-dir
  (expand-file-name "langserver" (file-name-directory (or load-file-name (buffer-file-name)))))

(defvar vhdl-ext-lsp-bridge-default-server 've-rust-hdl)

(defun vhdl-ext-lsp-bridge-set-server (server-id)
  "Configure VHDL for `lsp-bridge' with SERVER-ID server.
Override any previous configuration for `vhdl-mode' and `vhdl-ts-mode'."
  (interactive (list (intern (completing-read "Server-id: " vhdl-ext-lsp-server-ids nil t))))
  (let ((cmd (car (alist-get server-id vhdl-ext-lsp-available-servers)))
        (cfg-file (cadr (alist-get server-id vhdl-ext-lsp-available-servers)))
        (cfg-dir vhdl-ext-lsp-bridge-langserver-dir))
    (unless (featurep 'lsp-bridge)
      (user-error "lsp-bridge not available: check Installation section on https://github.com/manateelazycat/lsp-bridge"))
    (unless cmd
      (error "%s not recognized as a supported server" server-id))
    (if (not (executable-find (if (listp cmd)
                                  (car cmd)
                                cmd)))
        (message "%s not in $PATH, skipping config..." server-id)
      ;; Else configure available server
      (dolist (mode '(vhdl-mode vhdl-ts-mode))
        (setq lsp-bridge-single-lang-server-mode-list (assq-delete-all mode lsp-bridge-single-lang-server-mode-list))
        (push (cons mode (file-name-concat cfg-dir cfg-file)) lsp-bridge-single-lang-server-mode-list))
      ;; Some reporting
      (message "[vhdl-ext lsp-bridge]: %s" server-id))))



(provide 'vhdl-ext-lsp-bridge)

;;; vhdl-ext-lsp-bridge.el ends here

;; Silence all the lsp-bridge byte-compiler warnings:
;;
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
