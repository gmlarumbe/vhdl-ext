;;; vhdl-ext-lsp.el --- VHDL Language Server Protocol -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

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
;;     - vhdl-tool: http://vhdltool.com
;;     - hdl_checker: https://github.com/suoto/hdl_checker

;;; Code:

(require 'lsp-mode)
(require 'lsp-vhdl)
(require 'vhdl-ext-utils)

(defvar vhdl-ext-lsp-mode-default-server 've-rust-hdl)

(defun vhdl-ext-lsp-setup ()
  "Configure VHDL for `lsp-mode'.
Register clients."
  (interactive)
  (let (server-id server-bin)
    ;; Add `vhdl-ts-mode' to the list of existing lsp ids
    (unless (alist-get 'vhdl-ts-mode lsp-language-id-configuration)
      (push (cons 'vhdl-ts-mode "vhdl") lsp-language-id-configuration))
    ;; Register clients
    (dolist (server vhdl-ext-lsp-available-servers)
      (setq server-id (car server))
      (setq server-bin (cadr server))
      (setq lsp-vhdl-server server-id)
      (lsp-register-client
       (make-lsp-client :new-connection (lsp-stdio-connection server-bin)
                        :major-modes '(vhdl-mode vhdl-ts-mode)
                        :server-id server-id))
      (message "Registered lsp-client: %s" server-id))))

(defun vhdl-ext-lsp-set-server (server-id)
  "Set language server defined by SERVER-ID.
Disable the rest to avoid handling priorities.
Override any previous configuration for `vhdl-mode' and `vhdl-ts-mode'."
  (interactive (list (intern (completing-read "Server-id: " vhdl-ext-lsp-server-ids nil t))))
  (let ((cmd (cadr (assoc server-id vhdl-ext-lsp-available-servers))))
    (if (not (executable-find (if (listp cmd)
                                  (car cmd)
                                cmd)))
        (message "%s not in $PATH, skipping config..." server-id)
      ;; Else configure available server
      (dolist (mode '(vhdl-mode vhdl-ts-mode))
        (setq lsp-disabled-clients (assq-delete-all mode lsp-disabled-clients))
        (push (cons mode (remove server-id vhdl-ext-lsp-server-ids)) lsp-disabled-clients))
      (message "[vhdl-ext lsp]: %s" server-id))))



(provide 'vhdl-ext-lsp)

;;; vhdl-ext-lsp.el ends here
