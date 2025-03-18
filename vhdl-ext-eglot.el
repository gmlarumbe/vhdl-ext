;;; vhdl-ext-eglot.el --- VHDL eglot -*- lexical-binding: t -*-

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
;;     - hdl_checker: https://github.com/suoto/hdl_checker
;;     - vhdl-tool: http://vhdltool.com (INFO: Untested since LSP seems a premium feature)

;;; Code:

(require 'eglot)
(require 'vhdl-ext-utils)

(defvar vhdl-ext-eglot-default-server 've-rust-hdl)

(defun vhdl-ext-eglot-set-server (server-id)
  "Configure VHDL for `eglot' for selected SERVER-ID.
Override any previous configuration for `vhdl-mode' and `vhdl-ts-mode'."
  (interactive (list (intern (completing-read "Server-id: " vhdl-ext-lsp-server-ids nil t))))
  (let ((cmd (car (alist-get server-id vhdl-ext-lsp-available-servers))))
    (unless cmd
      (error "%s not recognized as a supported server" server-id))
    (if (not (executable-find (if (listp cmd)
                                  (car cmd)
                                cmd)))
        (message "%s not in $PATH, skipping config..." server-id)
      ;; Else configure available server
      (dolist (mode '(vhdl-mode vhdl-ts-mode))
        (setq eglot-server-programs (assq-delete-all mode eglot-server-programs))
        (if (listp cmd)
            (push `(,mode ,@cmd) eglot-server-programs)
          (push `(,mode ,cmd) eglot-server-programs)))
      (message "[vhdl-ext eglot]: %s" server-id))))


(provide 'vhdl-ext-eglot)

;;; vhdl-ext-eglot.el ends here
