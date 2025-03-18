;;; vhdl-ext-test-setup-package-test.el --- vhdl-ext ERT Basic Test with package.el  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/test-hdl

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
;;
;; vhdl-ext ERT Basic Test with package.el
;;
;; - Download package from MELPA, open VHDL file and ensure that
;;  `vhdl-ext-mode' is enabled without compilation or runtime errors.
;;
;; INFO: Packages downloaded from MELPA (not MELPA Stable) will fetch the
;; snapshot of the latest commit in the corresponding Git repo and its
;; dependencies. It would therefore have the same effect as testing with
;; straight but with the issue that test/ code in the repo would not be in sync
;; with the code of the downloaded package until the snapshot is updated
;; (various times per day).
;;
;; For MELPA Stable this is different since package.el will download the tagged
;; version of the repo and all its dependencies.
;;
;;; Code:

;;;; Setup package.el
;; INFO: Do not use MELPA-Stable for basic test
(setq test-hdl-setup-package-archives '(("melpa" . "https://melpa.org/packages/")))
(require 'test-hdl-setup-package)

;;;; Install/setup package
(message "Installing and setting up vhdl-ext")
(package-install 'vhdl-ext)
(require 'vhdl-ext)
(setq vhdl-ext-feature-list (remove 'lsp-bridge vhdl-ext-feature-list)) ; Do not autosetup since `lsp-bridge' is not available on MELPA
(setq vhdl-ext-feature-list (remove 'lspce vhdl-ext-feature-list)) ; Do not autosetup since `lspce' is not available on MELPA
(vhdl-ext-mode-setup)
(add-hook 'vhdl-mode-hook #'vhdl-ext-mode)


;;;; package.el CI test function
(defun vhdl-ext-test-setup-package-test-basic ()
  "Basic test for package.el."
  (let ((test-file (file-name-concat vhdl-ext-test-files-common-dir "axi_if_converter.vhd")))
    (find-file test-file)
    (if (not vhdl-ext-mode)
        (error "Error with package.el: Could not open %s with `vhdl-ext-mode'" buffer-file-name)
      (message "Opened file: %s, with `vhdl-ext-mode': %s" buffer-file-name vhdl-ext-mode))))


(provide 'vhdl-ext-test-setup-package-test)

;;; vhdl-ext-test-setup-package-test.el ends here
