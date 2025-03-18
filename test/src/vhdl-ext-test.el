;;; vhdl-ext-test.el --- vhdl-ext ERT tests  -*- lexical-binding: t -*-

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
;; vhdl-ext ERT tests
;;
;;; Code:


;; Allow loading of packages in Emacs interactive session
(defconst vhdl-ext-test-dir (file-name-parent-directory (file-name-directory (or load-file-name (buffer-file-name)))))
(defconst vhdl-ext-test-hdl-dir (file-name-concat vhdl-ext-test-dir "test-hdl"))

(unless noninteractive
  (dolist (dir `(,(file-name-concat vhdl-ext-test-dir "src")
                 ,vhdl-ext-test-hdl-dir))
    (unless (member dir load-path)
      (add-to-list 'load-path dir))))

(require 'test-hdl)
(require 'vhdl-ext)


;;;; Directories
(defconst vhdl-ext-test-ref-dir (file-name-concat vhdl-ext-test-dir "ref"))
(defconst vhdl-ext-test-dump-dir (file-name-concat vhdl-ext-test-dir "dump"))
(defconst vhdl-ext-test-files-dir (file-name-concat vhdl-ext-test-dir "files"))
(defconst vhdl-ext-test-files-common-dir (file-name-concat vhdl-ext-test-files-dir "common"))
(defconst vhdl-ext-test-axi-converter-dir (file-name-concat vhdl-ext-test-files-dir "axi_if_converter"))
(defconst vhdl-ext-test-axi-converter-rtl-dir (file-name-concat vhdl-ext-test-axi-converter-dir "rtl"))
(defconst vhdl-ext-test-axi-converter-tb-dir (file-name-concat vhdl-ext-test-axi-converter-dir "tb"))

(defconst vhdl-ext-test-common-file-list (test-hdl-directory-files vhdl-ext-test-files-common-dir
                                                                   vhdl-ext-file-extension-re))


;;;; Tests
(require 'vhdl-ext-test-faceup)
(require 'vhdl-ext-test-utils)
(require 'vhdl-ext-test-navigation)
(require 'vhdl-ext-test-imenu)
(require 'vhdl-ext-test-beautify)
(require 'vhdl-ext-test-hierarchy)
(require 'vhdl-ext-test-tags)
(require 'vhdl-ext-test-capf)
(require 'vhdl-ext-test-xref)


;;;; Aux funcs
(defun vhdl-ext-test-gen-expected-files ()
  (vhdl-ext-test-faceup-gen-expected-files)
  (vhdl-ext-test-utils-gen-expected-files)
  (vhdl-ext-test-navigation-gen-expected-files)
  (vhdl-ext-test-imenu-gen-expected-files)
  (vhdl-ext-test-beautify-gen-expected-files)
  (vhdl-ext-test-hierarchy-gen-expected-files)
  (vhdl-ext-test-tags-gen-expected-files)
  (vhdl-ext-test-capf-gen-expected-files)
  (vhdl-ext-test-xref-gen-expected-files))


(provide 'vhdl-ext-test)

;;; vhdl-ext-test.el ends here
