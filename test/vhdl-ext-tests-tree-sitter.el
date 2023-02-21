;;; vhdl-ext-tests-tree-sitter.el --- vhdl-ext ERT tree-sitter tests  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

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
;;
;; ERT Tree-sitter tests:
;;
;;; Code:


(require 'vhdl-ext-tests-font-lock)

(ert-deftest tree-sitter::font-lock ()
  (let ((default-directory vhdl-ext-tests-common-dir)
        (faceup-test-explain t))
    (dolist (file (directory-files vhdl-ext-tests-common-dir nil ".vhd$"))
      (should (eq t (vhdl-ext-test-font-lock-test-file file :tree-sitter))))))


(provide 'vhdl-ext-tests-tree-sitter)

;;; vhdl-ext-tests-tree-sitter.el ends here

