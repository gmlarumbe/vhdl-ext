;;; vhdl-ext-test-imenu.el --- vhdl-ext ERT imenu tests  -*- lexical-binding: t -*-

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
;; vhdl-ext ERT imenu tests
;;
;;; Code:

(defconst vhdl-ext-test-imenu-file-list vhdl-ext-test-common-file-list)

(defconst vhdl-ext-test-ref-dir-imenu (file-name-concat vhdl-ext-test-ref-dir "imenu"))
(defconst vhdl-ext-test-dump-dir-imenu (file-name-concat vhdl-ext-test-dump-dir "imenu"))


(defun vhdl-ext-test-imenu-gen-expected-files ()
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-imenu-file-list
                               :dest-dir vhdl-ext-test-ref-dir-imenu
                               :out-file-ext "el"
                               :process-fn 'eval-ff
                               :fn #'test-hdl-imenu-test-file
                               :args '(vhdl-mode)))

(ert-deftest imenu ()
  (dolist (file vhdl-ext-test-imenu-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-imenu (test-hdl-basename file "el"))
                                                         :process-fn 'eval-ff
                                                         :fn #'test-hdl-imenu-test-file
                                                         :args '(vhdl-mode))
                                  (file-name-concat vhdl-ext-test-ref-dir-imenu (test-hdl-basename file "el"))))))


(provide 'vhdl-ext-test-imenu)

;;; vhdl-ext-test-imenu.el ends here
