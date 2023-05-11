;;; vhdl-ext-tests-utils.el --- vhdl-ext ERT utils tests  -*- lexical-binding: t -*-

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
;; ERT Utils Tests
;;
;;; Code:


(defmacro vhdl-ext-test-utils-file (file &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert-file-contents (file-name-concat vhdl-ext-tests-common-dir ,file))
     (cl-letf (((symbol-function 'message)
                (lambda (FORMAT-STRING &rest ARGS)
                  nil))) ; Mock `message' to silence VHDL version reporting
       (vhdl-mode))
     ,@body))


(ert-deftest utils::scan-buffer-modules ()
  (should (equal (vhdl-ext-test-utils-file "tb_axi_if_converter.vhd"
                   (vhdl-ext-scan-buffer-entities))
                 '("tb_axi_if_converter")))
  (should (equal (vhdl-ext-test-utils-file "axi_if_converter.vhd"
                   (vhdl-ext-scan-buffer-entities))
                 '("axi_if_converter")))
  (should (equal (vhdl-ext-test-utils-file "global_pkg.vhd"
                   (vhdl-ext-scan-buffer-entities))
                 nil)))



(provide 'vhdl-ext-tests-utils)

;;; vhdl-ext-tests-utils.el ends here
