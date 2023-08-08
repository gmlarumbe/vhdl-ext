;;; vhdl-ext-tests-hierarchy.el --- Vhdl-Ext ERT Hierarchy  -*- lexical-binding: t -*-

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
;; ERT Tests for hierarchy
;;
;;; Code:


(defun vhdl-ext-test-hierarchy ()
  (let* ((test-file (file-name-concat vhdl-ext-tests-files-dir "common/instances.vhd"))
         (vhdl-standard '(8 nil)) ; If using std93, ghdl will not detect block0 instances
         (lib-directory "lib/")
         (vhdl-project-alist `((;; Name
                                "test"
                                ;; Title
                                "vhdl-ext hierarchy tests"
                                ;; Default directory
                                ,(file-name-as-directory vhdl-ext-tests-files-dir)
                                ;; Sources (only RTL included) as directories (could be included
                                ;; recursively or individually)
                                ("jump-parent/block0.vhd"
                                 "jump-parent/block1.vhd"
                                 "common/instances.vhd")
                                ;; Exclude regexp
                                ""
                                ;; Compile options
                                nil
                                ;; Compile directory
                                "./"
                                ;; Library name
                                "work"
                                ;; Library directory
                                ,lib-directory
                                ;; Makefile name
                                ""
                                ;; Description
                                ""))))
    (cl-letf (((symbol-function 'vhdl-ext-hierarchy-twidget-display)
               (lambda (hierarchy)
                 hierarchy))
              ((symbol-function 'message)
               (lambda (FORMAT-STRING &rest ARGS)
                 nil)))
      ;; vhdl-ext cannot use temp-buffer since it relies on buffer visiting file of directory of `vhdl-project-alist'
      (save-window-excursion
        (find-file test-file)
        (vhdl-mode)
        (cond (;; builtin-hierarchy
               (and (eq vhdl-ext-hierarchy-backend 'builtin)
                    (eq vhdl-ext-hierarchy-frontend 'hierarchy))
               (vhdl-ext-hierarchy-builtin-parse)
               (vhdl-ext-hierarchy-current-buffer))
              ;; GHDL-hierarchy
              ((and (eq vhdl-ext-hierarchy-backend 'ghdl)
                    (eq vhdl-ext-hierarchy-frontend 'hierarchy))
               (make-directory (file-name-concat (file-name-as-directory vhdl-ext-tests-files-dir) lib-directory) :parents)
               (vhdl-ext-hierarchy-current-buffer))
              ;; builtin-outshine
              ((and (eq vhdl-ext-hierarchy-backend 'builtin)
                    (eq vhdl-ext-hierarchy-frontend 'outshine))
               (vhdl-ext-hierarchy-builtin-parse)
               (vhdl-ext-hierarchy-current-buffer)
               (buffer-substring-no-properties (point-min) (point-max)))
              ;; GHDL-outshine
              ((and (eq vhdl-ext-hierarchy-backend 'ghdl)
                    (eq vhdl-ext-hierarchy-frontend 'outshine))
               (make-directory (file-name-concat (file-name-as-directory vhdl-ext-tests-files-dir) lib-directory) :parents)
               (vhdl-ext-hierarchy-current-buffer)
               (buffer-substring-no-properties (point-min) (point-max)))
              ;; Fallback
              (t
               (error "Not a proper backend-frontend combination!")))))))


(ert-deftest hierarchy::builtin-hierarchy ()
  (let ((vhdl-ext-hierarchy-backend  'builtin)
        (vhdl-ext-hierarchy-frontend 'hierarchy))
    (should (string= (with-temp-buffer
                       (hierarchy-print (vhdl-ext-test-hierarchy) (lambda (node) node))
                       (buffer-substring-no-properties (point-min) (point-max)))
"instances
  instances.block0:I_BLOCK0_0
  instances.block0:I_BLOCK0_1
  instances.block0:I_BLOCK0_2
  instances.block1:I_BLOCK1_0
  instances.block1:I_BLOCK1_1
  instances.block1:I_BLOCK1_2
  instances.block1:I_BLOCK1_GEN
"))))


(ert-deftest hierarchy::ghdl-hierarchy ()
  (let ((vhdl-ext-hierarchy-backend  'ghdl)
        (vhdl-ext-hierarchy-frontend 'hierarchy))
    (should (string= (with-temp-buffer
                       (hierarchy-print (vhdl-ext-test-hierarchy) (lambda (node) node))
                       (buffer-substring-no-properties (point-min) (point-max)))
"instances
  instances.block0:i_block0_0
  instances.block0:i_block0_1
  instances.block0:i_block0_2
  instances.block1:i_block1_0
  instances.block1:i_block1_1
  instances.block1:i_block1_2
  instances.gen_block1(0):for-generate
    instances.gen_block1(0):for-generate.block1:i_block1_gen
  instances.gen_block1(1):for-generate
    instances.gen_block1(1):for-generate.block1:i_block1_gen
  instances.gen_block1(2):for-generate
    instances.gen_block1(2):for-generate.block1:i_block1_gen
  instances.gen_block1(3):for-generate
    instances.gen_block1(3):for-generate.block1:i_block1_gen
"
))))


(ert-deftest hierarchy::builtin-outshine ()
  (let ((vhdl-ext-hierarchy-backend  'builtin)
        (vhdl-ext-hierarchy-frontend 'outshine))
    (should (string= (vhdl-ext-test-hierarchy)
"-- Hierarchy generated by `vhdl-ext'

-- * instances
-- ** block0
-- ** block0
-- ** block0
-- ** block1
-- ** block1
-- ** block1
-- ** block1


-- * Buffer local variables
-- Local Variables:
-- eval: (vhdl-mode 1)
-- eval: (vhdl-ext-hierarchy-outshine-nav-mode 1)
-- End:
"))))


(ert-deftest hierarchy::ghdl-outshine ()
  (let ((vhdl-ext-hierarchy-backend  'ghdl)
        (vhdl-ext-hierarchy-frontend 'outshine))
    (should (string= (vhdl-ext-test-hierarchy)
"-- Hierarchy generated by `vhdl-ext'

-- * instances
-- ** block0
-- ** block0
-- ** block0
-- ** block1
-- ** block1
-- ** block1
-- ** gen_block1(0):for-generate
-- *** block1
-- ** gen_block1(1):for-generate
-- *** block1
-- ** gen_block1(2):for-generate
-- *** block1
-- ** gen_block1(3):for-generate
-- *** block1


-- * Buffer local variables
-- Local Variables:
-- eval: (vhdl-mode 1)
-- eval: (vhdl-ext-hierarchy-outshine-nav-mode 1)
-- End:
"))))


(provide 'vhdl-ext-tests-hierarchy)

;;; vhdl-ext-tests-hierarchy.el ends here
