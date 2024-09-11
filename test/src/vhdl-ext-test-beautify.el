;;; vhdl-ext-test-beautify.el --- vhdl-ext ERT beautify tests  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Gonzalo Larumbe

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
;; vhdl-ext ERT beautify tests
;;
;;; Code:

(defconst vhdl-ext-test-ref-dir-beautify (file-name-concat vhdl-ext-test-ref-dir "beautify"))
(defconst vhdl-ext-test-dump-dir-beautify (file-name-concat vhdl-ext-test-dump-dir "beautify"))

(defconst vhdl-ext-test-beautify-instance-at-point-file-list (mapcar (lambda (file)
                                                                       (file-name-concat vhdl-ext-test-files-common-dir file))
                                                                     '("axi_if_converter.vhd" "hierarchy.vhd" "instances.vhd")))
(defconst vhdl-ext-test-beautify-block-at-point-file-list (mapcar (lambda (file)
                                                                    (file-name-concat vhdl-ext-test-files-common-dir file))
                                                                  '("global_pkg.vhd" "global_sim.vhd" "sexp.vhd" "tb_axi_if_converter.vhd")))

(defconst vhdl-ext-test-beautify-block-at-point-re
  `(,vhdl-ext-entity-re
    ,vhdl-ext-architecture-re
    ,vhdl-ext-package-re
    ,vhdl-ext-configuration-re
    ,vhdl-ext-context-re
    ,vhdl-ext-block-re
    ,vhdl-ext-component-re
    ,vhdl-ext-process-re
    ,vhdl-ext-procedure-re
    ,vhdl-ext-function-re
    ,vhdl-ext-instance-re))


(defun vhdl-ext-test-beautify-file (mode fn)
  (test-hdl-no-messages
    (funcall mode))
  ;; Deindent
  (let ((blank-re "^\\s-+"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward blank-re nil t)
        (replace-match ""))))
  ;; Remove spaces in port connections
  (let ((port-conn-re (concat "\\(?1:^\\s-*" vhdl-ext-identifier-re "\\)\\(?2:\\s-*\\)=>")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward port-conn-re nil t)
        (replace-match "\\1=>"))))
  (test-hdl-no-messages
    (funcall fn)))

(defun vhdl-ext-test-beautify-instance-at-point-fn ()
  (goto-char (point-min))
  (while (vhdl-ext-find-entity-instance-fwd)
    (vhdl-ext-beautify-instance-at-point)))

(defun vhdl-ext-test-beautify-block-at-point-fn ()
  (mapc (lambda (re)
          (goto-char (point-min))
          (while (vhdl-re-search-forward re nil t)
            (when (ignore-errors (vhdl-ext-block-at-point))
              (vhdl-ext-beautify-block-at-point))))
        vhdl-ext-test-beautify-block-at-point-re))


(defun vhdl-ext-test-beautify-gen-expected-files ()
  ;; Instance at point
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-beautify-instance-at-point-file-list
                               :dest-dir vhdl-ext-test-ref-dir-beautify
                               :out-file-ext "beauty.inst.vhd"
                               :fn #'vhdl-ext-test-beautify-file
                               :args '(vhdl-mode vhdl-ext-test-beautify-instance-at-point-fn))
  ;; Block at point
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-beautify-block-at-point-file-list
                               :dest-dir vhdl-ext-test-ref-dir-beautify
                               :out-file-ext "beauty.block.vhd"
                               :fn #'vhdl-ext-test-beautify-file
                               :args '(vhdl-mode vhdl-ext-test-beautify-block-at-point-fn))
  ;; Block at point (tree-sitter)
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-beautify-block-at-point-file-list
                               :dest-dir vhdl-ext-test-ref-dir-beautify
                               :out-file-ext "ts.beauty.block.vhd"
                               :fn #'vhdl-ext-test-beautify-file
                               :args '(vhdl-ts-mode vhdl-ext-test-beautify-block-at-point-fn)))

(ert-deftest beautify-instance-at-point ()
  (dolist (file vhdl-ext-test-beautify-instance-at-point-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-beautify (test-hdl-basename file "beauty.inst.vhd"))
                                                         :fn #'vhdl-ext-test-beautify-file
                                                         :args '(vhdl-mode vhdl-ext-test-beautify-instance-at-point-fn))
                                  (file-name-concat vhdl-ext-test-ref-dir-beautify (test-hdl-basename file "beauty.inst.vhd"))))))

(ert-deftest beautify-block-at-point ()
  (dolist (file vhdl-ext-test-beautify-block-at-point-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-beautify (test-hdl-basename file "beauty.block.vhd"))
                                                         :fn #'vhdl-ext-test-beautify-file
                                                         :args '(vhdl-mode vhdl-ext-test-beautify-block-at-point-fn))
                                  (file-name-concat vhdl-ext-test-ref-dir-beautify (test-hdl-basename file "beauty.block.vhd"))))))

(ert-deftest beautify-block-at-point-ts-mode ()
  (dolist (file vhdl-ext-test-beautify-block-at-point-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-beautify (test-hdl-basename file "ts.beauty.block.vhd"))
                                                         :fn #'vhdl-ext-test-beautify-file
                                                         :args '(vhdl-ts-mode vhdl-ext-test-beautify-block-at-point-fn))
                                  (file-name-concat vhdl-ext-test-ref-dir-beautify (test-hdl-basename file "ts.beauty.block.vhd"))))))


(provide 'vhdl-ext-test-beautify)

;;; vhdl-ext-test-beautify.el ends here
