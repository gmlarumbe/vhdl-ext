;;; vhdl-ext-test-xref.el --- vhdl-ext ERT xref tests  -*- lexical-binding: t -*-

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
;; vhdl-ext ERT xref tests
;;
;;; Code:

(defconst vhdl-ext-test-ref-dir-xref (file-name-concat vhdl-ext-test-ref-dir "xref"))
(defconst vhdl-ext-test-dump-dir-xref (file-name-concat vhdl-ext-test-dump-dir "xref"))

;; INFO: For these tests using a file should not be needed, as they are project
;; and tags table dependant.  However, for ease of implementation with current
;; test-hdl infrastructure the files where these tags appear are used.
(defconst vhdl-ext-test-xref-file-and-refs-alist
  `((,(file-name-concat vhdl-ext-test-files-common-dir "axi_if_converter.vhd")
     "axi_if_converter"                  ; entity
     "clk"                               ; port (very common signal)
     "s_axi_aclk"                        ; port
     "RTL"                               ; arch
     "axi_lite_regs"                     ; instance entity name
     "I_AXI_LITE_REGS"                   ; instance name
     "soft_reset"                        ; internal signal
     "fb_send_size_l")                   ; concurrent assignment
    (,(file-name-concat vhdl-ext-test-files-common-dir "global_sim.vhd")
     "global_sim"                       ; package & body declaration
     "AXI_CLK_T"                        ; constant declaration
     "read_control_reg")                ; procedure declaration
    ))


(cl-defun vhdl-ext-test-xref-fn (&key refs type)
  (let (ret-val)
    (dolist (ref refs (nreverse ret-val))
      (push (vhdl-ext-xref--find-symbol ref type) ret-val))))


(defun vhdl-ext-test-xref-gen-expected-files ()
  (let ((vhdl-ext-tags-defs-table nil)
        (vhdl-ext-tags-refs-table nil)
        (vhdl-ext-tags-inst-table nil))
    (vhdl-ext-test-with-test-project vhdl-ext-test-tags-proj-name
      ;; Generate/update tags for test project
      (vhdl-ext-test-tags-get :root vhdl-ext-test-files-common-dir
                              :files vhdl-ext-test-common-file-list
                              :rel-path t)
      ;; Iterate over files with tags tables
      (dolist (file-refs vhdl-ext-test-xref-file-and-refs-alist)
        (let ((file (car file-refs))
              (refs (cdr file-refs)))
          ;; Defs
          (test-hdl-gen-expected-files :file-list `(,file)
                                       :dest-dir vhdl-ext-test-ref-dir-xref
                                       :out-file-ext "xref.defs.el"
                                       :process-fn 'eval
                                       :fn #'vhdl-ext-test-xref-fn
                                       :args `(:refs ,refs
                                               :type def))
          (test-hdl-gen-expected-files :file-list `(,file)
                                       :dest-dir vhdl-ext-test-ref-dir-xref
                                       :out-file-ext "xref.refs.el"
                                       :process-fn 'eval
                                       :fn #'vhdl-ext-test-xref-fn
                                       :args `(:refs ,refs
                                               :type ref)))))))


(ert-deftest xref::defs ()
  (let ((vhdl-ext-tags-defs-table nil)
        (vhdl-ext-tags-refs-table nil)
        (vhdl-ext-tags-inst-table nil))
    (vhdl-ext-test-with-test-project vhdl-ext-test-tags-proj-name
      ;; Generate/update tags for test project
      (vhdl-ext-test-tags-get :root vhdl-ext-test-files-common-dir
                              :files vhdl-ext-test-common-file-list
                              :rel-path t)
      ;; Iterate over files with tags tables
      (dolist (file-refs vhdl-ext-test-xref-file-and-refs-alist)
        (let ((file (car file-refs))
              (refs (cdr file-refs)))
          ;; Defs
          (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                               :dump-file (file-name-concat vhdl-ext-test-dump-dir-xref (test-hdl-basename file "xref.defs.el"))
                                                               :process-fn 'eval
                                                               :fn #'vhdl-ext-test-xref-fn
                                                               :args `(:refs ,refs
                                                                       :type def))
                                        (file-name-concat vhdl-ext-test-ref-dir-xref (test-hdl-basename file "xref.defs.el")))))))))

(ert-deftest xref::refs ()
  (let ((vhdl-ext-tags-defs-table nil)
        (vhdl-ext-tags-refs-table nil)
        (vhdl-ext-tags-inst-table nil))
    (vhdl-ext-test-with-test-project vhdl-ext-test-tags-proj-name
      ;; Generate/update tags for test project
      (vhdl-ext-test-tags-get :root vhdl-ext-test-files-common-dir
                              :files vhdl-ext-test-common-file-list
                              :rel-path t)
      ;; Iterate over files with tags tables
      (dolist (file-refs vhdl-ext-test-xref-file-and-refs-alist)
        (let ((file (car file-refs))
              (refs (cdr file-refs)))
          ;; Refs
          (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                               :dump-file (file-name-concat vhdl-ext-test-dump-dir-xref (test-hdl-basename file "xref.refs.el"))
                                                               :process-fn 'eval
                                                               :fn #'vhdl-ext-test-xref-fn
                                                               :args `(:refs ,refs
                                                                       :type ref))
                                        (file-name-concat vhdl-ext-test-ref-dir-xref (test-hdl-basename file "xref.refs.el")))))))))


(provide 'vhdl-ext-test-xref)

;;; vhdl-ext-test-xref.el ends here
