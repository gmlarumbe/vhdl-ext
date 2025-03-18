;;; vhdl-ext-test-hierarchy.el --- vhdl-ext ERT hierarchy tests  -*- lexical-binding: t -*-

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
;; vhdl-ext ERT hierarchy tests
;;
;;; Code:

(defconst vhdl-ext-test-ref-dir-hierarchy (file-name-concat vhdl-ext-test-ref-dir "hierarchy"))
(defconst vhdl-ext-test-dump-dir-hierarchy (file-name-concat vhdl-ext-test-dump-dir "hierarchy"))

(defconst vhdl-ext-test-hierarchy-axi-converter-rtl-file-list
  (test-hdl-directory-files vhdl-ext-test-axi-converter-rtl-dir vhdl-ext-file-extension-re))
(defconst vhdl-ext-test-hierarchy-axi-converter-tb-file-list
  (test-hdl-directory-files vhdl-ext-test-axi-converter-tb-dir vhdl-ext-file-extension-re))

(defconst vhdl-ext-test-hierarchy-file-list (mapcar (lambda (file)
                                                         (file-name-concat vhdl-ext-test-files-common-dir file))
                                                       '("instances.vhd"
                                                         "axi_if_converter.vhd"
                                                         "tb_axi_if_converter.vhd")))

(defconst vhdl-ext-test-hierarchy-sources-list
  (append (test-hdl-directory-files (file-name-concat vhdl-ext-test-files-dir "subblocks") vhdl-ext-file-extension-re)
          vhdl-ext-test-hierarchy-file-list
          vhdl-ext-test-hierarchy-axi-converter-rtl-file-list
          vhdl-ext-test-hierarchy-axi-converter-tb-file-list))

(defconst vhdl-ext-test-hierarchy-ghdl-instances-sources-list
  `(,(file-name-concat vhdl-ext-test-files-dir "subblocks" "block0.vhd")
    ,(file-name-concat vhdl-ext-test-files-dir "subblocks" "block1.vhd")
    ,(file-name-concat vhdl-ext-test-files-common-dir "instances.vhd"))
  "Ordered filelist for \"instances.vhd\" GHDL hierarchy extraction.")

(defconst vhdl-ext-test-hierarchy-ghdl-axi-converter-sources-list
  `(;; RTL sources
    ,@(mapcar (lambda (file)
                (file-name-concat vhdl-ext-test-axi-converter-rtl-dir file))
              '("global_pkg.vhd"
                "input_buffer_pkg.vhd"
                "BUFG.vhd"
                "clk_div.vhd"
                "clk_sync.vhd"
                "axi_lite_master.vhd"
                "axi_lite_regs.vhd"
                "core_converter.vhd"
                "core_fsm.vhd"
                "input_buffer.vhd"
                "pattern_counter.vhd"
                "axi_if_converter.vhd"))
    ;; TB sources
    ,@(mapcar (lambda (file)
                (file-name-concat vhdl-ext-test-axi-converter-tb-dir file))
              '("axil_slave_bfm.vhd"
                "axil_master_bfm.vhd"
                "axif_master_bfm.vhd"
                "global_sim.vhd"
                "s_axi_model.vhd"
                "tb_axi_if_converter.vhd")))
  "Ordered filelist for \"tb_axi_if_converter.vhd\" GHDL hierarchy extraction.")


(defun vhdl-ext-test-hierarchy--hierarchy-fn ()
  (let ((hier (vhdl-ext-hierarchy-current-buffer)))
    (with-temp-buffer
      (hierarchy-print hier (lambda (node) node))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun vhdl-ext-test-hierarchy--outshine-fn ()
  (vhdl-ext-hierarchy-current-buffer)
  (buffer-substring-no-properties (point-min) (point-max)))


(cl-defun vhdl-ext-test-hierarchy-buffer (&key mode backend frontend files dirs root worklib workdir entity)
  (let* (;; Backend selection
         (vhdl-ext-hierarchy-backend backend)
         (vhdl-ext-hierarchy-frontend frontend)
         ;; Do not use cached hierarchies
         (vhdl-ext-hierarchy-ghdl-alist nil)
         ;; If using std93, ghdl will not detect block0 instances
         (vhdl-standard '(8 nil))
         ;; `vhdl-ext-project-alist' settings
         (proj-name "vhdl-ext-test-hierarchy")
         (vhdl-ext-project-alist `((,proj-name
                                    :root ,(or root vhdl-ext-test-files-common-dir)
                                    :files ,files
                                    :dirs ,dirs
                                    :worklib ,worklib
                                    :workdir ,(or workdir "lib/")))))
    ;; Mock functions
    (cl-letf (((symbol-function 'vhdl-ext-hierarchy-twidget-display)
               (lambda (hierarchy)
                 hierarchy))
              ((symbol-function 'vhdl-ext-select-file-entity)
               (lambda (&optional file)
                 (or entity
                     (car (vhdl-ext-read-file-entities file))))))
      (test-hdl-no-messages
        ;; INFO: This one seems important to have a clear state on each file parsed.
        (vhdl-ext-hierarchy-clear-cache)
        (funcall mode)
        (cond (;; ghdl-outshine
               ;;  - ghdl cannot use temp-buffer since executes a command that requires a filename as an argument
               (and (eq backend 'ghdl)
                    (eq frontend 'outshine))
               (vhdl-ext-test-hierarchy--outshine-fn))
              ;; ghdl-hierarchy
              ;;  - ghdl cannot use temp-buffer since executes a command that requires a filename as an argument
              ((and (eq backend 'ghdl)
                    (eq frontend 'hierarchy))
               (vhdl-ext-test-hierarchy--hierarchy-fn))
              ;; builtin-hierarchy
              ((and (eq backend 'builtin)
                    (eq frontend 'hierarchy))
               (vhdl-ext-hierarchy-parse)
               (vhdl-ext-test-hierarchy--hierarchy-fn))
              ;; builtin-outshine
              ((and (eq backend 'builtin)
                    (eq frontend 'outshine))
               (vhdl-ext-hierarchy-parse)
               (vhdl-ext-test-hierarchy--outshine-fn))
              ;; tree-sitter-hierarchy
              ((and (eq backend 'tree-sitter)
                    (eq frontend 'hierarchy))
               (vhdl-ext-hierarchy-parse)
               (vhdl-ext-test-hierarchy--hierarchy-fn))
              ;; tree-sitter-outshine
              ((and (eq backend 'tree-sitter)
                    (eq frontend 'outshine))
               (vhdl-ext-hierarchy-parse)
               (vhdl-ext-test-hierarchy--outshine-fn))
              ;; Fallback
              (t
               (error "Not a proper backend-frontend combination!")))))))


(defun vhdl-ext-test-hierarchy-gen-expected-files ()
  ;; ghdl-hierarchy simple: "instances.vhd"
  (let* ((root vhdl-ext-test-files-common-dir)
         (file (file-name-concat root "instances.vhd"))
         (workdir "lib/"))
    (test-hdl-gen-expected-files :file-list `(,file)
                                 :dest-dir vhdl-ext-test-ref-dir-hierarchy
                                 :out-file-ext "ghdl.hier.el"
                                 :process-fn 'eval-ff
                                 :fn #'vhdl-ext-test-hierarchy-buffer
                                 :args `(:mode vhdl-mode
                                         :backend ghdl
                                         :frontend hierarchy
                                         :root ,root
                                         :workdir ,workdir
                                         :files ,vhdl-ext-test-hierarchy-ghdl-instances-sources-list)
                                 :clean-fn (lambda () (delete-directory (file-name-concat root workdir) t))))
  ;; ghdl-outshine simple: "instances.vhd"
  (let* ((root vhdl-ext-test-files-common-dir)
         (file (file-name-concat root "instances.vhd"))
         (workdir "lib/"))
    (test-hdl-gen-expected-files :file-list `(,file)
                                 :dest-dir vhdl-ext-test-ref-dir-hierarchy
                                 :out-file-ext "ghdl.outshine.vhd"
                                 :process-fn 'eval-ff
                                 :fn #'vhdl-ext-test-hierarchy-buffer
                                 :args `(:mode vhdl-mode
                                         :backend ghdl
                                         :root ,root
                                         :workdir ,workdir
                                         :frontend outshine
                                         :files ,vhdl-ext-test-hierarchy-ghdl-instances-sources-list)
                                 :clean-fn (lambda () (delete-directory (file-name-concat root workdir) t))))
  ;; ghdl-hierarchy complex: "tb_axi_if_converter.vhd"
  (let* ((root vhdl-ext-test-axi-converter-dir)
         (file (file-name-concat root "tb/tb_axi_if_converter.vhd"))
         (workdir "lib/"))
    (test-hdl-gen-expected-files :file-list `(,file)
                                 :dest-dir vhdl-ext-test-ref-dir-hierarchy
                                 :out-file-ext "ghdl.hier.el"
                                 :process-fn 'eval-ff
                                 :fn #'vhdl-ext-test-hierarchy-buffer
                                 :args `(:mode vhdl-mode
                                         :backend ghdl
                                         :frontend hierarchy
                                         :root ,root
                                         :workdir ,workdir
                                         :worklib "xil_defaultlib"
                                         :files ,vhdl-ext-test-hierarchy-ghdl-axi-converter-sources-list)
                                 :clean-fn (lambda () (delete-directory (file-name-concat root workdir) t))))
  ;; ghdl-outshine complex: "tb_axi_if_converter.vhd"
  (let* ((root vhdl-ext-test-axi-converter-dir)
         (file (file-name-concat root "tb/tb_axi_if_converter.vhd"))
         (workdir "lib/"))
    (test-hdl-gen-expected-files :file-list `(,file)
                                 :dest-dir vhdl-ext-test-ref-dir-hierarchy
                                 :out-file-ext "ghdl.outshine.vhd"
                                 :process-fn 'eval-ff
                                 :fn #'vhdl-ext-test-hierarchy-buffer
                                 :args `(:mode vhdl-mode
                                         :backend ghdl
                                         :frontend outshine
                                         :root ,root
                                         :workdir ,workdir
                                         :worklib "xil_defaultlib"
                                         :files ,vhdl-ext-test-hierarchy-ghdl-axi-converter-sources-list)
                                 :clean-fn (lambda () (delete-directory (file-name-concat root workdir) t))))
  ;; builtin-hierarchy
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-hierarchy-file-list
                               :dest-dir vhdl-ext-test-ref-dir-hierarchy
                               :out-file-ext "builtin.hier.el"
                               :process-fn 'eval-ff
                               :fn #'vhdl-ext-test-hierarchy-buffer
                               :args `(:mode vhdl-mode
                                       :backend builtin
                                       :frontend hierarchy
                                       :files ,vhdl-ext-test-hierarchy-sources-list))
  ;; builtin-outshine
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-hierarchy-file-list
                               :dest-dir vhdl-ext-test-ref-dir-hierarchy
                               :out-file-ext "builtin.outshine.vhd"
                               :process-fn 'eval-ff
                               :fn #'vhdl-ext-test-hierarchy-buffer
                               :args `(:mode vhdl-mode
                                       :backend builtin
                                       :frontend outshine
                                       :files ,vhdl-ext-test-hierarchy-sources-list))
  ;; tree-sitter-hierarchy
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-hierarchy-file-list
                               :dest-dir vhdl-ext-test-ref-dir-hierarchy
                               :out-file-ext "ts.hier.el"
                               :process-fn 'eval-ff
                               :fn #'vhdl-ext-test-hierarchy-buffer
                               :args `(:mode vhdl-ts-mode
                                       :backend tree-sitter
                                       :frontend hierarchy
                                       :files ,vhdl-ext-test-hierarchy-sources-list))
  ;; tree-sitter-outshine
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-hierarchy-file-list
                               :dest-dir vhdl-ext-test-ref-dir-hierarchy
                               :out-file-ext "ts.outshine.vhd"
                               :process-fn 'eval-ff
                               :fn #'vhdl-ext-test-hierarchy-buffer
                               :args `(:mode vhdl-ts-mode
                                       :backend tree-sitter
                                       :frontend outshine
                                       :files ,vhdl-ext-test-hierarchy-sources-list))
  ;; More custom ones (e.g. need to explicit entity to be parsed from a file with multiple entities declared)
  ;; - hierarchy.vhd / builtin-hierarchy
  (test-hdl-gen-expected-files :file-list `(,(file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd"))
                               :dest-dir vhdl-ext-test-ref-dir-hierarchy
                               :out-file-ext "me.builtin.hier.el"
                               :process-fn 'eval-ff
                               :fn #'vhdl-ext-test-hierarchy-buffer
                               :args `(:mode vhdl-mode
                                       :backend builtin
                                       :frontend hierarchy
                                       :files ,vhdl-ext-test-hierarchy-sources-list
                                       :entity "tb_axi_if_converter"))
  ;; - hierarchy.vhd / builtin-outshine
  (test-hdl-gen-expected-files :file-list `(,(file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd"))
                               :dest-dir vhdl-ext-test-ref-dir-hierarchy
                               :out-file-ext "me.builtin.outshine.vhd"
                               :process-fn 'eval-ff
                               :fn #'vhdl-ext-test-hierarchy-buffer
                               :args `(:mode vhdl-mode
                                       :backend builtin
                                       :frontend outshine
                                       :files ,vhdl-ext-test-hierarchy-sources-list
                                       :entity "tb_axi_if_converter"))
  ;; - hierarchy.vhd / tree-sitter-hierarchy
  (test-hdl-gen-expected-files :file-list `(,(file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd"))
                               :dest-dir vhdl-ext-test-ref-dir-hierarchy
                               :out-file-ext "me.ts.hier.el"
                               :process-fn 'eval-ff
                               :fn #'vhdl-ext-test-hierarchy-buffer
                               :args `(:mode vhdl-ts-mode
                                       :backend tree-sitter
                                       :frontend hierarchy
                                       :files ,vhdl-ext-test-hierarchy-sources-list
                                       :entity "tb_axi_if_converter"))
  ;; - hierarchy.vhd / tree-sitter-outshine
  (test-hdl-gen-expected-files :file-list `(,(file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd"))
                               :dest-dir vhdl-ext-test-ref-dir-hierarchy
                               :out-file-ext "me.ts.outshine.vhd"
                               :process-fn 'eval-ff
                               :fn #'vhdl-ext-test-hierarchy-buffer
                               :args `(:mode vhdl-ts-mode
                                       :backend tree-sitter
                                       :frontend outshine
                                       :files ,vhdl-ext-test-hierarchy-sources-list
                                       :entity "tb_axi_if_converter")))


(ert-deftest hierarchy::ghdl-hierarchy::simple ()
  (let* ((root vhdl-ext-test-files-common-dir)
         (file (file-name-concat root "instances.vhd"))
         (workdir "lib/"))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "ghdl.hier.el"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-mode
                                                                 :backend ghdl
                                                                 :root ,root
                                                                 :workdir ,workdir
                                                                 :frontend hierarchy
                                                                 :files ,vhdl-ext-test-hierarchy-ghdl-instances-sources-list))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "ghdl.hier.el"))
                                  ;; Cleanup function (remove lib/ compilation directory)
                                  (lambda () (delete-directory (file-name-concat root workdir) t))))))


(ert-deftest hierarchy::ghdl-outshine::simple ()
  (let* ((root vhdl-ext-test-files-common-dir)
         (file (file-name-concat root "instances.vhd"))
         (workdir "lib/"))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "ghdl.outshine.vhd"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-mode
                                                                 :backend ghdl
                                                                 :root ,root
                                                                 :workdir ,workdir
                                                                 :frontend outshine
                                                                 :files ,vhdl-ext-test-hierarchy-ghdl-instances-sources-list))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "ghdl.outshine.vhd"))
                                  ;; Cleanup function (remove lib/ compilation directory)
                                  (lambda () (delete-directory (file-name-concat root workdir) t))))))


(ert-deftest hierarchy::ghdl-hierarchy::complex ()
  (let* ((root vhdl-ext-test-axi-converter-dir)
         (file (file-name-concat root "tb/tb_axi_if_converter.vhd"))
         (workdir "lib/"))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "ghdl.hier.el"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-mode
                                                                 :backend ghdl
                                                                 :frontend hierarchy
                                                                 :root ,root
                                                                 :workdir ,workdir
                                                                 :worklib "xil_defaultlib"
                                                                 :files ,vhdl-ext-test-hierarchy-ghdl-axi-converter-sources-list))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "ghdl.hier.el"))
                                  ;; Cleanup function (remove lib/ compilation directory)
                                  (lambda () (delete-directory (file-name-concat root workdir) t))))))


(ert-deftest hierarchy::ghdl-outshine::complex ()
  (let* ((root vhdl-ext-test-axi-converter-dir)
         (file (file-name-concat root "tb/tb_axi_if_converter.vhd"))
         (workdir "lib/"))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "ghdl.outshine.vhd"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-mode
                                                                 :backend ghdl
                                                                 :frontend outshine
                                                                 :root ,root
                                                                 :workdir ,workdir
                                                                 :worklib "xil_defaultlib"
                                                                 :files ,vhdl-ext-test-hierarchy-ghdl-axi-converter-sources-list))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "ghdl.outshine.vhd"))
                                  ;; Cleanup function (remove lib/ compilation directory)
                                  (lambda () (delete-directory (file-name-concat root workdir) t))))))


(ert-deftest hierarchy::builtin-hierarchy ()
  (dolist (file vhdl-ext-test-hierarchy-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "builtin.hier.el"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-mode
                                                                 :backend builtin
                                                                 :frontend hierarchy
                                                                 :files ,vhdl-ext-test-hierarchy-sources-list))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "builtin.hier.el"))))))


(ert-deftest hierarchy::builtin-outshine ()
  (dolist (file vhdl-ext-test-hierarchy-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "builtin.outshine.vhd"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-mode
                                                                 :backend builtin
                                                                 :frontend outshine
                                                                 :files ,vhdl-ext-test-hierarchy-sources-list))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "builtin.outshine.vhd"))))))


(ert-deftest hierarchy::tree-sitter-hierarchy ()
  (dolist (file vhdl-ext-test-hierarchy-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "ts.hier.el"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-ts-mode
                                                                 :backend tree-sitter
                                                                 :frontend hierarchy
                                                                 :files ,vhdl-ext-test-hierarchy-sources-list))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "ts.hier.el"))))))


(ert-deftest hierarchy::tree-sitter-outshine ()
  (dolist (file vhdl-ext-test-hierarchy-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "ts.outshine.vhd"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-ts-mode
                                                                 :backend tree-sitter
                                                                 :frontend outshine
                                                                 :files ,vhdl-ext-test-hierarchy-sources-list))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "ts.outshine.vhd"))))))


(ert-deftest hierarchy::builtin-hierarchy::multiple-entities ()
  (let ((file (file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd")))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "me.builtin.hier.el"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-mode
                                                                 :backend builtin
                                                                 :frontend hierarchy
                                                                 :files ,vhdl-ext-test-hierarchy-sources-list
                                                                 :entity "tb_axi_if_converter"))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "me.builtin.hier.el"))))))


(ert-deftest hierarchy::builtin-outshine::multiple-entities ()
  (let ((file (file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd")))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "me.builtin.outshine.vhd"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-mode
                                                                 :backend builtin
                                                                 :frontend outshine
                                                                 :files ,vhdl-ext-test-hierarchy-sources-list
                                                                 :entity "tb_axi_if_converter"))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "me.builtin.outshine.vhd"))))))


(ert-deftest hierarchy::tree-sitter-hierarchy::multiple-entities ()
  (let ((file (file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd")))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "me.ts.hier.el"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-ts-mode
                                                                 :backend tree-sitter
                                                                 :frontend hierarchy
                                                                 :files ,vhdl-ext-test-hierarchy-sources-list
                                                                 :entity "tb_axi_if_converter"))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "me.ts.hier.el"))))))


(ert-deftest hierarchy::tree-sitter-outshine::multiple-entities ()
  (let ((file (file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd")))
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-hierarchy (test-hdl-basename file "me.ts.outshine.vhd"))
                                                         :process-fn 'eval-ff
                                                         :fn #'vhdl-ext-test-hierarchy-buffer
                                                         :args `(:mode vhdl-ts-mode
                                                                 :backend tree-sitter
                                                                 :frontend outshine
                                                                 :files ,vhdl-ext-test-hierarchy-sources-list
                                                                 :entity "tb_axi_if_converter"))
                                  (file-name-concat vhdl-ext-test-ref-dir-hierarchy (test-hdl-basename file "me.ts.outshine.vhd"))))))


(provide 'vhdl-ext-test-hierarchy)

;;; vhdl-ext-test-hierarchy.el ends here
