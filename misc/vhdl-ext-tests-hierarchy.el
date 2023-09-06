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


(require 'vhdl-ext-hierarchy)

(defun vhdl-ext-test-hierarchy (file)
  (let* ((test-file (file-name-concat vhdl-ext-tests-files-dir "common" file))
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
                                 "common/instances.vhd"
                                 "common/hierarchy.vhd"
                                 "common/tb_axi_if_converter.vhd")
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
               (vhdl-ext-hierarchy-parse)
               (vhdl-ext-hierarchy-current-buffer))
              ;; tree-sitter-hierarchy
              ((and (eq vhdl-ext-hierarchy-backend 'tree-sitter)
                    (eq vhdl-ext-hierarchy-frontend 'hierarchy))
               (vhdl-ext-hierarchy-parse)
               (vhdl-ext-hierarchy-current-buffer))
              ;; GHDL-hierarchy
              ((and (eq vhdl-ext-hierarchy-backend 'ghdl)
                    (eq vhdl-ext-hierarchy-frontend 'hierarchy))
               (make-directory (file-name-concat (file-name-as-directory vhdl-ext-tests-files-dir) lib-directory) :parents)
               (vhdl-ext-hierarchy-current-buffer))
              ;; builtin-outshine
              ((and (eq vhdl-ext-hierarchy-backend 'builtin)
                    (eq vhdl-ext-hierarchy-frontend 'outshine))
               (vhdl-ext-hierarchy-parse)
               (vhdl-ext-hierarchy-current-buffer)
               (buffer-substring-no-properties (point-min) (point-max)))
              ;; tree-sitter-outshine
              ((and (eq vhdl-ext-hierarchy-backend 'tree-sitter)
                    (eq vhdl-ext-hierarchy-frontend 'outshine))
               (vhdl-ext-hierarchy-parse)
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


(ert-deftest hierarchy::ghdl-hierarchy ()
  (let ((vhdl-ext-hierarchy-backend  'ghdl)
        (vhdl-ext-hierarchy-frontend 'hierarchy))
    (should (string= (with-temp-buffer
                       (hierarchy-print (vhdl-ext-test-hierarchy "instances.vhd") (lambda (node) node))
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
    (should (string= (vhdl-ext-test-hierarchy "instances.vhd")
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
"))
    ;; Second test on tb_axi_if_converter.vhd
    (should (equal (vhdl-ext-test-hierarchy "tb_axi_if_converter.vhd")
"-- Hierarchy generated by `vhdl-ext'

-- * tb_axi_if_converter
-- ** axi_if_converter
-- *** axi_lite_regs
-- *** input_buffer
-- **** blk_mem_gen_0
-- **** blk_mem_gen_0
-- *** core_converter
-- *** core_converter
-- *** axi_lite_master
-- *** clk_div
-- **** BUFG
-- *** clk_sync
-- *** pattern_counter
-- *** pattern_counter
-- *** core_fsm
-- *** core_fsm
-- ** s_axi_model
-- ** s_axi_model


-- * Buffer local variables
-- Local Variables:
-- eval: (vhdl-mode 1)
-- eval: (vhdl-ext-hierarchy-outshine-nav-mode 1)
-- End:
"))))


(ert-deftest hierarchy::builtin-hierarchy ()
  (let ((vhdl-ext-hierarchy-backend  'builtin)
        (vhdl-ext-hierarchy-frontend 'hierarchy))
    (should (string= (with-temp-buffer
                       (hierarchy-print (vhdl-ext-test-hierarchy "instances.vhd") (lambda (node) node))
                       (buffer-substring-no-properties (point-min) (point-max)))
"instances
  instances.block0:I_BLOCK0_0
  instances.block0:I_BLOCK0_1
  instances.block0:I_BLOCK0_2
  instances.block1:I_BLOCK1_0
  instances.block1:I_BLOCK1_1
  instances.block1:I_BLOCK1_2
  instances.block1:I_BLOCK1_GEN
"))
    ;; Second test on tb_axi_if_converter.vhd
    (should (string= (with-temp-buffer
                       (hierarchy-print (vhdl-ext-test-hierarchy "tb_axi_if_converter.vhd") (lambda (node) node))
                       (buffer-substring-no-properties (point-min) (point-max)))
"tb_axi_if_converter
  tb_axi_if_converter.axi_if_converter:DUT
    tb_axi_if_converter.axi_if_converter:DUT.axi_lite_regs:I_AXI_LITE_REGS
    tb_axi_if_converter.axi_if_converter:DUT.input_buffer:I_INPUT_BUFFER
      tb_axi_if_converter.axi_if_converter:DUT.input_buffer:I_INPUT_BUFFER.blk_mem_gen_0:input_buffer_l
      tb_axi_if_converter.axi_if_converter:DUT.input_buffer:I_INPUT_BUFFER.blk_mem_gen_0:input_buffer_r
    tb_axi_if_converter.axi_if_converter:DUT.core_converter:I_CORE_CONVERTER_L
    tb_axi_if_converter.axi_if_converter:DUT.core_converter:I_CORE_CONVERTER_R
    tb_axi_if_converter.axi_if_converter:DUT.axi_lite_master:I_AXI_LITE_MASTER
    tb_axi_if_converter.axi_if_converter:DUT.clk_div:I_CLK_DIV
      tb_axi_if_converter.axi_if_converter:DUT.clk_div:I_CLK_DIV.BUFG:BUFG_inst
    tb_axi_if_converter.axi_if_converter:DUT.clk_sync:I_CLK_FS_SYNC
    tb_axi_if_converter.axi_if_converter:DUT.pattern_counter:I_PATTERN_COUNTER_L
    tb_axi_if_converter.axi_if_converter:DUT.pattern_counter:I_PATTERN_COUNTER_R
    tb_axi_if_converter.axi_if_converter:DUT.core_fsm:I_CORE_FSM_L
    tb_axi_if_converter.axi_if_converter:DUT.core_fsm:I_CORE_FSM_R
  tb_axi_if_converter.s_axi_model:I_SLAVEMODEL_L
  tb_axi_if_converter.s_axi_model:I_SLAVEMODEL_R
"))))


(ert-deftest hierarchy::ghdl-outshine ()
  (let ((vhdl-ext-hierarchy-backend  'ghdl)
        (vhdl-ext-hierarchy-frontend 'outshine))
    (should (string= (vhdl-ext-test-hierarchy "instances.vhd")
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
