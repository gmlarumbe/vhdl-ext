;;; vhdl-ext-tests-imenu.el --- vhdl-ext ERT Imenu tests  -*- lexical-binding: t -*-

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
;; ERT Imenu Tests
;;
;;; Code:


(defmacro vhdl-ext-test-imenu-file (file)
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (file-name-concat vhdl-ext-tests-common-dir ,file))
     (cl-letf (((symbol-function 'message)
                (lambda (FORMAT-STRING &rest ARGS)
                  nil))) ; Mock `message' to silence VHDL version reporting
       (vhdl-mode))
     (let ((imenu-use-markers nil)
           (print-level nil)
           (print-length nil)
           (eval-expression-print-level nil)
           (eval-expression-print-length nil))
       (imenu-default-create-index-function))))


(ert-deftest imenu::procedures ()
  (should (equal (vhdl-ext-test-imenu-file "global_sim.vhd")
                 '(("Package"
                    ("global_sim" . 205)
                    ("global_sim" . 5063))
                   ("Procedure"
                    ("read_control_reg" . 1854)
                    ("read_status_reg" . 1991)
                    ("read_version_reg" . 2128)
                    ("read_counters" . 2265)
                    ("read_master_lite_rd_data_reg" . 2402)
                    ("write_control_reg" . 2553)
                    ("write_converter_setup_reg" . 2742)
                    ("write_mm2s_size_reg" . 2931)
                    ("write_master_lite_wr_setup_reg" . 3120)
                    ("write_master_lite_wr_add_reg" . 3309)
                    ("write_master_lite_wr_data_reg" . 3498)
                    ("write_master_lite_rd_setup_reg" . 3687)
                    ("write_master_lite_rd_add_reg" . 3876)
                    ("write_control_reg_system_enable" . 4091)
                    ("write_control_reg_system_stop" . 4232)
                    ("write_control_reg_soft_reset" . 4373)
                    ("write_master_lite_write_request" . 4514)
                    ("write_master_lite_read_request" . 4755)
                    ("end_test_and_stop_clock" . 4961)
                    ("end_test_and_stop_clock" . 5105)
                    ("read_control_reg" . 5350)
                    ("read_status_reg" . 5621)
                    ("read_version_reg" . 5889)
                    ("read_counters" . 6160)
                    ("read_master_lite_rd_data_reg" . 6638)
                    ("write_control_reg" . 6968)
                    ("write_converter_setup_reg" . 7338)
                    ("write_mm2s_size_reg" . 7731)
                    ("write_master_lite_wr_setup_reg" . 8106)
                    ("write_master_lite_wr_add_reg" . 8514)
                    ("write_master_lite_wr_data_reg" . 8917)
                    ("write_master_lite_rd_setup_reg" . 9322)
                    ("write_master_lite_rd_add_reg" . 9730)
                    ("write_control_reg_system_enable" . 10166)
                    ("write_control_reg_system_stop" . 10494)
                    ("write_control_reg_soft_reset" . 10818)
                    ("write_master_lite_write_request" . 11140)
                    ("write_master_lite_read_request" . 11726))))))

(ert-deftest imenu::instances ()
  (should (equal (vhdl-ext-test-imenu-file "axi_if_converter.vhd")
                 '(("Entity"
                    ("axi_if_converter" . 169))
                   ("Architecture"
                    ("RTL of axi_if_converter" . 9815))
                   ("Instance"
                    ("axi_lite_regs" . 14265)
                    ("input_buffer" . 16772)
                    ("core_converter" . 19994)
                    ("core_converter" . 24147)
                    ("axi_lite_master" . 28301)
                    ("clk_div" . 30201)
                    ("clk_sync" . 30499)
                    ("pattern_counter" . 30767)
                    ("pattern_counter" . 31348)
                    ("core_fsm" . 31929)
                    ("core_fsm" . 32753))))))

(ert-deftest imenu::generic ()
  (should (equal (vhdl-ext-test-imenu-file "tb_axi_if_converter.vhd")
                 '(("Entity"
                    ("tb_axi_if_converter" . 402))
                   ("Architecture"
                    ("TB of tb_axi_if_converter" . 587))
                   ("Process"
                    ("main" . 25066))
                   ("Procedure"
                    ("init_values" . 25085)
                    ("rx_data" . 25285))
                   ("Instance"
                    ("axi_if_converter" . 10493)
                    ("s_axi_model" . 18789)
                    ("s_axi_model" . 21092))))))



(provide 'vhdl-ext-tests-imenu)

;;; vhdl-ext-tests-imenu.el ends here
