;;; vhdl-ext-tests-navigation.el --- vhdl-ext ERT Imenu tests  -*- lexical-binding: t -*-

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
;; ERT Navigation tests
;;
;;; Code:


;;;; Aux macros/defuns
(defmacro vhdl-ext-test-navigation-file (file &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((print-level nil)
           (print-length nil)
           (eval-expression-print-level nil)
           (eval-expression-print-length nil)
           (default-directory (file-name-as-directory vhdl-ext-tests-common-dir)))
       (insert-file-contents (file-name-concat vhdl-ext-tests-common-dir ,file))
       (cl-letf (((symbol-function 'message)
                  (lambda (FORMAT-STRING &rest ARGS)
                    nil))) ; Mock `message' to silence VHDL version reporting
         (vhdl-mode))
       ,@body)))

(defmacro vhdl-ext-test-jump-parent-file (file &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((print-level nil)
           (print-length nil)
           (eval-expression-print-level nil)
           (eval-expression-print-length nil)
           (default-directory (file-name-as-directory vhdl-ext-tests-jump-parent-dir)))
       (insert-file-contents (file-name-concat vhdl-ext-tests-jump-parent-dir ,file))
       (cl-letf (((symbol-function 'message)
                  (lambda (FORMAT-STRING &rest ARGS)
                    nil))) ; Mock `message' to silence VHDL version reporting
         (vhdl-mode))
       ,@body)))

(defun vhdl-ext-test-navigation-instances-fwd ()
  (let (var)
    (save-excursion
      (goto-char (point-min))
      (while (vhdl-ext-find-entity-instance-fwd)
        (push (point) var)))
    (reverse var)))

(defun vhdl-ext-test-navigation-instances-bwd ()
  (let (var)
    (save-excursion
      (goto-char (point-max))
      (while (vhdl-ext-find-entity-instance-bwd)
        (push (point) var)))
    (reverse var)))


;;;; Tests
(ert-deftest navigation::instances-fwd ()
  (vhdl-ext-test-navigation-file "axi_if_converter.vhd"
    (should (equal (vhdl-ext-test-navigation-instances-fwd)
                   '(14342 16847 20075 24228 28382 30266 30566 30850 31431 31995 32819))))
  (vhdl-ext-test-navigation-file "tb_axi_if_converter.vhd"
    (should (equal (vhdl-ext-test-navigation-instances-fwd)
                   '(10558 18860 21163)))))

(ert-deftest navigation::instances-bwd ()
  (vhdl-ext-test-navigation-file "axi_if_converter.vhd"
    (should (equal (vhdl-ext-test-navigation-instances-bwd)
                   '(32753 31929 31348 30767 30499 30201 28301 24147 19994 16772 14265))))
  (vhdl-ext-test-navigation-file "tb_axi_if_converter.vhd"
    (should (equal (vhdl-ext-test-navigation-instances-bwd)
                   '(21092 18789 10493)))))

(ert-deftest navigation::jump-to-parent-module-ag ()
  (cl-letf (((symbol-function 'compilation-start)
             (lambda (command &optional mode name-function highlight-regexp)
               (butlast (split-string (shell-command-to-string command) "\n") 4))))
    (let ((vhdl-ext-jump-to-parent-module-engine "ag")
          (ag-arguments '("--smart-case" "--stats" "--nogroup")))
      ;; block0
      (vhdl-ext-test-jump-parent-file "block0.vhd"
        (should (equal (vhdl-ext-jump-to-parent-entity)
                       '("\33[1;32mtest/files/common/instances.vhd\33[0m\33[K:\33[1;33m47\33[0m\33[K:1:\33[30;43m    I_BLOCK0_0 : block0\33[0m\33[K"
                         "\33[1;32mtest/files/common/instances.vhd\33[0m\33[K:\33[1;33m54\33[0m\33[K:1:\33[30;43m    I_BLOCK0_1 : block0\33[0m\33[K"
                         "\33[1;32mtest/files/common/instances.vhd\33[0m\33[K:\33[1;33m60\33[0m\33[K:1:\33[30;43m    I_BLOCK0_2 : block0\33[0m\33[K generic map ("
                         "3 matches" "1 files contained matches"))))
      ;; block1
      (vhdl-ext-test-jump-parent-file "block1.vhd"
        (should (equal (vhdl-ext-jump-to-parent-entity)
                       '("\33[1;32mtest/files/common/instances.vhd\33[0m\33[K:\33[1;33m70\33[0m\33[K:1:\33[30;43m    I_BLOCK1_0 : entity work.block1\33[0m\33[K"
                         "\33[1;32mtest/files/common/instances.vhd\33[0m\33[K:\33[1;33m77\33[0m\33[K:1:\33[30;43m    I_BLOCK1_1 : entity work.block1\33[0m\33[K"
                         "\33[1;32mtest/files/common/instances.vhd\33[0m\33[K:\33[1;33m84\33[0m\33[K:1:\33[30;43m    I_BLOCK1_2 : entity work.block1\33[0m\33[K generic map ("
                         "\33[1;32mtest/files/common/instances.vhd\33[0m\33[K:\33[1;33m96\33[0m\33[K:1:\33[30;43m        I_BLOCK1_GEN : entity work.block1\33[0m\33[K port map"
                         "4 matches" "1 files contained matches")))))))


(defvar vhdl-ext-test-navigation-instance-at-point
  '(("axi_if_converter.vhd" ((168 . nil)
                             (196 . nil)
                             (9493 . nil)
                             (14266 . nil)
                             (14268 . nil)
                             (14269 . "axi_lite_regs")
                             (15657 . "axi_lite_regs")
                             (16769 . nil)
                             (16775 . nil)
                             (16776 . "input_buffer")
                             (19059 . "input_buffer")
                             (19989 . "input_buffer")
                             (19990 . nil)
                             (20243 . "core_converter")
                             (25844 . "core_converter")
                             (28958 . "axi_lite_master")
                             (30411 . "clk_div")
                             (30601 . "clk_sync")
                             (30941 . "pattern_counter")
                             (31912 . "pattern_counter")
                             (32596 . "core_fsm")
                             (32596 . "core_fsm")
                             (33595 . "core_fsm")
                             (33596 . "core_fsm")
                             (33597 . nil)
                             (33600 . nil)
                             (33623 . nil)))
    ("instances.vhd" ((1024 . nil)
                      (1436 . nil)
                      (1496 . nil)
                      (1499 . nil)
                      (1500 . "block0")
                      (1509 . "block0")
                      (1567 . "block0")
                      (1634 . "block0")
                      (1635 . "block0")
                      (1636 . nil)
                      (1637 . nil)
                      (1681 . "block0")
                      (1737 . "block0")
                      (1766 . nil)
                      (1828 . "block0")
                      (1950 . "block0")
                      (1979 . nil)
                      (2119 . "block1")
                      (2161 . nil)
                      (2119 . "block1")
                      (2273 . "block1")
                      (2315 . nil)
                      (2448 . "block1")
                      (2538 . "block1")
                      (2539 . nil)
                      (2540 . nil)
                      (2610 . nil)
                      (2611 . "block1")
                      (2653 . "block1")
                      (2687 . "block1")
                      (2688 . nil)
                      (2706 . nil)
                      (2729 . nil)))))

(ert-deftest navigation::instance-at-point ()
  (let ((alist vhdl-ext-test-navigation-instance-at-point)
        file data block)
    (dolist (elm alist)
      (setq file (car elm))
      (setq data (cadr elm))
      (vhdl-ext-test-navigation-file file
        (dolist (pos-type data)
          (goto-char (car pos-type))
          (if (cdr pos-type)
              (setq block (cdr pos-type))
            (setq block nil))
          (should (string= (car (vhdl-ext-instance-at-point)) block)))))))



(provide 'vhdl-ext-tests-navigation)

;;; vhdl-ext-tests-navigation.el ends here
