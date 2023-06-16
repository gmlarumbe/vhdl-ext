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


(require 'vhdl-ext-utils)

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

(defun vhdl-ext-test-utils-sexp (file alist function)
  (vhdl-ext-test-utils-file file
    (let ((expected-alist alist)
          result-alist)
      (dolist (positions expected-alist)
        (goto-char (car positions))
        (funcall function)
        (push (cons (car positions) (point)) result-alist))
      (nreverse result-alist))))

(defconst vhdl-ext-test-forward-sexp-points
  '((949 . 1057) ; entity with end entity
    (951 . 1057)
    (955 . 1057)
    (1064 . 1166) ; entity w/o end entity
    (1067 . 1166)
    (1070 . 1166)
    (1244 . 2381) ; architecture with end architecture
    (1256 . 2381)
    (1312 . 1472) ; component
    (1321 . 1472)
    (1479 . 2031) ; function
    (1486 . 2031)
    (1668 . 1989) ; loop
    (1670 . 1989)
    (1672 . 1989)
    (1705 . 1750) ; then
    (1707 . 1750)
    (1709 . 1750)
    (1750 . 1971) ; else
    (1752 . 1971)
    (1754 . 1971)
    (2059 . 2238) ; process
    (2066 . 2238)
    (2076 . 2198) ; procedure with end procedure
    (2085 . 2198)
    (2287 . 2350) ; generate
    (2295 . 2350)
    (2390 . 3385) ; architecture w/o end architecture
    (2458 . 3077) ; function w/o end function
    (2647 . 3044) ; external nested loop
    (2808 . 2881) ; internal nested loop
    (3115 . 3227) ; procedure w/o end procedure
    (3124 . 3227)
    (3406 . 3527) ; Package with endpackage
    (3426 . 3514) ; procedure declaration without body
    (3534 . 3647) ; package w/o end package
    (3651 . 3858) ; package body
    (3658 . 3858)
    (3676 . 3835) ; procedure with endprocedure inside package body
    (3685 . 3835)
    (3866 . 4051) ; package body without end package body
    (3891 . 4040) ; procedure w/o endprocedure inside package body
    (4072 . 4119) ; configuration with end configuration
    (4126 . 4159) ; configuration w/o end configuration
    (4175 . 4202) ; context with end context
    (4209 . 4228)) ; context w/o context
  "Alist with initial point and expected point after `vhdl-ext-forward-sexp'.")

(defconst vhdl-ext-test-backward-sexp-points
  '((1057 . 949) ; entity with end entity
    (1051 . 949)
    (1047 . 949)
    (1166 . 1064) ; entity w/o end entity
    (1163 . 1064)
    (2381 . 1244) ; architecture with end architecture
    (2368 . 1244)
    (1472 . 1312) ; component
    (1462 . 1312)
    (2031 . 1479) ; function
    (2022 . 1479)
    (1989 . 1668) ; loop
    (1984 . 1668)
    (1981 . 1668)
    (1971 . 1685) ; end if
    (1951 . 1771)
    (1965 . 1750) ; end
    (1945 . 1844)
    (1754 . 1705) ; else
    (1750 . 1705)
    (2238 . 2059) ; process
    (2230 . 2059)
    (2198 . 2076) ; procedure with end procedure
    (2188 . 2076)
    (2350 . 2287) ; generate
    (2341 . 2287)
    (3385 . 2390) ; architecture w/o end architecture
    (3077 . 2458) ; function w/o end function
    (3044 . 2647) ; external nested loop
    (3039 . 2647)
    (2881 . 2808) ; internal nested loop
    (2876 . 2808)
    (3227 . 3115) ; procedure w/o end procedure
    (3224 . 3115)
    (3527 . 3406) ; Package with endpackage
    (3514 . 3440) ; procedure declaration without body
    (3647 . 3534) ; package w/o end package
    (3858 . 3651) ; package body
    (3853 . 3651)
    (4040 . 3891) ; procedure with endprocedure inside package body
    (4037 . 3891)
    (4051 . 3866) ; package body without end package body
    (4040 . 3891) ; procedure w/o endprocedure inside package body
    (4119 . 4072) ; configuration with end configuration
    (4159 . 4126) ; configuration w/o end configuration
    (4202 . 4175) ; context with end context
    (4228 . 4209)) ; context w/o context
  "Alist with initial point and expected point after `vhdl-ext-backward-sexp'.")

(ert-deftest utils::forward-sexp ()
  (should (equal (vhdl-ext-test-utils-sexp "sexp.vhd" vhdl-ext-test-forward-sexp-points #'vhdl-ext-forward-sexp)
                 vhdl-ext-test-forward-sexp-points)))

(ert-deftest utils::backward-sexp ()
  (should (equal (vhdl-ext-test-utils-sexp "sexp.vhd" vhdl-ext-test-backward-sexp-points #'vhdl-ext-backward-sexp)
                 vhdl-ext-test-backward-sexp-points)))

(defconst vhdl-ext-test-utils-block-at-point-alist
  '((520 . nil)
    (892 . nil)
    (936 . nil)
    (962 . ((type . "entity") (name . "foo")))
    (1035 . ((type . "entity") (name . "foo")))
    (1047 . ((type . "entity") (name . "foo")))
    (1048 . ((type . "entity") (name . "foo")))
    (1057 . ((type . "entity") (name . "foo")))
    (1057 . ((type . "entity") (name . "foo")))
    (1058 . nil)
    (1079 . ((type . "entity") (name . "foo2")))
    (1162 . ((type . "entity") (name . "foo2")))
    (1174 . nil)
    (1271 . ((type . "architecture") (name . "RTL")))
    (1336 . ((type . "component") (name . "blk_mem_gen_0")))
    (1440 . ((type . "component") (name . "blk_mem_gen_0")))
    (1474 . ((type . "architecture") (name . "RTL")))
    (1535 . ((type . "function") (name . "clogb2")))
    (1582 . ((type . "function") (name . "clogb2")))
    (1631 . ((type . "function") (name . "clogb2")))
    (1889 . ((type . "function") (name . "clogb2")))
    (2015 . ((type . "function") (name . "clogb2")))
    (2023 . ((type . "function") (name . "clogb2")))
    (2031 . ((type . "architecture") (name . "RTL")))
    (2040 . ((type . "architecture") (name . "RTL")))
    (2047 . ((type . "architecture") (name . "RTL")))
    (2067 . ((type . "process") (name . "main")))
    (2115 . ((type . "procedure") (name . "init_values")))
    (2146 . ((type . "procedure") (name . "init_values")))
    (2212 . ((type . "process") (name . "main")))
    (2245 . ((type . "architecture") (name . "RTL")))
    (2296 . ((type . "generate") (name . "gen_mem_sel")))
    (2306 . ((type . "generate") (name . "gen_mem_sel")))
    (2334 . ((type . "generate") (name . "gen_mem_sel")))
    (2364 . ((type . "architecture") (name . "RTL")))
    (2365 . ((type . "architecture") (name . "RTL")))
    (2387 . nil)
    (2389 . nil)
    (2417 . ((type . "architecture") (name . "RTL")))
    (2453 . ((type . "architecture") (name . "RTL")))
    (2514 . ((type . "function") (name . "clogb2")))
    (2561 . ((type . "function") (name . "clogb2")))
    (2610 . ((type . "function") (name . "clogb2")))
    (2717 . ((type . "function") (name . "clogb2")))
    (2771 . ((type . "function") (name . "clogb2")))
    (2853 . ((type . "function") (name . "clogb2")))
    (3028 . ((type . "function") (name . "clogb2")))
    (3079 . ((type . "architecture") (name . "RTL")))
    (3086 . ((type . "architecture") (name . "RTL")))
    (3106 . ((type . "process") (name . "main")))
    (3140 . ((type . "procedure") (name . "init_values")))
    (3154 . ((type . "procedure") (name . "init_values")))
    (3216 . ((type . "procedure") (name . "init_values")))
    (3241 . ((type . "process") (name . "main")))
    (3252 . ((type . "process") (name . "main")))
    (3275 . ((type . "architecture") (name . "RTL")))
    (3335 . ((type . "generate") (name . "gen_mem_sel")))
    (3363 . ((type . "generate") (name . "gen_mem_sel")))
    (3381 . ((type . "architecture") (name . "RTL")))
    (3391 . nil)
    (3393 . nil)
    (3421 . ((type . "package") (name . "foo")))
    (3442 . ((type . "procedure") (name . "foo2")))
    (3504 . ((type . "procedure") (name . "foo2")))
    (3515 . ((type . "package") (name . "foo")))
    (3516 . ((type . "package") (name . "foo")))
    (3533 . nil)
    (3549 . ((type . "package") (name . "foo")))
    (3570 . ((type . "procedure") (name . "foo2")))
    (3643 . ((type . "package") (name . "foo")))
    (3649 . nil)
    (3651 . nil)
    (3671 . ((type . "package") (name . "foo")))
    (3692 . ((type . "procedure") (name . "foo")))
    (3777 . ((type . "procedure") (name . "foo")))
    (3841 . ((type . "package") (name . "foo")))
    (3864 . nil)
    (3865 . nil)
    (3886 . ((type . "package") (name . "foo")))
    (3907 . ((type . "procedure") (name . "foo")))
    (3992 . ((type . "procedure") (name . "foo")))
    (4047 . ((type . "package") (name . "foo")))
    (4053 . nil)
    (4055 . nil)
    (4101 . ((type . "configuration") (name . "foo")))
    (4125 . nil)
    (4155 . ((type . "configuration") (name . "foo")))
    (4161 . nil)
    (4163 . nil)
    (4175 . nil)
    (4190 . ((type . "context") (name . "foo")))
    (4208 . nil)
    (4224 . ((type . "context") (name . "foo")))
    (4230 . nil)))

(ert-deftest utils::block-at-point ()
  (let ((alist vhdl-ext-test-utils-block-at-point-alist)
        (file "sexp.vhd")
        pos expected-alist)
    (vhdl-ext-test-utils-file file
      (dolist (elm alist)
        (setq pos (car elm))
        (goto-char pos)
        (push (cons pos (vhdl-ext-block-at-point)) expected-alist)))
    (setq expected-alist (nreverse expected-alist))
    (should (equal alist expected-alist))))

(provide 'vhdl-ext-tests-utils)

;;; vhdl-ext-tests-utils.el ends here
