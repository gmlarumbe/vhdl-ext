;;; vhdl-ext-test-utils.el --- vhdl-ext ERT utils tests  -*- lexical-binding: t -*-

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
;; vhdl-ext ERT utils tests
;;
;;; Code:

(defconst vhdl-ext-test-utils-file-list vhdl-ext-test-common-file-list)
(defconst vhdl-ext-test-ref-dir-utils (file-name-concat vhdl-ext-test-ref-dir "utils"))
(defconst vhdl-ext-test-dump-dir-utils (file-name-concat vhdl-ext-test-dump-dir "utils"))

(defconst vhdl-ext-test-dummy-file-list `(,(file-name-concat vhdl-ext-test-files-common-dir "instances.vhd")))
(defconst vhdl-ext-test-utils-proj-name "vhdl-ext-test-utils")


(defconst vhdl-ext-test-utils-instance-at-point-file-and-pos
  `((,(file-name-concat vhdl-ext-test-files-common-dir "axi_if_converter.vhd") 168 196 9493 14266 14268 14269 15657 16769 16775 16776 19059 19989 19990 20243 25844 28958 30411 30601 30941 31912 32596 32596 33595 33596 33597 33600 33623)
    (,(file-name-concat vhdl-ext-test-files-common-dir "instances.vhd") 1025 1437 1497 1500 1501 1510 1568 1635 1636 1637 1638 1682 1738 1767 1829 1951 1980 2120 2162 2120 2274 2316 2449 2539 2540 2541 2596 2681 2756 2757 2759 2780 2822 2830 2842 2873 2907 2935 2936 2937 2939 2961)))

(defconst vhdl-ext-test-utils-block-at-point-file-and-pos
  `((,(file-name-concat vhdl-ext-test-files-common-dir "sexp.vhd") 520 892 936 962 1035 1047 1048 1057 1057 1058 1079 1162 1174 1271 1336 1440 1474 1535 1582 1631 1889 2015 2023 2031 2040 2047 2067 2115 2146 2212 2245 2296 2306 2334 2364 2365 2387 2389 2417 2453 2514 2561 2610 2717 2771 2853 3028 3079 3086 3106 3140 3154 3216 3241 3252 3275 3335 3363 3381 3391 3393 3421 3442 3504 3515 3516 3533 3549 3570 3643 3649 3651 3671 3692 3777 3841 3864 3865 3886 3907 3992 4047 4053 4055 4101 4125 4155 4161 4163 4175 4190 4208 4224 4230)))

(defconst vhdl-ext-test-utils-forward-sexp-file-and-pos
  `((,(file-name-concat vhdl-ext-test-files-common-dir "sexp.vhd") 949 951 955 1064 1067 1070 1244 1256 1312 1321 1479 1486 1668 1670 1672 1705 1707 1709 1750 1752 1754 2059 2066 2076 2085 2287 2295 2390 2458 2647 2808 3115 3124 3406 3426 3534 3651 3658 3676 3685 3866 3891 4072 4126 4175 4209)))

(defconst vhdl-ext-test-utils-backward-sexp-file-and-pos
  `((,(file-name-concat vhdl-ext-test-files-common-dir "sexp.vhd") 1057 1051 1047 1166 1163 2381 2368 1472 1462 2031 2022 1989 1984 1981 1971 1951 1965 1945 1754 1750 2238 2230 2198 2188 2350 2341 3385 3077 3044 3039 2881 2876 3227 3224 3527 3514 3647 3858 3853 4040 4037 4051 4040 4119 4159 4202 4228)))

(defconst vhdl-ext-test-utils-point-inside-block-file-pos-and-match
  `((,(file-name-concat vhdl-ext-test-files-common-dir "axi_if_converter.vhd") ((196 . entity)
                                                                                (9813 . entity)
                                                                                (9914 . architecture)
                                                                                (31104 . entity)
                                                                                (31104 . architecture)
                                                                                (33623 . architecture)))
    (,(file-name-concat vhdl-ext-test-files-common-dir "global_pkg.vhd") ((70 . package)
                                                                          (89 . package)
                                                                          (1337 . package)
                                                                          (1411 . entity)
                                                                          (1950 . package)))
    ;; TODO: There is something wrong with procedures inside package body global_sim with `vhdl-ext'
    (,(file-name-concat vhdl-ext-test-files-common-dir "global_sim.vhd") ((94 . package)
                                                                          (292 . package)
                                                                          (1854 . procedure)
                                                                          (1892 . procedure)
                                                                          (1990 . procedure)
                                                                          (5035 . package)
                                                                          (5061 . package)
                                                                          (5090 . package)
                                                                          (5191 . procedure)
                                                                          (5251 . procedure)
                                                                          (5326 . procedure)
                                                                          (5326 . package)
                                                                          (6433 . procedure)
                                                                          (6433 . package)
                                                                          (6433 . architecture)
                                                                          (12186 . procedure)
                                                                          (12186 . package)
                                                                          (12186 . architecture)
                                                                          (12186 . entity)))
    (,(file-name-concat vhdl-ext-test-files-common-dir "hierarchy.vhd") ((1013 . entity)
                                                                         (1141 . entity)
                                                                         (1141 . architecture)
                                                                         (10722 . entity)
                                                                         (10722 . architecture)
                                                                         (10763 . architecture)
                                                                         (34518 . architecture)
                                                                         (34932 . entity)
                                                                         (35490 . architecture)
                                                                         (36547 . process)
                                                                         (36621 . process)
                                                                         (36972 . process)
                                                                         (40431 . component)
                                                                         (40431 . architecture)
                                                                         (40431 . entity)
                                                                         (41112 . procedure)
                                                                         (41112 . architecture)
                                                                         (41492 . procedure)
                                                                         (41670 . procedure)
                                                                         (130102 . architecture)
                                                                         (130102 . entity)
                                                                         (131680 . entity)))
    (,(file-name-concat vhdl-ext-test-files-common-dir "instances.vhd") ((1663 . architecture)
                                                                         (2797 . generate)
                                                                         (2813 . generate)
                                                                         (2821 . generate)
                                                                         (2821 . architecture)
                                                                         (2821 . entity)
                                                                         (2873 . generate)
                                                                         (2912 . generate)
                                                                         (2916 . generate)
                                                                         (2916 . architecture)
                                                                         (2924 . generate)))
    ;; TODO: function and procedure at the beginning not detected in `vhdl-ext'
    (,(file-name-concat vhdl-ext-test-files-common-dir "sexp.vhd") ((2514 . function)
                                                                    (3154 . procedure)
                                                                    (3154 . process)
                                                                    (3325 . generate)
                                                                    (3334 . generate)
                                                                    (3335 . generate)
                                                                    (3380 . generate)
                                                                    (3381 . generate)
                                                                    (3570 . procedure)
                                                                    (3570 . package)
                                                                    (3754 . procedure)
                                                                    (3777 . procedure)
                                                                    (3841 . package)
                                                                    (4047 . package)
                                                                    (4101 . configuration)
                                                                    (4155 . configuration)
                                                                    (4190 . context)
                                                                    (4224 . context)))))


(defmacro vhdl-ext-test-with-test-project (project &rest body)
  (declare (indent 1) (debug t))
  ;; Mock `vhdl-ext-buffer-proj' so that function can be run outside of a VHDL
  ;; project buffer and sources are extracted project
  `(cl-letf (((symbol-function 'vhdl-ext-buffer-proj)
              (lambda () ,project)))
     ,@body))


(cl-defun vhdl-ext-test-utils-scan-entities-fn (&key mode)
  (test-hdl-no-messages
    (funcall mode))
  (vhdl-ext-scan-buffer-entities))

(cl-defun vhdl-ext-test-proj-files-fn (&key root dirs ignore-dirs files ignore-files)
  "Show as one file per line instead of as an Elisp string list."
  (let* ((vhdl-ext-project-alist `((,vhdl-ext-test-utils-proj-name
                                    :root ,root
                                    :dirs ,dirs
                                    :ignore-dirs ,ignore-dirs
                                    :files ,files
                                    :ignore-files ,ignore-files)))
         (file-list (vhdl-ext-proj-files)))
    (mapconcat (lambda (file)
                 (file-relative-name file vhdl-ext-test-files-dir))
               file-list
               "\n")))


(defun vhdl-ext-test-utils-gen-expected-files ()
  ;; Forward sexp
  (dolist (file-and-pos vhdl-ext-test-utils-forward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "fwd.sexp.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-mode
                                           :fn vhdl-ext-forward-sexp
                                           :pos-list ,pos-list))))
  ;; Backward sexp
  (dolist (file-and-pos vhdl-ext-test-utils-backward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "bwd.sexp.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-mode
                                           :fn vhdl-ext-backward-sexp
                                           :pos-list ,pos-list))))
  ;; Forward sexp (ts-mode)
  (dolist (file-and-pos vhdl-ext-test-utils-forward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "ts.fwd.sexp.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ext-forward-sexp
                                           :pos-list ,pos-list))))
  ;; Backward sexp (ts-mode)
  (dolist (file-and-pos vhdl-ext-test-utils-backward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "ts.bwd.sexp.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ext-backward-sexp
                                           :pos-list ,pos-list))))
  ;; Point inside block
  (dolist (file-pos-and-match vhdl-ext-test-utils-point-inside-block-file-pos-and-match)
    (let ((file (car file-pos-and-match))
          (pos-and-match-alist (cadr file-pos-and-match)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "point.inside.block.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-and-match-alist-fn
                                   :args `(:mode vhdl-mode
                                           :fn vhdl-ext-point-inside-block
                                           :pos-and-match-alist ,pos-and-match-alist))))
  ;; Point inside block (ts-mode)
  (dolist (file-pos-and-match vhdl-ext-test-utils-point-inside-block-file-pos-and-match)
    (let ((file (car file-pos-and-match))
          (pos-and-match-alist (cadr file-pos-and-match)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "ts.point.inside.block.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-and-match-alist-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ext-point-inside-block
                                           :pos-and-match-alist ,pos-and-match-alist))))
  ;; Block at point
  (dolist (file-and-pos vhdl-ext-test-utils-block-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "block.at.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-mode
                                           :fn vhdl-ext-block-at-point
                                           :pos-list ,pos-list))))
  ;; Block at point (ts-mode)
  (dolist (file-and-pos vhdl-ext-test-utils-block-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "ts.block.at.point.el"
                                   :process-fn 'eval-ff
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ext-block-at-point
                                           :pos-list ,pos-list))))
  ;; Instance at point
  (dolist (file-and-pos vhdl-ext-test-utils-instance-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "inst.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-mode
                                           :fn vhdl-ext-instance-at-point
                                           :pos-list ,pos-list))))
  ;; Instance at point (ts-mode)
  (dolist (file-and-pos vhdl-ext-test-utils-instance-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (test-hdl-gen-expected-files :file-list `(,file)
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "ts.inst.point.el"
                                   :process-fn 'eval
                                   :fn #'test-hdl-pos-list-fn
                                   :args `(:mode vhdl-ts-mode
                                           :fn vhdl-ext-instance-at-point
                                           :pos-list ,pos-list))))
  ;; Scan buffer entities
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-utils-file-list
                               :dest-dir vhdl-ext-test-ref-dir-utils
                               :out-file-ext "scan.entities.el"
                               :process-fn 'eval
                               :fn #'vhdl-ext-test-utils-scan-entities-fn
                               :args `(:mode vhdl-mode))
  ;; Scan buffer entities (ts-mode)
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-utils-file-list
                               :dest-dir vhdl-ext-test-ref-dir-utils
                               :out-file-ext "ts.scan.entities.el"
                               :process-fn 'eval
                               :fn #'vhdl-ext-test-utils-scan-entities-fn
                               :args `(:mode vhdl-ts-mode))
  ;; Proj files
  (let ((file-list vhdl-ext-test-dummy-file-list))
    (vhdl-ext-test-with-test-project vhdl-ext-test-utils-proj-name
      ;; Test1: Set only `:root'
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test1"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-common-dir))
      ;; Test2: Set `:root' and `:dirs'
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test2"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-dir
                                           :dirs ("common"
                                                  "axi_if_converter/rtl")))
      ;; Test3: Set `:root' and `:dirs' recursively
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test3"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-dir
                                           :dirs ("-r common"
                                                  "-r axi_if_converter")))
      ;; Test4: Set `:root', `:dirs' and `:files'
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test4"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-dir
                                           :dirs ("axi_if_converter/rtl")
                                           :files ("axi_if_converter/tb/tb_axi_if_converter.vhd"
                                                   "axi_if_converter/tb/tb_core_fsm.vhd")))
      ;; Test5: Set `:root' and `:ignore-dirs'
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test5"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-common-dir
                                           :ignore-dirs ("subblocks")))
      ;; Test6: Set `:root', `:ignore-dirs' and `:ignore-files'
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test6"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-common-dir
                                           :ignore-dirs ("subblocks")
                                           :ignore-files ("axi_if_converter.vhd"
                                                          "instances.vhd")))
      ;; Test7: Set glob pattern: files
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test7"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-dir
                                           :files ("axi_if_converter/rtl/*.vhd"
                                                   "axi_if_converter/tb/*.vhd"
                                                   "common/*.vhd"
                                                   "common/*.vhdl")
                                           :ignore-files ("common/axi_*.vhd"
                                                          "common/tb_*.vhd")))
      ;; Test8: Set glob pattern: directories
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test8"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-dir
                                           :dirs ("axi_if_converter/*" ; axi_if_converter rtl/tb
                                                  "comm*n/*")))   ; common/subblocks
      ;; Test9: Set glob pattern: recursive directories and ignoring
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test9"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-files-dir
                                           :dirs ("-r comm*n"       ; common/subblocks
                                                  "-r *xi_if_converter") ; axi_if_converter rtl/tb
                                           :ignore-dirs ("*xi_if_converter/tb"))) ; ignore axi_if_converter/tb
      ;; Test10: Set globstar pattern
      (test-hdl-gen-expected-files :file-list file-list
                                   :dest-dir vhdl-ext-test-ref-dir-utils
                                   :out-file-ext "files.test10"
                                   :process-fn 'eval
                                   :fn #'vhdl-ext-test-proj-files-fn
                                   :args `(:root ,vhdl-ext-test-dir
                                           :dirs ("files/**/rtl"
                                                  "**/tb"))))))


(ert-deftest utils::point-inside-block ()
  (dolist (file-pos-and-match vhdl-ext-test-utils-point-inside-block-file-pos-and-match)
    (let ((file (car file-pos-and-match))
          (pos-and-match-alist (cadr file-pos-and-match)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "point.inside.block.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-and-match-alist-fn
                                                           :args `(:mode vhdl-mode
                                                                   :fn vhdl-ext-point-inside-block
                                                                   :pos-and-match-alist ,pos-and-match-alist))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "point.inside.block.el")))))))


(ert-deftest utils::point-inside-block-ts-mode ()
  (dolist (file-pos-and-match vhdl-ext-test-utils-point-inside-block-file-pos-and-match)
    (let ((file (car file-pos-and-match))
          (pos-and-match-alist (cadr file-pos-and-match)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "ts.point.inside.block.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-and-match-alist-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ext-point-inside-block
                                                                   :pos-and-match-alist ,pos-and-match-alist))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "ts.point.inside.block.el")))))))


(ert-deftest utils::block-at-point ()
  (dolist (file-and-pos vhdl-ext-test-utils-block-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "block.at.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-mode
                                                                   :fn vhdl-ext-block-at-point
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "block.at.point.el")))))))


(ert-deftest utils::block-at-point-ts-mode ()
  (dolist (file-and-pos vhdl-ext-test-utils-block-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "ts.block.at.point.el"))
                                                           :process-fn 'eval-ff
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ext-block-at-point
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "ts.block.at.point.el")))))))


(ert-deftest utils::instance-at-point ()
  (dolist (file-and-pos vhdl-ext-test-utils-instance-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "inst.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-mode
                                                                   :fn vhdl-ext-instance-at-point
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "inst.point.el")))))))


(ert-deftest utils::instance-at-point-ts-mode ()
  (dolist (file-and-pos vhdl-ext-test-utils-instance-at-point-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "ts.inst.point.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ext-instance-at-point
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "ts.inst.point.el")))))))


(ert-deftest utils::scan-buffer-modules ()
  (dolist (file vhdl-ext-test-utils-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "scan.entities.el"))
                                                         :process-fn 'eval
                                                         :fn #'vhdl-ext-test-utils-scan-entities-fn
                                                         :args `(:mode vhdl-mode))
                                  (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "scan.entities.el"))))))


(ert-deftest utils::scan-buffer-modules-ts-mode ()
  (dolist (file vhdl-ext-test-utils-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "ts.scan.entities.el"))
                                                         :process-fn 'eval
                                                         :fn #'vhdl-ext-test-utils-scan-entities-fn
                                                         :args `(:mode vhdl-ts-mode))
                                  (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "ts.scan.entities.el"))))))


(ert-deftest utils::forward-sexp ()
  (dolist (file-and-pos vhdl-ext-test-utils-forward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "fwd.sexp.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-mode
                                                                   :fn vhdl-ext-forward-sexp
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "fwd.sexp.el")))))))


(ert-deftest utils::backward-sexp ()
  (dolist (file-and-pos vhdl-ext-test-utils-backward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "bwd.sexp.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-mode
                                                                   :fn vhdl-ext-backward-sexp
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "bwd.sexp.el")))))))

(ert-deftest utils::forward-sexp-ts-mode ()
  (dolist (file-and-pos vhdl-ext-test-utils-forward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "ts.fwd.sexp.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ext-forward-sexp
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "ts.fwd.sexp.el")))))))


(ert-deftest utils::backward-sexp-ts-mode ()
  (dolist (file-and-pos vhdl-ext-test-utils-backward-sexp-file-and-pos)
    (let ((file (car file-and-pos))
          (pos-list (cdr file-and-pos)))
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "ts.bwd.sexp.el"))
                                                           :process-fn 'eval
                                                           :fn #'test-hdl-pos-list-fn
                                                           :args `(:mode vhdl-ts-mode
                                                                   :fn vhdl-ext-backward-sexp
                                                                   :pos-list ,pos-list))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "ts.bwd.sexp.el")))))))


(ert-deftest utils::proj-files ()
  (let ((file (car vhdl-ext-test-dummy-file-list)))
    (vhdl-ext-test-with-test-project vhdl-ext-test-utils-proj-name
      ;; Test1: Set only `:root'
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test1"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-common-dir))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test1"))))
      ;; Test2: Set `:root' and `:dirs'
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test2"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-dir
                                                                   :dirs ("common"
                                                                          "axi_if_converter/rtl")))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test2"))))
      ;; Test3: Set `:root' and `:dirs' recursively
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test3"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-dir
                                                                   :dirs ("-r common"
                                                                          "-r axi_if_converter")))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test3"))))
      ;; Test4: Set `:root', `:dirs' and `:files'
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test4"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-dir
                                                                   :dirs ("axi_if_converter/rtl")
                                                                   :files ("axi_if_converter/tb/tb_axi_if_converter.vhd"
                                                                           "axi_if_converter/tb/tb_core_fsm.vhd")))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test4"))))
      ;; Test5: Set `:root' and `:ignore-dirs'
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test5"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-common-dir
                                                                   :ignore-dirs ("subblocks")))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test5"))))
      ;; Test6: Set `:root', `:ignore-dirs' and `:ignore-files'
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test6"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-common-dir
                                                                   :ignore-dirs ("subblocks")
                                                                   :ignore-files ("axi_if_converter.vhd"
                                                                                  "instances.vhd")))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test6"))))
      ;; Test7: Set glob pattern: files
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test7"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-dir
                                                                   :files ("axi_if_converter/rtl/*.vhd"
                                                                           "axi_if_converter/tb/*.vhd"
                                                                           "common/*.vhd"
                                                                           "common/*.vhdl")
                                                                   :ignore-files ("common/axi_*.vhd"
                                                                                  "common/tb_*.vhd")))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test7"))))
      ;; Test8: Set glob pattern: directories
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test8"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-dir
                                                                   :dirs ("axi_if_converter/*" ; axi_if_converter rtl/tb
                                                                          "comm*n/*")))   ; common/subblocks
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test8"))))
      ;; Test9: Set glob pattern: recursive directories and ignoring
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test9"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-files-dir
                                                                   :dirs ("-r comm*n"       ; common/subblocks
                                                                          "-r *xi_if_converter") ; axi_if_converter rtl/tb
                                                                   :ignore-dirs ("*xi_if_converter/tb"))) ; ignore axi_if_converter/tb
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test9"))))
      ;; Test10: Set globstar pattern
      (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                           :dump-file (file-name-concat vhdl-ext-test-dump-dir-utils (test-hdl-basename file "files.test10"))
                                                           :process-fn 'eval
                                                           :fn #'vhdl-ext-test-proj-files-fn
                                                           :args `(:root ,vhdl-ext-test-dir
                                                                   :dirs ("files/**/rtl"
                                                                          "**/tb")))
                                    (file-name-concat vhdl-ext-test-ref-dir-utils (test-hdl-basename file "files.test10")))))))


(provide 'vhdl-ext-test-utils)

;;; vhdl-ext-test-utils.el ends here
