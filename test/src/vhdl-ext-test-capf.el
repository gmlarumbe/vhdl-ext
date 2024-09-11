;;; vhdl-ext-test-capf.el --- vhdl-ext ERT tags tests  -*- lexical-binding: t -*-

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
;; vhdl-ext ERT tags tests
;;
;;; Code:

(defconst vhdl-ext-test-ref-dir-capf (file-name-concat vhdl-ext-test-ref-dir "capf"))
(defconst vhdl-ext-test-dump-dir-capf (file-name-concat vhdl-ext-test-dump-dir "capf"))

(defconst vhdl-ext-test-capf-file-pos-init-string-alist
  `((,(file-name-concat vhdl-ext-test-files-common-dir "axi_if_converter.vhd")
     ((168 "library ")    ; library autocompletion
      (168 "use i")       ; library autocompletion
      (168 "use ieee.")   ; library package autocompletion
      (168 "use ieee.s")  ; library package autocompletion
      (14048 "sig")       ; Declarative part: signal declaration
      (14344 "C_")        ; Generic inside instance
      (14484 "s")         ; Port inside instance
      (33599 "I_")        ; Concurrent statements: Instance
      (point-max "a")))   ; Outside entity/architecture body

    (,(file-name-concat vhdl-ext-test-files-common-dir "global_sim.vhd")
     ((227 "constant AX")    ; Constant in package
      (348 "pr")             ; Procedure keyword
      (348 "wr")             ; write procedures
      (point-max "en")))     ; end
    ))


(defun vhdl-ext-test-capf-anotation-fn ()
  (mapcar #'vhdl-ext-capf-annotation-function (nth 2 (vhdl-ext-capf))))


(defun vhdl-ext-test-capf-gen-expected-files ()
  (vhdl-ext-test-with-test-project vhdl-ext-test-tags-proj-name
    ;; Generate/update tags for test project
    (vhdl-ext-test-tags-get :root vhdl-ext-test-files-common-dir
                            :files vhdl-ext-test-common-file-list)
    (dolist (file-pos-and-init-string vhdl-ext-test-capf-file-pos-init-string-alist)
      (let ((file (car file-pos-and-init-string))
            (pos-and-init-string-alist (cadr file-pos-and-init-string)))
        ;; Completion
        (test-hdl-gen-expected-files :file-list `(,file)
                                     :dest-dir vhdl-ext-test-ref-dir-capf
                                     :out-file-ext "capf.el"
                                     :process-fn 'eval
                                     :fn #'test-hdl-capf-fn
                                     :args `(:capf-fn vhdl-ext-capf
                                             :pos-init-string-alist ,pos-and-init-string-alist))
        ;; Annotation
        (test-hdl-gen-expected-files :file-list `(,file)
                                     :dest-dir vhdl-ext-test-ref-dir-capf
                                     :out-file-ext "annotations.el"
                                     :process-fn 'eval
                                     :fn #'test-hdl-capf-fn
                                     :args `(:capf-fn vhdl-ext-test-capf-anotation-fn
                                             :pos-init-string-alist ,pos-and-init-string-alist))))))


(ert-deftest capf::completions ()
  (vhdl-ext-test-with-test-project vhdl-ext-test-tags-proj-name
    ;; Generate/update tags for test project
    (vhdl-ext-test-tags-get :root vhdl-ext-test-files-common-dir
                            :files vhdl-ext-test-common-file-list)
    ;; Test each file
    (dolist (file-pos-and-init-string vhdl-ext-test-capf-file-pos-init-string-alist)
      (let ((file (car file-pos-and-init-string))
            (pos-and-init-string-alist (cadr file-pos-and-init-string)))
        (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                             :dump-file (file-name-concat vhdl-ext-test-dump-dir-capf (test-hdl-basename file "capf.el"))
                                                             :process-fn 'eval
                                                             :fn #'test-hdl-capf-fn
                                                             :args `(:capf-fn vhdl-ext-capf
                                                                     :pos-init-string-alist ,pos-and-init-string-alist))
                                      (file-name-concat vhdl-ext-test-ref-dir-capf (test-hdl-basename file "capf.el"))))))))


(ert-deftest capf::annotations ()
  (vhdl-ext-test-with-test-project vhdl-ext-test-tags-proj-name
    ;; Generate/update tags for test project
    (vhdl-ext-test-tags-get :root vhdl-ext-test-files-common-dir
                            :files vhdl-ext-test-common-file-list)
    ;; Test each file
    (dolist (file-pos-and-init-string vhdl-ext-test-capf-file-pos-init-string-alist)
      (let ((file (car file-pos-and-init-string))
            (pos-and-init-string-alist (cadr file-pos-and-init-string)))
        (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                             :dump-file (file-name-concat vhdl-ext-test-dump-dir-capf (test-hdl-basename file "annotations.el"))
                                                             :process-fn 'eval
                                                             :fn #'test-hdl-capf-fn
                                                             :args `(:capf-fn vhdl-ext-test-capf-anotation-fn
                                                                     :pos-init-string-alist ,pos-and-init-string-alist))
                                      (file-name-concat vhdl-ext-test-ref-dir-capf (test-hdl-basename file "annotations.el"))))))))


(provide 'vhdl-ext-test-capf)

;;; vhdl-ext-test-capf.el ends here
