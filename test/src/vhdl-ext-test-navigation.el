;;; vhdl-ext-test-navigation.el --- vhdl-ext ERT navigation tests  -*- lexical-binding: t -*-

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
;; vhdl-ext ERT navigation tests
;;
;;; Code:

(defconst vhdl-ext-test-ref-dir-navigation (file-name-concat vhdl-ext-test-ref-dir "navigation"))
(defconst vhdl-ext-test-dump-dir-navigation (file-name-concat vhdl-ext-test-dump-dir "navigation"))

(defconst vhdl-ext-test-navigation-jump-to-parent-file-list
  (test-hdl-directory-files (file-name-concat vhdl-ext-test-files-dir "subblocks") vhdl-ext-file-extension-re))

(defconst vhdl-ext-test-navigation-rtl-file-list (mapcar (lambda (file)
                                                           (file-name-concat vhdl-ext-test-files-common-dir file))
                                                         '("axi_if_converter.vhd"
                                                           "hierarchy.vhd"
                                                           "instances.vhd"
                                                           "tb_axi_if_converter.vhd")))
(defconst vhdl-ext-test-navigation-tb-file-list (mapcar (lambda (file)
                                                          (file-name-concat vhdl-ext-test-files-common-dir file))
                                                        '("global_pkg.vhd"
                                                          "global_sim.vhd"
                                                          "misc.vhd"
                                                          "sexp.vhd")))

(defconst vhdl-ext-test-navigation-block-nav-file-list vhdl-ext-test-common-file-list)



(cl-defun vhdl-ext-test-jump-to-parent-entity (&key mode engine)
  (cl-letf (((symbol-function 'compilation-start)
             (lambda (command &optional mode name-function highlight-regexp)
               (butlast (split-string (shell-command-to-string command) "\n") 4)))
            ((symbol-function 'vhdl-ext-buffer-proj-root)
             (lambda (&optional project)
               vhdl-ext-test-files-common-dir)))
    (let* ((vhdl-ext-jump-to-parent-entity-engine engine)
           ;; INFO: Using let-binding in ripgrep.el arguments for compatibility with release 0.4.0 (Feb 2017) for MELPA Stable tests
           ;;
           ;; From man rg(1):
           ;;
           ;; --vimgrep
           ;;     Show results with every match on its own line, including line
           ;;     numbers and column numbers. With this option, a line with more than
           ;;     one match will be printed more than once.
           (ripgrep-highlight-search nil)
           (ripgrep-arguments '("--vimgrep")))
      ;; Core after all the function setup, using default args for ag and rg
      (test-hdl-no-messages
        (funcall mode))
      (vhdl-ext-jump-to-parent-entity))))


(defun vhdl-ext-test-navigation-gen-expected-files ()
  ;; Instances fwd
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-navigation-rtl-file-list
                               :dest-dir vhdl-ext-test-ref-dir-navigation
                               :out-file-ext "inst.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-mode
                                       :fn vhdl-ext-find-entity-instance-fwd))
  ;; Instances bwd
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-navigation-rtl-file-list
                               :dest-dir vhdl-ext-test-ref-dir-navigation
                               :out-file-ext "inst.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-mode
                                       :fn vhdl-ext-find-entity-instance-bwd
                                       :start-pos-max t))
  ;; Instances fwd (ts-mode)
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-navigation-rtl-file-list
                               :dest-dir vhdl-ext-test-ref-dir-navigation
                               :out-file-ext "ts.inst.fwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-ts-mode
                                       :fn vhdl-ext-find-entity-instance-fwd))
  ;; Instances bwd (ts-mode)
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-navigation-rtl-file-list
                               :dest-dir vhdl-ext-test-ref-dir-navigation
                               :out-file-ext "ts.inst.bwd.el"
                               :process-fn 'eval
                               :fn #'test-hdl-navigation-nav-file-fn
                               :args '(:mode vhdl-ts-mode
                                       :fn vhdl-ext-find-entity-instance-bwd
                                       :start-pos-max t))
  ;; Jump-to-parent ag
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-navigation-jump-to-parent-file-list
                               :dest-dir vhdl-ext-test-ref-dir-navigation
                               :out-file-ext "ag"
                               :process-fn 'eval
                               :fn #'vhdl-ext-test-jump-to-parent-entity
                               :args `(:mode vhdl-mode
                                       :engine "ag"))
  ;; Jump-to-parent rg
  (test-hdl-gen-expected-files :file-list vhdl-ext-test-navigation-jump-to-parent-file-list
                               :dest-dir vhdl-ext-test-ref-dir-navigation
                               :out-file-ext "rg"
                               :process-fn 'eval
                               :fn #'vhdl-ext-test-jump-to-parent-entity
                               :args `(:mode vhdl-mode
                                       :engine "rg")))


(ert-deftest navigation::instances ()
  (dolist (file vhdl-ext-test-navigation-rtl-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-navigation (test-hdl-basename file "inst.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-mode
                                                                 :fn vhdl-ext-find-entity-instance-fwd))
                                  (file-name-concat vhdl-ext-test-ref-dir-navigation (test-hdl-basename file "inst.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-navigation (test-hdl-basename file "inst.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-mode
                                                                 :fn vhdl-ext-find-entity-instance-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat vhdl-ext-test-ref-dir-navigation (test-hdl-basename file "inst.bwd.el"))))))

(ert-deftest navigation::instances-ts-mode ()
  (dolist (file vhdl-ext-test-navigation-rtl-file-list)
    ;; Forward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-navigation (test-hdl-basename file "ts.inst.fwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-ts-mode
                                                                 :fn vhdl-ext-find-entity-instance-fwd))
                                  (file-name-concat vhdl-ext-test-ref-dir-navigation (test-hdl-basename file "ts.inst.fwd.el"))))
    ;; Backward
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-navigation (test-hdl-basename file "ts.inst.bwd.el"))
                                                         :process-fn 'eval
                                                         :fn #'test-hdl-navigation-nav-file-fn
                                                         :args '(:mode vhdl-ts-mode
                                                                 :fn vhdl-ext-find-entity-instance-bwd
                                                                 :start-pos-max t))
                                  (file-name-concat vhdl-ext-test-ref-dir-navigation (test-hdl-basename file "ts.inst.bwd.el"))))))


(ert-deftest navigation::jump-to-parent-entity-ag ()
  (dolist (file vhdl-ext-test-navigation-jump-to-parent-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-navigation (test-hdl-basename file "ag"))
                                                         :process-fn 'eval
                                                         :fn #'vhdl-ext-test-jump-to-parent-entity
                                                         :args '(:mode vhdl-mode
                                                                 :engine "ag"))
                                  (file-name-concat vhdl-ext-test-ref-dir-navigation (test-hdl-basename file "ag"))))))


(ert-deftest navigation::jump-to-parent-entity-rg ()
  (dolist (file vhdl-ext-test-navigation-jump-to-parent-file-list)
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-navigation (test-hdl-basename file "rg"))
                                                         :process-fn 'eval
                                                         :fn #'vhdl-ext-test-jump-to-parent-entity
                                                         :args '(:mode vhdl-mode
                                                                 :engine "rg"))
                                  (file-name-concat vhdl-ext-test-ref-dir-navigation (test-hdl-basename file "rg"))))))



(provide 'vhdl-ext-test-navigation)

;;; vhdl-ext-test-navigation.el ends here
