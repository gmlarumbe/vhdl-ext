;;; vhdl-ext-test-tags.el --- vhdl-ext ERT tags tests  -*- lexical-binding: t -*-

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


;;;; Aux functions (used for capf/hierarchy/xref)
(defconst vhdl-ext-test-tags-proj-name "vhdl-ext-test-tags")

(cl-defun vhdl-ext-test-tags-get (&key root files rel-path)
  "Populate the value of the tags tables for test-hdl-vhdl project."
  (let ((vhdl-ext-project-alist `((,vhdl-ext-test-tags-proj-name
                                   :root ,(or root vhdl-ext-test-files-common-dir)
                                   :files ,files
                                   :workdir "lib/")))
        (default-directory vhdl-ext-test-files-common-dir)) ; DANGER: Needed to get relative filename for GitHub Actions via advice
    ;; Get tags after setting environment
    (test-hdl-no-messages
      (vhdl-ext-tags-clear-cache) ; INFO: This is very important in order to start off with a clean environment
      ;; Make file entries relative to avoid issues in GitHub Actions CI with a different $HOME
      (when rel-path
        (advice-add 'vhdl-ext-proj-files :filter-return #'test-hdl-tags-proj-files-relative))
      (vhdl-ext-tags-get)
      (when rel-path
        (advice-remove 'vhdl-ext-proj-files #'test-hdl-tags-proj-files-relative)))))


;;;; Standalone tests
(defconst vhdl-ext-test-tags-file-list vhdl-ext-test-common-file-list)

(defconst vhdl-ext-test-ref-dir-tags (file-name-concat vhdl-ext-test-ref-dir "tags"))
(defconst vhdl-ext-test-dump-dir-tags (file-name-concat vhdl-ext-test-dump-dir "tags"))

(defun vhdl-ext-test-tags-setup ()
  "Avoid errors in desc when there are tabs and trailing whitespaces."
  (let ((disable-serialization nil))
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace (point-min) (point-max))
    ;; The lines below are run every time a file is processed in `vhdl-ext-tags-get--process-file'
    (setq vhdl-ext-tags-defs-current-file (make-hash-table :test #'equal))
    (setq vhdl-ext-tags-inst-current-file (make-hash-table :test #'equal))
    (setq vhdl-ext-tags-refs-current-file (make-hash-table :test #'equal))
    (treesit-parser-create 'vhdl)
    ;; Avoid cache serialization in batch mode, if set locally
    (when disable-serialization
      (remove-hook 'kill-emacs-hook #'vhdl-ext-tags-serialize))))

(defun vhdl-ext-test-tags-ts-defs-file-fn (file)
  (let ((file (file-relative-name file test-hdl-test-dir))) ; Use relative path for GitHub Actions
    (vhdl-ext-test-tags-setup)
    (vhdl-ext-tags-table-push-defs-ts file)
    vhdl-ext-tags-defs-current-file))

(defun vhdl-ext-test-tags-ts-refs-file-fn (file)
  (let ((file (file-relative-name file test-hdl-test-dir))) ; Use relative path for GitHub Actions
    (vhdl-ext-test-tags-setup)
    (vhdl-ext-tags-table-push-refs-ts file)
    vhdl-ext-tags-refs-current-file))

(defun vhdl-ext-test-tags-gen-expected-files ()
  ;; Tree-sitter
  (dolist (file vhdl-ext-test-tags-file-list)
    ;; Per-file defs
    (test-hdl-gen-expected-files :file-list `(,file)
                                 :dest-dir vhdl-ext-test-ref-dir-tags
                                 :out-file-ext "ts.defs.el"
                                 :process-fn 'eval
                                 :fn #'vhdl-ext-test-tags-ts-defs-file-fn
                                 :args `(,file))
    ;; Per-file refs
    (test-hdl-gen-expected-files :file-list `(,file)
                                 :dest-dir vhdl-ext-test-ref-dir-tags
                                 :out-file-ext "ts.refs.el"
                                 :process-fn 'eval
                                 :fn #'vhdl-ext-test-tags-ts-refs-file-fn
                                 :args `(,file))))


(ert-deftest tags::tree-sitter ()
  (dolist (file vhdl-ext-test-tags-file-list)
    ;; Per-file defs
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-tags (test-hdl-basename file "ts.defs.el"))
                                                         :process-fn 'eval
                                                         :fn #'vhdl-ext-test-tags-ts-defs-file-fn
                                                         :args `(,file))
                                  (file-name-concat vhdl-ext-test-ref-dir-tags (test-hdl-basename file "ts.defs.el"))))
    ;; Per-file refs
    (should (test-hdl-files-equal (test-hdl-process-file :test-file file
                                                         :dump-file (file-name-concat vhdl-ext-test-dump-dir-tags (test-hdl-basename file "ts.refs.el"))
                                                         :process-fn 'eval
                                                         :fn #'vhdl-ext-test-tags-ts-refs-file-fn
                                                         :args `(,file))
                                  (file-name-concat vhdl-ext-test-ref-dir-tags (test-hdl-basename file "ts.refs.el"))))))


(provide 'vhdl-ext-test-tags)

;;; vhdl-ext-test-tags.el ends here
