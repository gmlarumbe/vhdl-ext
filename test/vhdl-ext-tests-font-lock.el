;;; vhdl-ext-tests-font-lock.el --- vhdl-ext ERT font-lock tests  -*- lexical-binding: t -*-

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
;; ERT font-lock tests:
;; - https://github.com/Lindydancer/faceup
;;
;;; Code:


(require 'faceup)

(defun vhdl-ext-test-font-lock-update-dir (&optional tree-sitter)
  "Update .faceup files."
  (save-window-excursion
    (dolist (file (directory-files vhdl-ext-tests-common-dir t ".vhd$"))
      (find-file file)
      (if tree-sitter
          (vhdl-ts-mode)
        (vhdl-mode))
      (message "Processing %s" file)
      ;; It is needed to explicitly fontify for batch-mode updates, since by
      ;; default batch mode does not enable font-lock.  Initially tried with
      ;; `font-lock-ensure' but gave different results for tree-sitter.  Plus,
      ;; `faceup-write-file' calls internally `font-lock-fontify-region' so
      ;; it's more consistent
      (font-lock-fontify-region (point-min) (point-max))
      (faceup-write-file (file-name-concat vhdl-ext-tests-faceup-dir
                                             (concat (file-name-nondirectory file)
                                                     (when tree-sitter
                                                       ".ts")
                                                     ".faceup"))))))

(defun vhdl-ext-test-font-lock-test-file (file &optional tree-sitter)
  "Test that VHDL FILE fontifies as the .faceup file describes."
  (let ((mode (if tree-sitter
                  'vhdl-ts-mode
                'vhdl-mode))
        result)
    (cl-letf (((symbol-function 'message)
               (lambda (FORMAT-STRING &rest ARGS)
                 nil))) ; Mock `message' to silence VHDL version reporting
      (setq result (faceup-test-font-lock-file mode
                                               (file-name-concat vhdl-ext-tests-common-dir file)
                                               (file-name-concat vhdl-ext-tests-faceup-dir (concat file
                                                                                                     (when tree-sitter
                                                                                                       ".ts")
                                                                                                     ".faceup")))))
    (if (eq t result)
        t ; Propagate 't for the test to pass
      ;; In case of failure, show also which file failed
      (push file result)
      result)))

(ert-deftest font-lock::generic ()
  (let ((default-directory vhdl-ext-tests-common-dir)
        (faceup-test-explain t))
    (dolist (file (directory-files vhdl-ext-tests-common-dir nil ".vhd$"))
      (should (eq t (vhdl-ext-test-font-lock-test-file file))))))


(provide 'vhdl-ext-tests-font-lock)

;;; vhdl-ext-tests-font-lock.el ends here
