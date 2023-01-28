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
  "Update .faceup files.
INFO: Makes sure that additional settings that might change specific font-lock
are disabled for the .faceup generation.
E.g: disables `fic-mode', `untabify-trailing-ws', 'outshine-mode'.
At some point tried with `with-temp-buffer' without success."
  (save-window-excursion
    (when (fboundp 'untabify-trailing-ws-mode)
      (untabify-trailing-ws-mode -1)
      (message "Disabling untabify-trailing-ws-mode..."))
    (dolist (file (directory-files vhdl-ext-tests-examples-dir t ".vhd$"))
      (find-file file)
      (if tree-sitter
          (vhdl-ts-mode)
        (vhdl-mode))
      (when (fboundp 'fic-mode)
        (fic-mode -1)
        (message "Disabling fic-mode for file %s" file))
      (when (fboundp 'outshine-mode)
        (outshine-mode -1)
        (message "Disabling outshine-mode for file %s" file))
      (message "Processing %s" file)
      (faceup-write-file (concat (file-name-directory file)
                                 "faceup/"
                                 (file-name-nondirectory file)
                                 (when tree-sitter
                                   ".ts")
                                 ".faceup")))))

(defun vhdl-ext-test-font-lock-test-file (file &optional tree-sitter)
  "Test that VHDL FILE fontifies as the .faceup file describes."
  (let ((vhdl-align-typedef-regexp nil)
        (mode (if tree-sitter
                  'vhdl-ts-mode
                'vhdl-mode)))
    (cl-letf (((symbol-function 'message)
               (lambda (FORMAT-STRING &rest ARGS)
                 nil))) ; Mock `message' to silence VHDL version reporting
      (faceup-test-font-lock-file mode
                                  (vhdl-ext-path-join vhdl-ext-tests-examples-dir file)
                                  (vhdl-ext-path-join vhdl-ext-tests-faceup-dir (concat file
                                                                                        (when tree-sitter
                                                                                          ".ts")
                                                                                        ".faceup"))))))

(faceup-defexplainer vhdl-ext-test-font-lock-test-file)

(ert-deftest font-lock::generic ()
  (should (vhdl-ext-test-font-lock-test-file "axi_if_converter.vhd"))
  (should (vhdl-ext-test-font-lock-test-file "tb_axi_if_converter.vhd"))
  (should (vhdl-ext-test-font-lock-test-file "global_pkg.vhd"))
  (should (vhdl-ext-test-font-lock-test-file "global_sim.vhd")))

(provide 'vhdl-ext-tests-font-lock)

;;; vhdl-ext-tests-font-lock.el ends here
