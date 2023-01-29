;;; vhdl-navigation.el --- VHDL Navigation  -*- lexical-binding: t -*-

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
;; Navigation functions:
;;  - Find instances forward/backwards
;;  - Jump to definition/reference of entity at point (requires gtags/xref)
;;
;;; Code:


(require 'vhdl-utils)
(require 'ggtags)


(defcustom vhdl-ext-jump-to-parent-module-engine "ag"
  "Default program to find parent module instantiations.
Either `rg' or `ag' are implemented."
  :type '(choice (const :tag "silver searcher" "ag")
                 (const :tag "ripgrep"         "rg"))
  :group 'vhdl-ext)



(defun vhdl-ext-find-entity-instance (&optional limit bwd interactive-p)
  "Search for a VHDL entity/instance.
Optional LIMIT argument bounds the search.

If optional argument BWD is non-nil search backwards instead.

Third arg INTERACTIVE-P specifies whether function call should be treated as if
it was interactive.  This changes the position where point will be at the end of
the function call."
  (let ((found nil)
        (pos))
    (save-excursion
      (when interactive-p
        (if bwd
            (backward-char))
        (forward-char))
      (while (and (not found)
                  (if bwd
                      (re-search-backward vhdl-ext-instance-re limit t)
                    (re-search-forward vhdl-ext-instance-re limit t)))
        (unless (or (equal (face-at-point) 'font-lock-comment-face)
                    (equal (face-at-point) 'font-lock-string-face))
          (setq found t)
          (if interactive-p
              (setq pos (match-beginning 6))
            (setq pos (point))))))
    (when found
      (goto-char pos))))

(defun vhdl-ext-find-entity-instance-fwd (&optional limit)
  "Search forward for a VHDL entity/instance.
Optional LIMIT argument bounds the search."
  (interactive)
  (let ((interactive-p (called-interactively-p 'any)))
    (vhdl-ext-find-entity-instance limit nil interactive-p)))

(defun vhdl-ext-find-entity-instance-bwd (&optional limit)
  "Search backward for a VHDL entity/instance.
Optional LIMIT argument bounds the search."
  (interactive)
  (let ((interactive-p (called-interactively-p 'any)))
    (vhdl-ext-find-entity-instance limit :bwd interactive-p)))

(defun vhdl-ext-instance-at-point ()
  "Return list with entity and instance names if point is at an instance."
  (let ((point-cur (point))
        point-instance-begin point-instance-end instance-type instance-name)
    (save-excursion
      (when (and (vhdl-re-search-forward ";" nil t)
                 (vhdl-ext-find-entity-instance-bwd) ; Sets match data
                 (setq instance-name (match-string-no-properties 1))
                 (setq instance-type (match-string-no-properties 6))
                 (setq point-instance-begin (match-beginning 1))
                 (vhdl-re-search-forward ";" nil t) ; Needed to avoid issues with last instance on a file
                 (setq point-instance-end (1- (point))))
        (if (and (>= point-cur point-instance-begin)
                 (<= point-cur point-instance-end))
            (list instance-type instance-name)
          nil)))))

(defun vhdl-ext-jump-to-entity-at-point (&optional ref)
  "Jump to definition of instance at point.
If REF is non-nil show references instead."
  (interactive)
  (unless (executable-find "global")
    (error "Couldn't find executable `global' in PATH"))
  (unless (member 'ggtags--xref-backend xref-backend-functions)
    (error "Error: ggtags not configured as an xref backend.  Is ggtags-mode enabled?"))
  (unless ggtags-project-root
    (error "Error: `ggtags-project-root' not set.  Are GTAGS/GRTAGS/GPATH files created?"))
  (let ((entity (car (vhdl-ext-instance-at-point))))
    (if entity
        (progn
          (if ref
              (xref-find-references entity)
            (xref-find-definitions entity))
          entity) ; Report entity name
      (user-error "Not inside a VHDL instance"))))


;;;; Jump to parent module
(defvar vhdl-ext-jump-to-parent-module-point-marker nil
  "Point marker to save the state of the buffer where the search was started.
Used in ag/rg end of search hooks to conditionally set the xref marker stack.")
(defvar vhdl-ext-jump-to-parent-module-name nil)
(defvar vhdl-ext-jump-to-parent-module-dir nil)
(defvar vhdl-ext-jump-to-parent-trigger nil
  "Variable to run the post ag/rg command hook only when the ag/rg search
was triggered by `vhdl-ext-jump-to-parent-module' command.")

(defun vhdl-ext-jump-to-parent-module ()
  "Find current module/interface instantiations via `ag'/`rg'.
Configuration should be done so that `vhdl-ext-navigation-ag-rg-hook' is run
after the search has been done."
  (interactive)
  (let* ((proj-dir (vhdl-ext-project-root))
         (entity-name (or (vhdl-ext-select-file-entity buffer-file-name)
                          (error "No entity found @ %s" buffer-file-name)))
         ;; Regexp fetched from `vhdl-ext-instance-re', replaced "\\s-" with "[ ]"
         ;; and dismissing \n to allow for easy elisp to pcre conversion
         (entity-instance-pcre (concat "^[ ]*(" vhdl-ext-identifier-re ")[ ]*:[ ]*" ; Instance name
                                       "(((component)|(configuration)|(entity))[ ]+(" vhdl-ext-identifier-re ")\\.)?"
                                       "\\b(" entity-name ")\\b")))
    ;; Update variables used by the ag/rg search finished hooks
    (setq vhdl-ext-jump-to-parent-module-name entity-name)
    (setq vhdl-ext-jump-to-parent-module-dir proj-dir)
    ;; Perform project based search
    (cond
     ;; Try ripgrep
     ((and (string= vhdl-ext-jump-to-parent-module-engine "rg")
           (executable-find "rg"))
      (let ((rg-extra-args '("-t" "vhdl" "--pcre2" "--multiline" "--stats")))
        (setq vhdl-ext-jump-to-parent-module-point-marker (point-marker))
        (setq vhdl-ext-jump-to-parent-trigger t)
        (ripgrep-regexp entity-instance-pcre proj-dir rg-extra-args)))
     ;; Try ag
     ((and (string= vhdl-ext-jump-to-parent-module-engine "ag")
           (executable-find "ag"))
      (let ((ag-arguments ag-arguments)
            (extra-ag-args '("--vhdl" "--stats")))
        (dolist (extra-ag-arg extra-ag-args)
          (add-to-list 'ag-arguments extra-ag-arg :append))
        (setq vhdl-ext-jump-to-parent-module-point-marker (point-marker))
        (setq vhdl-ext-jump-to-parent-trigger t)
        (ag-regexp entity-instance-pcre proj-dir)))
     ;; Fallback
     (t
      (error "Did not find `rg' nor `ag' in $PATH")))))

(defun vhdl-ext-navigation-ag-rg-hook ()
  "Jump to the first result and push xref marker if there were any matches.
Kill the buffer if there is only one match."
  (when vhdl-ext-jump-to-parent-trigger
    (let ((entity-name (propertize vhdl-ext-jump-to-parent-module-name 'face '(:foreground "green")))
          (dir (propertize vhdl-ext-jump-to-parent-module-dir 'face '(:foreground "light blue")))
          (num-matches))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\([0-9]+\\) matches\\s-*$" nil :noerror)
        (setq num-matches (string-to-number (match-string-no-properties 1))))
      (cond ((eq num-matches 1)
             (xref-push-marker-stack vhdl-ext-jump-to-parent-module-point-marker)
             (next-error)
             (kill-buffer (current-buffer))
             (message "Jump to only match for [%s] @ %s" entity-name dir))
            ((> num-matches 1)
             (xref-push-marker-stack vhdl-ext-jump-to-parent-module-point-marker)
             (next-error)
             (message "Showing matches for [%s] @ %s" entity-name dir))
            (t
             (kill-buffer (current-buffer))
             (message "No matches found")))
      (setq vhdl-ext-jump-to-parent-trigger nil))))


;;; Setup
(add-hook 'ag-search-finished-hook #'vhdl-ext-navigation-ag-rg-hook)
(add-hook 'ripgrep-search-finished-hook #'vhdl-ext-navigation-ag-rg-hook)


(provide 'vhdl-navigation)

;;; vhdl-navigation.el ends here
