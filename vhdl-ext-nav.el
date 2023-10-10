;;; vhdl-ext-nav.el --- VHDL Navigation -*- lexical-binding: t -*-

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

;; Navigation functions
;;  - Find instances forward/backwards
;;  - Jump to definition/reference of entity at point

;;; Code:

(require 'ag)
(require 'ripgrep)
(require 'xref)
(require 'vhdl-ext-utils)


(defcustom vhdl-ext-jump-to-parent-entity-engine "ag"
  "Default program to find parent entity instantiations.
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
            (backward-char)
          (forward-char)))
      (if bwd
          (setq found (vhdl-re-search-backward vhdl-ext-instance-re limit t))
        (setq found (vhdl-re-search-forward vhdl-ext-instance-re limit t)))
      (if interactive-p
          (setq pos (match-beginning 6))
        (setq pos (point))))
    (if (not found)
        (when interactive-p
          (message "Cound not find instance %s" (if bwd "backward" "forward")))
      ;; When found
      (when interactive-p
        (message (concat (match-string-no-properties 1) " : " (match-string-no-properties 6))))
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
        (when (and (>= point-cur point-instance-begin)
                   (<= point-cur point-instance-end))
          (set-match-data (list point-instance-begin
                                point-instance-end))
          (list instance-type instance-name))))))

(defun vhdl-ext-jump-to-entity-at-point (&optional ref)
  "Jump to definition of instance at point.
If REF is non-nil show references instead."
  (interactive)
  (let ((entity (car (vhdl-ext-instance-at-point))))
    (if entity
        (progn
          (if ref
              (xref-find-references entity)
            (xref-find-definitions entity))
          entity) ; Report entity name
      (user-error "Not inside a VHDL instance"))))

(defun vhdl-ext-jump-to-entity-at-point-def ()
  "Jump to definition of entity at point."
  (interactive)
  (vhdl-ext-jump-to-entity-at-point))

(defun vhdl-ext-jump-to-entity-at-point-ref ()
  "Show references of entity at point."
  (interactive)
  (vhdl-ext-jump-to-entity-at-point :ref))


;;;; Jump to parent module
(defvar vhdl-ext-jump-to-parent-entity-point-marker nil
  "Point marker to save the state of the buffer where the search was started.
Used in ag/rg end of search hooks to conditionally set the xref marker stack.")
(defvar vhdl-ext-jump-to-parent-entity-name nil)
(defvar vhdl-ext-jump-to-parent-entity-dir nil)
(defvar vhdl-ext-jump-to-parent-entity-trigger nil
  "Variable to run the post ag/rg command hook.
Run only when the ag/rg search was triggered by `vhdl-ext-jump-to-parent-entity'
command.")
(defvar vhdl-ext-jump-to-parent-entity-starting-windows nil
  "Variable to register how many windows are open when trying to jump-to-parent.")

(defun vhdl-ext-jump-to-parent-entity ()
  "Find current module/interface instantiations via `ag'/`rg'.

Configuration should be done so that `vhdl-ext-navigation-ag-rg-hook' is run
after the search has been done."
  (interactive)
  (let* ((proj-dir (vhdl-ext-buffer-proj-root))
         (entity-name (or (vhdl-ext-select-file-entity buffer-file-name)
                          (error "No entity found @ %s" buffer-file-name)))
         ;; Regexp fetched from `vhdl-ext-instance-re', replaced "\\s-" with "[ ]"
         ;; and dismissing \n to allow for easy elisp to pcre conversion
         (entity-instance-pcre (concat "^[ ]*(" vhdl-ext-identifier-re ")[ ]*:[ ]*" ; Instance name
                                       "(((component)|(configuration)|(entity))[ ]+(" vhdl-ext-identifier-re ")\\.)?"
                                       "\\b(" entity-name ")\\b")))
    ;; Update variables used by the ag/rg search finished hooks
    (setq vhdl-ext-jump-to-parent-entity-name entity-name)
    (setq vhdl-ext-jump-to-parent-entity-dir proj-dir)
    (setq vhdl-ext-jump-to-parent-entity-starting-windows (length (window-list)))
    ;; Perform project based search
    (cond
     ;; Try ripgrep
     ((and (string= vhdl-ext-jump-to-parent-entity-engine "rg")
           (executable-find "rg"))
      (let ((rg-extra-args '("-t" "vhdl" "--pcre2" "--multiline" "--stats" "--ignore-case")))
        (setq vhdl-ext-jump-to-parent-entity-point-marker (point-marker))
        (setq vhdl-ext-jump-to-parent-entity-trigger t)
        (ripgrep-regexp entity-instance-pcre proj-dir rg-extra-args)))
     ;; Try ag
     ((and (string= vhdl-ext-jump-to-parent-entity-engine "ag")
           (executable-find "ag"))
      (let ((ag-arguments ag-arguments)
            (extra-ag-args '("--vhdl" "--stats" "--ignore-case")))
        (dolist (extra-ag-arg extra-ag-args)
          (add-to-list 'ag-arguments extra-ag-arg :append))
        (setq vhdl-ext-jump-to-parent-entity-point-marker (point-marker))
        (setq vhdl-ext-jump-to-parent-entity-trigger t)
        (ag-regexp entity-instance-pcre proj-dir)))
     ;; Fallback
     (t
      (error "Did not find `rg' nor `ag' in $PATH")))))

(defun vhdl-ext-navigation-ag-rg-hook-cleanup ()
  "Handle buffer killing depending on the number of active windows."
  (if (> vhdl-ext-jump-to-parent-entity-starting-windows 1)
      (kill-buffer (current-buffer))
    (other-window 1)
    (delete-window)))

(defun vhdl-ext-navigation-ag-rg-hook ()
  "Jump to the first result and push xref marker if there were any matches.
Kill the buffer or delete window if there is only one match."
  (when vhdl-ext-jump-to-parent-entity-trigger
    (let ((entity-name (propertize vhdl-ext-jump-to-parent-entity-name 'face '(:foreground "green")))
          (dir (propertize vhdl-ext-jump-to-parent-entity-dir 'face '(:foreground "light blue")))
          (num-matches))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\([0-9]+\\) matches\\s-*$" nil :noerror)
        (setq num-matches (string-to-number (match-string-no-properties 1))))
      (cond ((eq num-matches 1)
             (xref-push-marker-stack vhdl-ext-jump-to-parent-entity-point-marker)
             (next-error)
             (vhdl-ext-navigation-ag-rg-hook-cleanup)
             (message "Jump to only match for [%s] @ %s" entity-name dir))
            ((> num-matches 1)
             (xref-push-marker-stack vhdl-ext-jump-to-parent-entity-point-marker)
             (next-error)
             (message "Showing matches for [%s] @ %s" entity-name dir))
            (t
             (vhdl-ext-navigation-ag-rg-hook-cleanup)
             (message "No matches found")))
      (setq vhdl-ext-jump-to-parent-entity-trigger nil))))


(provide 'vhdl-ext-nav)

;;; vhdl-ext-nav.el ends here
