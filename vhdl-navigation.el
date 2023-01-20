;;; vhdl-navigation.el --- VHDL Navigation  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Gonzalo Larumbe

;; Author: Gonzalo Larumbe <gonzalomlarumbe@gmail.com>
;; URL: https://github.com/gmlarumbe/verilog-ext

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
                 (setq point-instance-end (point))
                 (vhdl-ext-find-entity-instance-bwd)) ; Sets match data
        (setq instance-name (match-string-no-properties 1))
        (setq instance-type (match-string-no-properties 6))
        (setq point-instance-begin (match-beginning 1))
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


(provide 'vhdl-navigation)

;;; vhdl-navigation.el ends here
