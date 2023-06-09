;;; vhdl-ext-tests-setup-faces.el --- Vhdl-ext tests faces setup  -*- lexical-binding: t -*-

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

;; Setup faces

;;; Code:

(set-face-attribute 'vhdl-ext-font-lock-then-face nil :foreground "dark olive green")
(set-face-attribute 'vhdl-ext-font-lock-punctuation-face nil :foreground "burlywood")
(set-face-attribute 'vhdl-ext-font-lock-operator-face nil :inherit 'vhdl-ext-font-lock-punctuation-face :weight 'extra-bold)
(set-face-attribute 'vhdl-ext-font-lock-brackets-face nil :foreground "goldenrod")
(set-face-attribute 'vhdl-ext-font-lock-parenthesis-face nil :foreground "dark goldenrod")
(set-face-attribute 'vhdl-ext-font-lock-curly-braces-face nil :foreground "DarkGoldenrod2")
(set-face-attribute 'vhdl-ext-font-lock-brackets-content-face nil :foreground "yellow green")
(set-face-attribute 'vhdl-ext-font-lock-port-connection-face nil :foreground "bisque2")
(set-face-attribute 'vhdl-ext-font-lock-entity-face nil :foreground "green1")
(set-face-attribute 'vhdl-ext-font-lock-instance-face nil :foreground "medium spring green")
(set-face-attribute 'vhdl-ext-font-lock-instance-lib-face nil :foreground "gray70")
(set-face-attribute 'vhdl-ext-font-lock-translate-off-face nil :background "gray20" :slant 'italic)

(provide 'vhdl-ext-tests-setup-faces)

;;; vhdl-ext-tests-setup-faces.el ends here
