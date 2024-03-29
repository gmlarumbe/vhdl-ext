;;; vhdl-ext-ports.el --- Vhdl-ext ports  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Gonzalo Larumbe

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

;; Port connections utils

;;; Code:

(require 'vhdl-ext-utils)


(defun vhdl-ext-ports-toggle-connect (&optional force-connect)
  "Toggle connect/disconnect port at current line.

Return non-nil if a port regex was found on current line.

If called with universal arg, FORCE-CONNECT will force connection
of current port instead of toggling."
  (interactive "P")
  (let* ((re (concat "\\(?1:^\\s-*\\)\\(?2:" vhdl-ext-identifier-re "\\)\\(?3:\\s-*=>\\s-*\\)\\(?4:[^,;\n]*\\)\\(?5:[,\n]\\)"))
         ;; INFO: With this regexp, only ports that end in newline or comma will be detected. That means that the following
         ;; regexps won't be considered as a port (last port without space after closing parenthesis:
         ;;  Port2 => Port2);
         ;;  Port2 => );
         port-found port conn sig)
    (save-excursion
      (beginning-of-line)
      (if (looking-at re)
          (progn
            (setq port-found t)
            (setq port (match-string-no-properties 2))
            (setq conn (match-string-no-properties 4))
            (if (or (string= conn "") force-connect) ; Disconnected or forced connection
                (progn ; Connect
                  (setq sig (read-string (concat "Connect [" port "] to: ") port))
                  (looking-at re) ; Needed before `replace-match' to avoid buggy situations
                  (replace-match (concat "\\1\\2\\3" sig "\\5") t))
              ;; Else disconnect
              (replace-match "\\1\\2\\3\\5" t)))
        ;; No port found
        (message "No port detected at current line")))
    (when port-found
      (forward-line 1))))

(defun vhdl-ext-ports-connect-recursively ()
  "Connect ports of current instance recursively.
Ask for connection of ports until no port is found at current line."
  (interactive)
  (while (vhdl-ext-ports-toggle-connect :force-connect)
    (vhdl-ext-ports-toggle-connect :force-connect)))


(provide 'vhdl-ext-ports)

;;; vhdl-ext-ports.el ends here
