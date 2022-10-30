;;; Utils
(defun vhdl-ext-point-inside-block-p (block)
  "Return block name if cursor is inside specified BLOCK type."
  (let ((pos (point))
        (re (cond ((eq block 'entity)        "^\\<\\(entity\\)\\>")
                  ((eq block 'architecture)  "^\\<\\(architecture\\)\\>")
                  ((eq block 'function)      "\\<\\(function\\)\\>")
                  ((eq block 'procedure)     "\\<\\(procedure\\)\\>")
                  ((eq block 'component)     "^\\<\\(component\\)\\>")
                  ((eq block 'process)       "^\\<\\(process\\)\\>")
                  ((eq block 'block)         "^\\<\\(block\\)\\>")
                  ((eq block 'package)       "^\\<\\(package\\)\\>")
                  ((eq block 'configuration) "^\\<\\(configuration\\)\\>")
                  ((eq block 'context)       "^\\<\\(context\\)\\>")
                  (t (error "Incorrect block argument"))))
        temp-pos block-beg-point block-end-point block-type block-name)
    (save-match-data
      (save-excursion
        (and (vhdl-re-search-backward re nil t)
             (setq block-type (match-string-no-properties 1))
             (setq temp-pos (point))
             (progn
               (beginning-of-line)
               t)
             (save-excursion
               (vhdl-forward-syntactic-ws (line-end-position))
               (setq block-beg-point (point)))
             ;; TODO: Here I stopped
             (cond ((looking-at vhdl-ext-entity-re)
                    (setq block-name (match-string-no-properties 3))
                    (vhdl-re-search-forward "is" nil t)
                    (goto-char (match-beginning 0))
                    (vhdl-forward-sexp)
                    (backward-word)
                    (setq block-end-point (point)))
                   ((looking-at vhdl-ext-architecture-re)
                    ))
                    )
                   ((or (looking-at vhdl-ext-function-re)
                        (looking-at vhdl-ext-procedure-re))
                    (setq block-name (match-string-no-properties 3)))
                   (t
                    (error "Invalid condition")))
             (goto-char temp-pos)
             (progn
               (vhdl-forward-sexp)
               t)
             (progn
               (backward-word)
               t)
             (setq block-end-point (point)))
        (if (and block-beg-point block-end-point
                 (>= pos block-beg-point)
                 (< pos block-end-point))
            (cons block-type block-name)
          nil)))




;;;; Others
;; TODO: This one shouldn't be needed anymore...
(defun vhdl-ext-electric-return ()
  "Wrapper for RET key to add functionality when there is an AG search buffer.
This will normally happen after calling `vhdl-ext-find-parent-module'"
  (interactive)
  (let* ((ag-buf "*ag search*")
         (ag-win (get-buffer-window ag-buf)))
    (if ag-win
        (delete-window ag-win)
      (vhdl-electric-return))))

;; Keys
;; ("<return>" . larumbe/vhdl-electric-return)
;; ("RET"      . larumbe/vhdl-electric-return)



;;;; Navigation
;; TODO: Rewrite `vhdl-ext-find-parent-entity' so that it resembles that one of verilog-ext
(defun vhdl-ext-find-parent-entity ()
  "Find the places where the current VHDL entity is instantiated in the project.
Fetched from `modi/verilog-find-parent-entity'"
  (interactive)
  (let ((entity-name)
        (entity-instance-pcre)
        (regexp)
        (ag-arguments ag-arguments)) ; Save the global value of `ag-arguments'
    ;; Get entity name
    (save-excursion
      (re-search-backward vhdl-ext-entity-re))
    (setq entity-name (match-string-no-properties 2))
    ;; Reformat regexp to PCRE:
    ;; INFO: Regexp fetched from `vhdl-ext-instance-re', replaced "\\s-" with "[ ]", and dismissing \n to allow for easy elisp to pcre conversion
    ;; Otherwise it was getting a real hell since `rxt-elisp-to-pcre' does not seem to support newlines conversion.
    (setq regexp (concat "^[ ]*\\([a-zA-Z_][a-zA-Z0-9_-]*\\)[ ]*:[ ]*"
                         "\\(\\(component[ ]+\\|configuration[ ]+\\|\\(entity[ ]+\\([a-zA-Z_][a-zA-Z0-9_-]*\\).\\)\\)\\)?"
                         "\\(" entity-name "\\)[ ]*"))
    (setq entity-instance-pcre (rxt-elisp-to-pcre regexp))
    (setq ag-arguments (append ag-arguments '("--vhdl")))
    (xref-push-marker-stack)
    (ag-regexp entity-instance-pcre (projectile-project-root))))



;;;; Which func
