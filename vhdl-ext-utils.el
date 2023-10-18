;;; vhdl-ext-utils.el --- Vhdl-ext Utils -*- lexical-binding: t -*-

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

;; Common utils used in different features.

;;; Code:

(require 'vhdl-mode)

;;;; Custom
(defcustom vhdl-ext-file-extension-re "\\.vhdl?\\'"
  "VHDL file extension regexp.
Defaults to .vhd and .vhdl."
  :type 'string
  :group 'vhdl-ext)

(defcustom vhdl-ext-ghdl-extra-args nil
  "Vhdl-ext extra arguments for GHDL processes."
  :type '(repeat string)
  :group 'vhdl-ext)

(defcustom vhdl-ext-cache-enable t
  "If set to non-nil enable use of cache files."
  :type 'boolean
  :group 'vhdl-ext)

(defcustom vhdl-ext-cache-do-compression nil
  "If set to non-nil compress cache files.

Requires having \"gzip\" and \"gunzip\" in the $PATH.

If set to non-nil increases loading time of `vhdl-ext' package but cache
files take up much less disk space."
  :type 'boolean
  :group 'vhdl-ext)

(defcustom vhdl-ext-project-alist nil
  "`vhdl-ext' project alist.

Used for per-project functionality in `vhdl-ext'.

Its elements have the following structure: their car is a string with the name
of the project and their cdr a property list with the following properties:
 - :root - base directory of the project (mandatory)
 - :dirs - directories to search for project files (list of strings)
 - :ignore-dirs - directories to ignore (list of strings)
 - :files - files to be used for the project, keep in order for GHDL
            compilations (list of strings)
 - :ignore-files - files to be ignored for project (list of strings)

GHDL related:
 - :worklib - defaults to \"work\"
 - :workdir - output compilation directory for worklib
 - :lib-search-path - extra directories to look for compiled libraries."
  :type '(repeat
          (list (string :tag "Project")
                (plist :tag "Properties"
                       :options ((:root string)
                                 (:dirs (repeat directory))
                                 (:ignore-dirs (repeat directory))
                                 (:files (repeat file))
                                 (:ignore-files (repeat file))
                                 (:worklib string)
                                 (:workdir directory)
                                 (:lib-search-path (repeat directory))))))
  :group 'vhdl-ext)


;;;; Consts/Vars
;;;;; Regexps
(defconst vhdl-ext-blank-optional-re "[[:blank:]\n]*")
(defconst vhdl-ext-blank-mandatory-re "[[:blank:]\n]+")
(defconst vhdl-ext-identifier-re "[a-zA-Z_][a-zA-Z0-9_-]*")
(defconst vhdl-ext-identifier-sym-re (concat "\\_<" vhdl-ext-identifier-re "\\_>"))
(defconst vhdl-ext-arch-identifier-opt-re (concat "\\(\\s-*(\\s-*" vhdl-ext-identifier-re ")\\s-*\\)?"))
(defconst vhdl-ext-instance-re
  (concat "^\\s-*\\(?1:" vhdl-ext-identifier-re "\\)\\s-*:" vhdl-ext-blank-optional-re ; Instance name
          "\\(?2:\\(?3:component\\s-+\\|configuration\\s-+\\|\\(?4:entity\\s-+\\(?:\\(?5:" vhdl-ext-identifier-re "\\)\\.\\)?\\)\\)\\)?"
          "\\(?6:" vhdl-ext-identifier-re "\\)" vhdl-ext-arch-identifier-opt-re vhdl-ext-blank-optional-re ; Entity name
          "\\(--[^\n]*" vhdl-ext-blank-mandatory-re "\\)*\\(generic\\|port\\)\\s-+map\\>"))
(defconst vhdl-ext-entity-re "^\\s-*\\(entity\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-function-re "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)")
(defconst vhdl-ext-procedure-re "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)procedure\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)")
(defconst vhdl-ext-component-re "^\\s-*\\(component\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-process-re "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)")
(defconst vhdl-ext-block-re "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(block\\)")
(defconst vhdl-ext-package-re "^\\s-*\\(package\\( body\\|\\)\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-configuration-re "^\\s-*\\(configuration\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)")
(defconst vhdl-ext-architecture-re "^\\s-*\\(?1:architecture\\)\\s-+\\(?2:\\(?3:\\(?:\\w\\|\\s_\\)+\\)\\s-+of\\s-+\\(?4:\\(?:\\w\\|\\s_\\)+\\)\\)")
(defconst vhdl-ext-context-re "^\\s-*\\(context\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)")

;;;;; LSP
(defconst vhdl-ext-lsp-available-servers
  '((ve-hdl-checker . ("hdl_checker" "--lsp"))
    (ve-rust-hdl    . "vhdl_ls")
    (ve-ghdl-ls     . "ghdl-ls")
    (ve-vhdl-tool   . ("vhdl-tool" "lsp")))
  "Vhdl-ext available LSP servers.")
(defconst vhdl-ext-lsp-server-ids (mapcar #'car vhdl-ext-lsp-available-servers))

;;;;; Misc
(defvar vhdl-ext-buffer-list nil)
(defvar vhdl-ext-dir-list nil)
(defvar vhdl-ext-file-list nil)

(defconst vhdl-ext-cache-dir (file-name-concat user-emacs-directory "vhdl-ext")
  "The directory where vhdl-ext cache files will be placed at.")


;;;; Macros
(defmacro vhdl-ext-with-disabled-messages (&rest body)
  "Execute BODY without displaying messages in the echo area."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t))
     ,@body))

(defmacro vhdl-ext-with-no-hooks (&rest body)
  "Execute BODY without running any additional VHDL hooks."
  (declare (indent 0) (debug t))
  `(let ((prog-mode-hook nil)
         (vhdl-mode-hook '(vhdl-ext-mode))
         (vhdl-ts-mode-hook nil))
     ,@body))

(defmacro vhdl-ext-proj-setcdr (proj alist value)
  "Set cdr of ALIST for current PROJ to VALUE.

If current VALUE is nil remove its key from ALIST.

ALIST keys are strings that define projects in `vhdl-ext-project-alist'."
  (declare (indent 0) (debug t))
  `(setf (alist-get ,proj ,alist nil 'remove 'string=) ,value))

;;;; Wrappers
(defun vhdl-ext-forward-sexp (&optional count)
  "Move forward one SEXP.
With prefix arg, move COUNT sexps."
  (interactive "P")
  (let* ((symbol-raw (thing-at-point 'symbol :no-props))
         (symbol (when symbol-raw
                   (downcase symbol-raw)))
         (bounds (bounds-of-thing-at-point 'symbol)))
    (cond (;; entity, architecture, package, configuration, context
           (member symbol '("entity" "architecture" "configuration" "context"))
           (vhdl-re-search-forward "\\_<is\\_>" nil t)
           (goto-char (match-beginning 0))
           (vhdl-forward-sexp count)
           (when (looking-at (concat "\\s-+\\_<" symbol "\\_>"))
             (forward-word)))
          ;; function, procedure
          ((member symbol '("function" "procedure"))
           (let ((pos (point)))
             (if (save-excursion (setq pos (vhdl-end-of-statement))
                                 (eq (preceding-char) ?\;)) ; Function declaration in package declaration (not body)
                 (goto-char pos)
               (vhdl-re-search-forward "\\_<is\\_>" nil t)
               (goto-char (match-beginning 0))
               (vhdl-forward-sexp count)
               (when (looking-at (concat "\\s-+\\_<" symbol "\\_>"))
                 (forward-word)))))
          ;; component, process
          ((member symbol '("component" "process" "generate" "loop"))
           (goto-char (car bounds))
           (vhdl-forward-sexp count)
           (when (looking-at (concat "\\s-+\\_<" symbol "\\_>"))
             (forward-word)))
          ;; if then/else/elsif
          ((member symbol '("then" "else" "elsif"))
           (goto-char (car bounds))
           (vhdl-forward-sexp count)
           (when (looking-at (concat "\\s-+\\_<if\\_>"))
             (forward-word)))
          ;; Package/package body
          ((member symbol '("package"))
           (vhdl-re-search-forward "\\_<is\\_>" nil t)
           (goto-char (match-beginning 0))
           (vhdl-forward-sexp count)
           (when (looking-at (concat "\\s-+\\_<" symbol "\\_>\\(\\s-+body\\)?"))
             (goto-char (match-end 0))))
          ;; Fallback
          (t
           (vhdl-forward-sexp count)))))

(defun vhdl-ext-backward-sexp (&optional count)
  "Move backward one SEXP.
With prefix arg, move COUNT sexps.

Algorithm takes into account that keywords component, generate and process
cannot be ommitted after an end."
  (interactive "P")
  (let* ((symbol-raw (thing-at-point 'symbol :no-props))
         (symbol (when symbol-raw
                   (downcase symbol-raw)))
         (bounds (bounds-of-thing-at-point 'symbol)))
    (cond (;; end
           (member symbol '("end"))
           (goto-char (cdr bounds))
           (vhdl-backward-sexp count)
           (cond ((looking-at "\\_<is\\_>")
                  (vhdl-re-search-backward "\\_<\\(entity\\|function\\|procedure\\|component\\|package\\|context\\|configuration\\)\\_>" nil t))
                 ((looking-at "\\_<begin\\_>")
                  (vhdl-re-search-backward "\\_<\\(?1:end\\|architecture\\|function\\|procedure\\|process\\)\\_>" nil t)
                  (while (or (string= "end" (match-string-no-properties 1))
                             (looking-back "\\_<\\(?1:end\\)\\_>\\s-+" (line-beginning-position)))
                    (goto-char (match-end 1))
                    (vhdl-ext-backward-sexp count)
                    (vhdl-re-search-backward "\\_<\\(end\\|architecture\\|function\\|procedure\\|process\\)\\_>" nil t)))))
          ;; entity, architecture, ,function, procedure, package, process, if, context, configuration
          ((member symbol '("entity" "architecture" "function" "procedure" "package" "process" "if" "context" "configuration"))
           (vhdl-re-search-backward "\\_<end\\_>" nil t)
           (goto-char (match-end 0))
           (vhdl-backward-sexp count)
           (vhdl-re-search-backward (concat "\\_<" symbol "\\_>") nil t))
          ;; component, generate, loop
          ((member symbol '("component" "generate" "loop"))
           (vhdl-re-search-backward "\\_<end\\_>" nil t)
           (goto-char (match-end 0))
           (vhdl-backward-sexp count))
          ;; if then/else/elsif
          ((member symbol '("else" "elsif"))
           (goto-char (car bounds))
           (vhdl-backward-sexp count))
          ;; package body
          ((member symbol '("body"))
           (vhdl-re-search-backward "\\(?1:end\\)\\s-+package" nil t)
           (goto-char (match-end 1))
           (vhdl-backward-sexp count)
           (vhdl-re-search-backward (concat "package\\s-+body") nil t))
          ;; Fallback
          (t
           (vhdl-backward-sexp count)))))

(defun vhdl-ext-get-standard ()
  "Get current standard as a string from `vhdl-standard'."
  (let ((std (car vhdl-standard)))
    (if (equal std 8)
        (format "0%s" std)
      (format "%s" std))))

;;;; String/regexp
(defun vhdl-ext-replace-regexp (regexp to-string start end)
  "Wrapper function for programatic use of `replace-regexp'.
Replace REGEXP with TO-STRING from START to END."
  (let* ((marker (make-marker))
         (endpos (when end (set-marker marker end))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp endpos t)
        (replace-match to-string)))))

(defun vhdl-ext-replace-regexp-whole-buffer (regexp to-string)
  "Replace REGEXP with TO-STRING on whole `current-buffer'."
  (vhdl-ext-replace-regexp regexp to-string (point-min) nil))

(defun vhdl-ext-replace-string (string to-string start end &optional fixedcase)
  "Wrapper function for programatic use of `replace-string'.
Replace STRING with TO-STRING from START to END.

If optional arg FIXEDCASE is non-nil, do not alter the case of
the replacement text (see `replace-match' for more info)."
  (let* ((marker (make-marker))
         (endpos (when end (set-marker marker end))))
    (save-excursion
      (goto-char start)
      (while (search-forward string endpos t)
        (replace-match to-string fixedcase)))))

;;;; Dirs/files
(defun vhdl-ext-dir-files (dir &optional recursive follow-symlinks ignore-dirs)
  "Find VHDL files on DIR.

If RECURSIVE is non-nil find files recursively.

Follow symlinks if optional argument FOLLOW-SYMLINKS is non-nil.

Discard non-regular files (e.g. Emacs temporary non-saved buffer files like
symlink #.test.vhd).

Optional arg IGNORE-DIRS specifies which directories should be excluded from
search."
  (let* ((files (if recursive
                    (directory-files-recursively dir vhdl-ext-file-extension-re nil nil follow-symlinks)
                  (directory-files dir t vhdl-ext-file-extension-re)))
         (files-after-ignored (seq-filter (lambda (file)
                                            ;; Each file checks if it has its prefix in the list of ignored directories
                                            (let (ignore-file)
                                              (dolist (dir ignore-dirs)
                                                (when (string-prefix-p (expand-file-name dir) (expand-file-name file))
                                                  (setq ignore-file t)))
                                              (not ignore-file)))
                                          files))
         (files-regular (seq-filter #'file-regular-p files-after-ignored)))
    files-regular))

(defun vhdl-ext-dirs-files (dirs &optional follow-symlinks ignore-dirs)
  "Find VHDL files recursively on DIRS.
DIRS is a list of directory strings.

Follow symlinks if optional argument FOLLOW-SYMLINKS is non-nil.

Optional arg IGNORE-DIRS specifies which directories should be excluded from
search."
  (let (files)
    (dolist (dir dirs)
      (push (vhdl-ext-dir-files dir follow-symlinks ignore-dirs) files))
    (when files
      (flatten-tree files))))

(defun vhdl-ext-filelist-from-file (file)
  "Return filelist from FILE as a list of strings."
  (with-temp-buffer
    (insert-file-contents file)
    (delete "" (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))))

(defun vhdl-ext-file-from-filefile (filelist out-file)
  "Write FILELIST to OUT-FILE as one line per file."
  (with-temp-file out-file
    (insert (mapconcat #'identity filelist "\n"))))

(defun vhdl-ext-expand-file-list (file-list &optional rel-dir)
  "Expand files in FILE-LIST.

Expand with respect to REL-DIR if non-nil."
  (mapcar (lambda (file)
            (expand-file-name file rel-dir))
          file-list))

;;;; File entities
(defun vhdl-ext-scan-buffer-entities-and-lines ()
  "Find entities and their definition files in current buffer.
Return list with found entities and their line number or nil if not found."
  (let (entities-and-files)
    (save-excursion
      (goto-char (point-min))
      (while (vhdl-re-search-forward vhdl-ext-entity-re nil t)
        (push `(,(match-string-no-properties 2)
                ,(line-number-at-pos))
              entities-and-files)))
    (nreverse (delete-dups entities-and-files))))

(defun vhdl-ext-scan-buffer-entities ()
  "Find entities in current buffer.
Return list with found entities or nil if not found."
  (mapcar #'car (vhdl-ext-scan-buffer-entities-and-lines)))

(defun vhdl-ext-read-file-entities (&optional file)
  "Find entities in current buffer.
Find entities in FILE if optional arg is non-nil.
Return list with found entities or nil if not found."
  (let ((buf (if file
                 (get-file-buffer file)
               (current-buffer)))
        (debug nil))
    (if buf
        (with-current-buffer buf
          (vhdl-ext-scan-buffer-entities))
      ;; If FILE buffer is not being visited, use a temporary buffer
      (with-temp-buffer
        (when debug
          (clone-indirect-buffer-other-window "*debug*" t))
        (insert-file-contents file)
        (vhdl-ext-scan-buffer-entities)))))

(defun vhdl-ext-select-file-entity (&optional file)
  "Select file entity from FILE.
If only one entity was found return it as a string.
If more than one entity was found, select between available ones.
Return nil if no entity was found."
  (let ((entities (vhdl-ext-read-file-entities file)))
    (if (cdr entities)
        (completing-read "Select entity: " entities)
      (car entities))))

;;;; Block at point / point inside block
(defconst vhdl-ext-block-at-point-re
  (eval-when-compile
    (regexp-opt
     '("entity" "architecture" "package" "configuration" "context"
       "function" "procedure" "component" "process" "generate")
     'symbols)))

(defun vhdl-ext-block-at-point ()
  "Return current block at point type and name."
  (let ((pos (point))
        beg-pos end-pos found block name)
    (save-excursion
      (while (and (not found)
                  (vhdl-re-search-backward vhdl-ext-block-at-point-re nil t))
        (when (looking-back "\\_<end\\_>\\s-+" (line-beginning-position))
          (vhdl-ext-backward-sexp)
          (vhdl-re-search-backward vhdl-ext-block-at-point-re nil t))
        (setq block (thing-at-point 'symbol :no-props))
        (setq beg-pos (point))
        (cond ((string= block "process")
               (when (looking-back "^\\s-*\\(?1:[a-zA-Z0-9_-]+\\)\\s-*:\\s-*" (line-beginning-position))
                 (setq name (match-string-no-properties 1))))
              ((string= block "generate")
               (when (save-excursion (vhdl-re-search-backward "^\\s-*\\(?1:[a-zA-Z0-9_-]+\\)\\s-*:\\s-*" (line-beginning-position) :noerror))
                 (setq name (match-string-no-properties 1))))
              ((string= block "package")
               (when (save-excursion (vhdl-re-search-forward "package\\s-+\\(?1:body\\s-+\\)?\\(?2:[a-zA-Z0-9_-]+\\)" (line-end-position) :noerror))
                 (setq name (match-string-no-properties 2))))
              (t
               (save-excursion
                 (forward-word)
                 (vhdl-forward-syntactic-ws)
                 (setq name (thing-at-point 'symbol :no-props)))))
        (save-excursion
          (vhdl-ext-forward-sexp)
          (setq end-pos (point)))
        (when (and (>= pos beg-pos)
                   (<= pos end-pos))
          (setq found t))))
    (when found
      `((type      . ,block)
        (name      . ,name)
        (beg-point . ,beg-pos)
        (end-point . ,end-pos)))))

(defun vhdl-ext-point-inside-block (block)
  "Return block name if cursor is inside specified BLOCK type."
  (let ((pos (point))
        (re (cond ((eq block 'entity)        "\\<\\(entity\\)\\>")
                  ((eq block 'architecture)  "\\<\\(architecture\\)\\>")
                  ((eq block 'package)       "\\<\\(package\\)\\>")
                  ((eq block 'configuration) "\\<\\(configuration\\)\\>")
                  ((eq block 'context)       "\\<\\(context\\)\\>")
                  ((eq block 'function)      "\\<\\(function\\)\\>")
                  ((eq block 'procedure)     "\\<\\(procedure\\)\\>")
                  ((eq block 'component)     "\\<\\(component\\)\\>")
                  ((eq block 'process)       "\\<\\(process\\)\\>")
                  ((eq block 'generate)      "\\<\\(generate\\)\\>")
                  (t (error "Incorrect block argument"))))
        block-beg-point block-end-point)
    (save-match-data
      (save-excursion
        (when (and (vhdl-re-search-backward re nil t)
                   (setq block-beg-point (point))
                   (vhdl-ext-forward-sexp)
                   (setq block-end-point (point))
                   (>= pos block-beg-point)
                   (< pos block-end-point))
          `((type      . ,(symbol-name block))
            (beg-point . ,block-beg-point)
            (end-point . ,block-end-point)))))))

(defun vhdl-ext-inside-if-else ()
  "Return non-nil if point is inside an if-else block."
  (let (beg-pos end-pos)
    (vhdl-prepare-search-2
     (save-excursion
       (when (vhdl-re-search-backward "\\_<\\(then\\|else\\)\\_>" nil t)
         (setq beg-pos (point))
         (vhdl-forward-sexp)
         (setq end-pos (point)))))
    (when (and beg-pos end-pos
               (> (point) beg-pos)
               (< (point) end-pos))
      (cons beg-pos end-pos))))

;;;; Buffers/hooks
(defun vhdl-ext-update-buffer-file-and-dir-list ()
  "Update `vhdl-mode' list of open buffers, files, and dir lists."
  (let (vhdl-buffers vhdl-dirs vhdl-files)
    (dolist (buf (buffer-list (current-buffer)))
      (with-current-buffer buf
        (when (or (eq major-mode 'vhdl-mode)
                  (eq major-mode 'vhdl-ts-mode))
          (push buf vhdl-buffers)
          (unless (member default-directory vhdl-dirs)
            (push default-directory vhdl-dirs))
          (when (and buffer-file-name
                     (string-match vhdl-ext-file-extension-re (concat "." (file-name-extension buffer-file-name))))
            (push buffer-file-name vhdl-files)))))
    (setq vhdl-ext-buffer-list vhdl-buffers)
    (setq vhdl-ext-dir-list vhdl-dirs)
    (setq vhdl-ext-file-list vhdl-files)))

(defun vhdl-ext-kill-buffer-hook ()
  "VHDL hook to run when killing a buffer."
  (setq vhdl-ext-buffer-list (remove (current-buffer) vhdl-ext-buffer-list)))


;;;; Project
(defun vhdl-ext-buffer-proj ()
  "Return current buffer project if it belongs to `vhdl-ext-project-alist'."
  (catch 'project
    (when (and buffer-file-name vhdl-ext-project-alist)
      (dolist (proj vhdl-ext-project-alist)
        (when (string-prefix-p (expand-file-name (plist-get (cdr proj) :root))
                               (expand-file-name buffer-file-name))
          (throw 'project (car proj)))))))

(defun vhdl-ext-buffer-proj-root (&optional project)
  "Return current buffer PROJECT root if it belongs to `vhdl-ext-project-alist'."
  (let ((proj (or project (vhdl-ext-buffer-proj))))
    (when proj
      (expand-file-name (plist-get (vhdl-aget vhdl-ext-project-alist proj) :root)))))

(defun vhdl-ext-proj-workdir (&optional project)
  "Return working library dir according to PROJECT of current buffer dir."
  (let* ((proj (or project (vhdl-ext-buffer-proj)))
         (root (vhdl-ext-buffer-proj-root proj))
         (workdir (plist-get (vhdl-aget vhdl-ext-project-alist proj) :workdir)))
    (when (and root workdir)
      (expand-file-name workdir root))))

(defun vhdl-ext-proj-worklib (&optional project)
  "Return the working library name of the current directory PROJECT.

Return `vhdl-default-library' (normally \"work\") if no project is defined.

See `vhdl-work-library'."
  (let ((proj (or project (vhdl-ext-buffer-proj))))
    (or (plist-get (vhdl-aget vhdl-ext-project-alist proj) :worklib)
        vhdl-default-library)))

(defun vhdl-ext-proj-files (&optional project)
  "Return list of files for PROJECT.

These depend on the value of property list of `vhdl-ext-project-alist'.
 :dirs - list of strings with optional \"-r\" to find files recursively
 :ignore-dirs - list of strings of dirs to be ignored
 :files - list of strings with the files
 :ignore-files - list of strings of ignored files"
  (let* ((proj (or project (vhdl-ext-buffer-proj)))
         (proj-plist (vhdl-aget vhdl-ext-project-alist proj))
         (proj-root (when proj-plist (expand-file-name (plist-get proj-plist :root))))
         (proj-dirs (plist-get proj-plist :dirs))
         (proj-ignore-dirs (plist-get proj-plist :ignore-dirs))
         (proj-files (plist-get proj-plist :files))
         (proj-ignore-files (plist-get proj-plist :ignore-files))
         files-dirs files-all)
    ;; Basic checks
    (unless proj
      (user-error "Not in a VHDL project buffer, check `vhdl-ext-project-alist'"))
    (unless proj-root
      (user-error "Project root not set for project %s" proj))
    ;; Expand filenames (except for proj-dirs since they need to parse the -r recursive flag)
    (when proj-ignore-dirs
      (setq proj-ignore-dirs (vhdl-ext-expand-file-list proj-ignore-dirs proj-root)))
    (when proj-files
      (setq proj-files (vhdl-ext-expand-file-list proj-files proj-root)))
    (when proj-ignore-files
      (setq proj-ignore-files (vhdl-ext-expand-file-list proj-ignore-files proj-root)))
    ;; Analyze directories
    (when proj-dirs
      (mapc (lambda (dir)
              (if (string= "-r" (car (split-string dir)))
                  (setq files-dirs (append files-dirs (vhdl-ext-dir-files (expand-file-name (cadr (split-string dir)) proj-root) :recursive :follow-symlinks proj-ignore-dirs)))
                (setq files-dirs (append files-dirs (vhdl-ext-dir-files (expand-file-name dir proj-root) nil :follow-symlinks proj-ignore-dirs)))))
            proj-dirs))
    ;; If no dirs or files are specified get recursively all files in root
    (if (and (not proj-files)
             (not proj-dirs))
        (setq files-all (vhdl-ext-dir-files proj-root :recursive :follow-symlinks proj-ignore-dirs))
      (setq files-dirs (delete-dups files-dirs))
      (setq files-all (append files-dirs proj-files)))
    ;; Merge and filter
    (seq-filter (lambda (file)
                  (not (member file proj-ignore-files)))
                files-all)))

;;;; Cache
(defun vhdl-ext-serialize (data filename)
  "Serialize DATA to FILENAME.

Compress cache files if gzip is available."
  (let ((dir (file-name-directory filename))
        (gzip-buf "*vhdl-ext-serialize-compress*"))
    (unless (file-exists-p dir)
      (make-directory dir :parents))
    (if (not (file-writable-p filename))
        (message "Vhdl-ext cache '%s' not writeable" filename)
      (with-temp-file filename
        (insert (let (print-length) (prin1-to-string data))))
      (when (and vhdl-ext-cache-do-compression
                 (executable-find "gzip"))
        ;; Synchronous processing, as it might be run just before exiting Emacs
        (unless (eq 0 (call-process-shell-command (format "gzip -9f %s" filename) nil gzip-buf t))
          (error "Error compressing %s" filename))))))

(defun vhdl-ext-unserialize (filename)
  "Read data serialized by `vhdl-ext-serialize' from FILENAME."
  (let* ((compressed-filename (concat filename ".gz"))
         (temp-filename (make-temp-file (concat (file-name-nondirectory filename) "-")))
         (gzip-buf "*vhdl-ext-serialize-decompress*")
         (decompress-cmd (format "gunzip -c %s > %s" compressed-filename temp-filename))) ; Keep original compressed file
    (with-demoted-errors
        "Error during file deserialization: %S"
      ;; INFO: Tried using zlib with `zlib-available-p' and
      ;; `zlib-decompress-region', which are faster.  However, these require the
      ;; buffer to be unibyte (i.e. have only ASCII characters). That could not
      ;; be the case for paths with non-latin characters
      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Disabling-Multibyte.html
      (if (and vhdl-ext-cache-do-compression
               (executable-find "gunzip")
               (file-exists-p compressed-filename))
          (progn
            ;; External gunzip program: This doesn't need to be asynchronous as it will only be done during initial setup
            (unless (eq 0 (call-process-shell-command decompress-cmd nil gzip-buf t))
              (error "Error decompressing %s" compressed-filename))
            (with-temp-buffer
              (insert-file-contents temp-filename)
              (delete-file temp-filename)
              (read (buffer-string))))
        ;; Do not decompress
        (when (file-exists-p filename)
          (with-temp-buffer
            (insert-file-contents filename)
            (read (buffer-string))))))))

;;;; GHDL
(defun vhdl-ext-ghdl-proj-args ()
  "Return arguments of GHDL command for current buffer project."
  (let* ((proj (vhdl-ext-buffer-proj))
         (workdir (vhdl-ext-proj-workdir proj))
         (args (mapconcat #'identity
                          `("-fno-color-diagnostics"
                            ,(concat "--std=" (vhdl-ext-get-standard))
                            ,(concat "--workdir=" workdir)
                            ,(concat "--work=" (vhdl-ext-proj-worklib proj)))
                          " "))
         (extra-args (when vhdl-ext-ghdl-extra-args
                       (mapconcat #'identity vhdl-ext-ghdl-extra-args " ")))
         (dirs (mapcar #'expand-file-name (plist-get (vhdl-aget vhdl-ext-project-alist proj) :lib-search-path)))
         (dirs-args (mapconcat (lambda (dir) (concat "-P" dir))
                               dirs
                               " ")))
    (mapconcat #'identity `(,args ,extra-args ,dirs-args) " ")))


;;;; Overrides
;; TODO: To be fixed @ emacs/main
(defun vhdl-ext-corresponding-begin (&optional lim)
  "If the word at the current position corresponds to an \"end\"
keyword, then return a vector containing enough information to find
the corresponding \"begin\" keyword, else return nil.  The keyword to
search backward for is aref 0.  The column in which the keyword must
appear is aref 1 or nil if any column is suitable.  The supplementary
keyword to search forward for is aref 2 or nil if this is not
required.  If aref 3 is t, then the \"begin\" keyword may be found in
the middle of a statement.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain an \"end\" keyword."
  (save-excursion
    (let (pos)
      (if (and (looking-at vhdl-end-fwd-re)
               (not (vhdl-in-literal))
               (vhdl-end-p lim))
          (if (looking-at "el")
              ;; "else", "elsif":
              (vector "if\\|elsif" (vhdl-first-word (point)) "then\\|use" nil)
            ;; "end ...":
            (setq pos (point))
            (forward-sexp)
            (skip-chars-forward " \t\n\r\f")
            (cond
             ;; "end if":
             ((looking-at "if\\b[^_]")
              (vector "else\\|elsif\\|if"
                      (vhdl-first-word pos)
                      "else\\|then\\|use" nil))
             ;; "end component":
             ;; DANGER: Overrid here
             ((looking-at "\\(component\\)\\b[^_]")
              (vector (buffer-substring (match-beginning 1)
                                        (match-end 1))
                      (vhdl-first-word pos)
                      nil nil))
             ;; End of DANGER: Overrid here
             ;; "end units", "end record", "end protected":
             ((looking-at "\\(units\\|record\\|protected\\(\\s-+body\\)?\\)\\b[^_]")
              (vector (buffer-substring (match-beginning 1)
                                        (match-end 1))
                      (vhdl-first-word pos)
                      nil t))
             ;; "end block", "end process", "end procedural":
             ((looking-at "\\(block\\|process\\|procedural\\)\\b[^_]")
              (vector "begin" (vhdl-first-word pos) nil nil))
             ;; "end case":
             ((looking-at "case\\b[^_]")
              (vector "case" (vhdl-first-word pos) "is" nil))
             ;; "end generate":
             ((looking-at "generate\\b[^_]")
              (vector "generate\\|for\\|if"
                      (vhdl-first-word pos)
                      "generate" nil))
             ;; "end loop":
             ((looking-at "loop\\b[^_]")
              (vector "loop\\|while\\|for"
                      (vhdl-first-word pos)
                      "loop" nil))
             ;; "end for" (inside configuration declaration):
             ((looking-at "for\\b[^_]")
              (vector "for" (vhdl-first-word pos) nil nil))
             ;; "end [id]":
             (t
              (vector "begin\\|architecture\\|configuration\\|context\\|entity\\|package\\|procedure\\|function"
                      (vhdl-first-word pos)
                      ;; return an alist of (statement . keyword) mappings
                      '(
                        ;; "begin ... end [id]":
                        ("begin"          . nil)
                        ;; "architecture ... is ... begin ... end [id]":
                        ("architecture"   . "is")
                        ;; "configuration ... is ... end [id]":
                        ("configuration"  . "is")
                        ;; "context ... is ... end [id]":
                        ("context"        . "is")
                        ;; "entity ... is ... end [id]":
                        ("entity"         . "is")
                        ;; "package ... is ... end [id]":
                        ("package"        . "is")
                        ;; "procedure ... is ... begin ... end [id]":
                        ("procedure"      . "is")
                        ;; "function ... is ... begin ... end [id]":
                        ("function"       . "is")
                        )
                      nil))
             ))) ; "end ..."
      )))

(advice-add 'vhdl-corresponding-begin :override #'vhdl-ext-corresponding-begin)


(provide 'vhdl-ext-utils)

;;; vhdl-ext-utils.el ends here
