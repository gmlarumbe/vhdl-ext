;;; vhdl-ext-hierarchy.el --- Vhdl-ext Hierarchy  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Gonzalo Larumbe

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

;; Utils for hierarchy extraction and navigation

;;; Code:

(require 'outline)
(require 'hierarchy)
(require 'tree-widget)
(require 'async)
(require 'vhdl-ext-nav)

(defgroup vhdl-ext-hierarchy nil
  "Vhdl-ext hierarchy."
  :group 'vhdl-ext)

(defcustom vhdl-ext-hierarchy-backend (if (and (treesit-available-p) (treesit-language-available-p 'vhdl))
                                          'tree-sitter
                                        'builtin)
  "Vhdl-ext hierarchy extraction backend."
  :type '(choice (const :tag "GHDL"        ghdl)
                 (const :tag "Tree-sitter" tree-sitter)
                 (const :tag "Built-in"    builtin))
  :group 'vhdl-ext-hierarchy)

(defcustom vhdl-ext-hierarchy-frontend 'hierarchy
  "Vhdl-ext hierarchy display and navigation frontend."
  :type '(choice (const :tag "Hierarchy" hierarchy)
                 (const :tag "Outline"   outline))
  :group 'vhdl-ext-hierarchy)

(defcustom vhdl-ext-hierarchy-twidget-init-expand nil
  "Set to non-nil to initially expand the hierarchy using hierarchy.el frontend."
  :group 'vhdl-ext-hierarchy
  :type 'boolean)


;;;; Utils
(defvar vhdl-ext-hierarchy-entity-alist nil
  "Per project entity alist.")

(defconst vhdl-ext-hierarchy-async-inject-variables-re
  (eval-when-compile
    (regexp-opt '("load-path"
                  "buffer-file-name"
                  "default-directory"
                  "vhdl-ext-feature-list"
                  "vhdl-ext-project-alist"
                  "vhdl-ext-hierarchy-backend")
                'symbols)))

;;;;; hierarchy.el
(defconst vhdl-ext-hierarchy-entity-cache-file (file-name-concat vhdl-ext-cache-dir "entity")
  "The file where Vhdl-ext entities will be written to.
Used to navigate definitions with `vhdl-ext-hierarchy-twidget-nav-open'.")

(defconst vhdl-ext-hierarchy-internal-cache-file (file-name-concat vhdl-ext-cache-dir "hierarchy-builtin")
  "The file where Vhdl-ext builtin/tree-sitter hierarchies will be written to.")

(defvar vhdl-ext-hierarchy-internal-alist nil
  "Per project flat hierarchy alist.
Used by builtin and tree-sitter backends.")

(defvar vhdl-ext-hierarchy-current-flat-hier nil
  "Current flat hierarchy.

Used by `vhdl-ext-hierarchy-extract--internal',
`vhdl-ext-hierarchy-ghdl-extract' and their subroutines.
Needed since `vhdl-ext-hierarchy-extract--childrenfn' can only
have one argument (item).")


(defun vhdl-ext-hierarchy--get-node-leaf (node)
  "Return leaf name of hierarchical reference NODE.
E.g: return \"leaf\" for \"top.block.subblock.leaf\"."
  (car (last (split-string node "\\."))))

(defun vhdl-ext-hierarchy--get-node-prefix (node)
  "Return prefix name of hierarchical reference NODE.
E.g: return \"top.block.subblock\" for \"top.block.subblock.leaf\"."
  (let ((prefix (string-join (butlast (split-string node "\\.")) ".")))
    (unless (string= prefix "")
      prefix)))

(defun vhdl-ext-hierarchy-extract--childrenfn (item)
  "Childrenfn for `hierarchy'.
Arg ITEM are hierarchy nodes."
  (let* ((prefix (vhdl-ext-hierarchy--get-node-prefix item))
         (leaf (vhdl-ext-hierarchy--get-node-leaf item))
         (children (cdr (assoc-string (car (split-string leaf ":")) vhdl-ext-hierarchy-current-flat-hier t))))
    (mapcar (lambda (child) (concat (when prefix (concat prefix ".")) leaf "." child)) children)))

(defun vhdl-ext-hierarchy-extract--construct-node (node hierarchy)
  "Recursively build HIERARCHY for NODE using childrenfn."
  (let ((children (mapcar (lambda (child)
                            (concat node "." child))
                          (cdr (assoc-string (vhdl-ext-hierarchy--get-node-leaf node) vhdl-ext-hierarchy-current-flat-hier t)))))
    (when children
      (hierarchy-add-tree hierarchy node nil #'vhdl-ext-hierarchy-extract--childrenfn)
      (dolist (child children)
        (vhdl-ext-hierarchy-extract--construct-node child hierarchy)))
    hierarchy))

(defun vhdl-ext-hierarchy-extract--internal (entity)
  "Construct hierarchy struct for ENTITY.

Entities and instances will be analyzed from corresponding entry in
`vhdl-ext-hierarchy-current-flat-hier'.  These entries will have an associated
project present `vhdl-ext-project-alist' and will be of the form:
 \(entity instance1:NAME1 instance2:NAME2 ...\).

With current prefix, force refreshing of hierarchy database for active project.

Return populated `hierarchy' struct."
  (let* ((proj (vhdl-ext-buffer-proj))
         (hierarchy-alist (if current-prefix-arg
                              nil
                            (vhdl-aget vhdl-ext-hierarchy-internal-alist proj))))
    ;; Error checking
    (unless hierarchy-alist
      (cond (current-prefix-arg
             (message "Forcing refresh of hierarchy database for [%s]" proj)
             (vhdl-ext-hierarchy-parse)
             (setq hierarchy-alist (vhdl-aget vhdl-ext-hierarchy-internal-alist proj)))
            ((y-or-n-p (format "Empty hierarchy database for [%s].  Run `vhdl-ext-hierarchy-parse'?" proj))
             (vhdl-ext-hierarchy-parse)
             (setq hierarchy-alist (vhdl-aget vhdl-ext-hierarchy-internal-alist proj)))
            (t
             (user-error "Aborting"))))
    (unless (assoc-string entity hierarchy-alist t)
      (user-error "Could not find %s in the flat-hierarchy for project [%s]" entity proj))
    (unless (cdr (assoc-string entity hierarchy-alist t))
      (user-error "Current entity has no instances"))
    ;; Extract hierarchy
    (setq vhdl-ext-hierarchy-current-flat-hier hierarchy-alist)
    (vhdl-ext-hierarchy-extract--construct-node entity (hierarchy-new))))

;;;;; Frontend format conversion
(defun vhdl-ext-hierarchy--convert-struct-to-string (hierarchy-struct)
  "Convert HIERARCHY-STRUCT to a string.
Used to convert hierarchy formats for displaying on different frontends."
  (let ((offset-blank-spaces 2) ; Intended to be used by outline, which assumes an input ...
        (unicode-spc 32)        ; ... with an offset of two indent spaces
        (debug nil))
    (unless (hierarchy-p hierarchy-struct)
      (error "Wrong type for hierarchy-struct"))
    (with-temp-buffer
      (when debug
        (clone-indirect-buffer-other-window "*debug*" t))
      (hierarchy-print hierarchy-struct (lambda (node) (vhdl-ext-hierarchy--get-node-leaf node)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (insert-char unicode-spc offset-blank-spaces)
          (forward-line)))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;;; Entity-alist
(defun vhdl-ext-hierarchy-build-entity-alist (files proj)
  "Build alist of entities for FILES in project PROJ.

Used for hierarchy.el frontend to visit file of entity at point."
  (let (proj-entity-alist)
    (dolist (file files)
      (cond (;; Builtin or GHDL without tree-sitter support
             (or (eq vhdl-ext-hierarchy-backend 'builtin)
                 (and (eq vhdl-ext-hierarchy-backend 'ghdl)
                      (not (treesit-language-available-p 'vhdl))))
             (with-temp-buffer
               (insert-file-contents file)
               (vhdl-ext-with-disabled-messages
                 (vhdl-ext-with-no-hooks
                   (vhdl-mode)))
               (dolist (entity-and-line (vhdl-ext-scan-buffer-entities-and-lines))
                 (push `(,(car entity-and-line)
                         ,file
                         ,(cadr entity-and-line))
                       proj-entity-alist))))
            (;; Tree-sitter or GHDL with tree-sitter support
             (or (eq vhdl-ext-hierarchy-backend 'tree-sitter)
                 (and (eq vhdl-ext-hierarchy-backend 'ghdl)
                      (treesit-language-available-p 'vhdl)))
             (with-temp-buffer
               (insert-file-contents file)
               (treesit-parser-create 'vhdl)
               (dolist (entity-node (vhdl-ts-nodes "\\<entity_declaration\\>"))
                 (push `(,(vhdl-ts--node-identifier-name entity-node)
                         ,file
                         ,(line-number-at-pos (treesit-node-start entity-node)))
                       proj-entity-alist))))
            ;; Default, wrong backend
            (t
             (error "Wrong backend selected"))))
    (setf (alist-get proj vhdl-ext-hierarchy-entity-alist nil 'remove 'string=) proj-entity-alist)))


;;;; Backends/extraction
;;;;; GHDL
(defconst vhdl-ext-hierarchy-ghdl-buffer-name " *ghdl-hier*"
  "Buffer name to use for the hierarchy file.")
(defconst vhdl-ext-hierarchy-ghdl-shell-cmds-buffer-name "*ghdl-hier-errors*"
  "Buffer name to use for the errors of the GHDL shell commands.")
(defconst vhdl-ext-hierarchy-ghdl-sim-args '("--ieee-asserts=disable-at-0"
                                             "--disp-tree=inst"))

(defconst vhdl-ext-hierarchy-ghdl-cache-file (file-name-concat vhdl-ext-cache-dir "hierarchy-ghdl")
  "The file where Vhdl-ext GHDL hierarchy will be written to.")

(defvar vhdl-ext-hierarchy-ghdl-flat-hier nil)
(defvar vhdl-ext-hierarchy-ghdl-alist nil)

(defun vhdl-ext-hierarchy-ghdl-parse-output ()
  "Parse output of GHDL process to extract hierarchy.

First preprocess output to make parsing a bit easier through regex search.
Next, step through nodes to build `vhdl-ext-hierarchy-ghdl-flat-hier' by calling
the recursive function `vhdl-ext-hierarchy-ghdl-parse-output--recursive'."
  (interactive)
  (let (root-node)
    ;; Preprocess:
    ;;   - Move to second line (to ignore first entity)
    ;;   - For the whole buffer, search for [entity] and `delete-indentation' to place instance/entities in the same line
    (with-current-buffer vhdl-ext-hierarchy-ghdl-buffer-name
      (goto-char (point-min))
      (forward-line)
      (while (re-search-forward "\\[entity\\]$" nil t)
        (delete-indentation)
        (forward-line))
      ;; - Remove anything that ends in [arch] or [instance] (these instances detected from not found entities)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(arch\\|instance\\)\\]$" nil t)
        (beginning-of-line)
        (kill-line 1))
      ;; Populate flat hierarchy recursively
      (setq vhdl-ext-hierarchy-ghdl-flat-hier nil)
      (goto-char (point-min))
      (looking-at vhdl-ext-identifier-re)
      (setq root-node (match-string-no-properties 0))
      (push (cons root-node nil) vhdl-ext-hierarchy-ghdl-flat-hier)
      (vhdl-ext-hierarchy-ghdl-parse-output--recursive root-node 0))))

(defun vhdl-ext-hierarchy-ghdl-parse-output--recursive (parent indent-level)
  "Step through nodes of preprocessed output recursively.

PARENT is the node name of the parent of current call.
INDENT-LEVEL is the `current-column' of the parent of current call."
  (let ((re-hier-node (concat "^[ |]+[+`]-"
                              "\\(?1:" vhdl-ext-identifier-re "\\(\([0-9]+\)\\)?\\) " ; Numbers in parenthesis support generate blocks
                              "\\[\\(?2:instance\\|entity\\|if-generate false\\|if-generate true\\|for-generate\\)\\]"
                              "\\(?3:[ |]+`-\\(?4:" vhdl-ext-identifier-re "\\) \\[entity\\]\\)?"))
        finished current-node current-node-type instance-string instances-list)
    (while (and (not finished)
                (re-search-forward re-hier-node nil t))
      (goto-char (match-beginning 1))
      (setq current-node-type (match-string-no-properties 2))
      (if (string= current-node-type "instance")
          (setq current-node (match-string-no-properties 4))
        (setq current-node (match-string-no-properties 1)))
      (setq instance-string (if (string= current-node-type "instance")
                                (concat (match-string-no-properties 4) ":" (match-string-no-properties 1))
                              (concat (match-string-no-properties 1) ":" current-node-type))) ; Else, generate if/for
      (if (> (current-column) indent-level) ;; Child
          ;; Add child to parent in `vhdl-ext-hierarchy-ghdl-flat-hier' and find its children
          (progn (unless (member (cons current-node nil) vhdl-ext-hierarchy-ghdl-flat-hier)
                   (push (cons current-node nil) vhdl-ext-hierarchy-ghdl-flat-hier))
                 (setq instances-list (assoc-string parent vhdl-ext-hierarchy-ghdl-flat-hier t))
                 (setcdr instances-list `(,@(cdr instances-list) ,instance-string))
                 (vhdl-ext-hierarchy-ghdl-parse-output--recursive current-node (current-column)))
        ;; Sibling/parent, go up one recursive call
        (beginning-of-line)
        (setq finished t)))))

(defun vhdl-ext-hierarchy-ghdl-extract (entity)
  "Extract hierarchy of ENTITY using GHDL as a backend.

With current prefix, force refreshing of hierarchy database for active project.

Return populated `hierarchy' struct."
  (unless (executable-find "ghdl")
    (error "Executable ghdl not found"))
  (let* ((proj (vhdl-ext-buffer-proj))
         (proj-workdir (vhdl-ext-proj-workdir proj))
         (cached-hierarchy-alist (if current-prefix-arg
                                     nil
                                   (vhdl-aget vhdl-ext-hierarchy-ghdl-alist proj)))
         (ghdl-sim-args (mapconcat #'identity vhdl-ext-hierarchy-ghdl-sim-args " "))
         (buf vhdl-ext-hierarchy-ghdl-buffer-name)
         (buf-err vhdl-ext-hierarchy-ghdl-shell-cmds-buffer-name)
         (err-msg (format "ghdl returned with errors\nCheck %s buffer" buf-err))
         (sources (vhdl-ext-proj-files proj))
         (sources-filtered `(,@(seq-take-while (lambda (elm) (not (string= elm buffer-file-name))) sources)
                               ,buffer-file-name))
         (elab-snapshot (file-name-concat temporary-file-directory entity))
         (cmd-compile (list (concat "ghdl -a "
                                    (vhdl-ext-ghdl-proj-args) " "
                                    (mapconcat #'identity sources-filtered " "))
                            "Compiling hierarchy..."))
         (cmd-elab (list (concat "ghdl -e "
                                 (vhdl-ext-ghdl-proj-args) " "
                                 "-o " elab-snapshot " "
                                 entity)
                         "Elaborating hierarchy..."))
         (cmd-hierarchy (list (concat "cd " (file-name-directory elab-snapshot) " && "
                                      "ghdl -r " entity " " ghdl-sim-args)
                              "Extracting hierarchy...")))
    ;; Use cache if already available instead of running GHDL command
    (if cached-hierarchy-alist
        (setq vhdl-ext-hierarchy-current-flat-hier cached-hierarchy-alist)
      ;; Otherwise, compile and elaborate from project sources
      (unless (file-exists-p proj-workdir) ; First prepare directory where modules are compiled to
        (make-directory proj-workdir :parents))
      (dolist (cmd `(,cmd-compile ,cmd-elab ,cmd-hierarchy))
        (message (cadr cmd))
        (unless (= 0 (shell-command (car cmd) buf buf-err))
          (pop-to-buffer buf-err)
          (insert (car cmd) "\n")
          (error (if noninteractive (buffer-substring-no-properties (point-min) (point-max))) err-msg)))
      ;; Extract flat hierarchy alist from GHDL output and construct hierarchy struct
      (vhdl-ext-hierarchy-ghdl-parse-output) ; Populates `vhdl-ext-hierarchy-ghdl-flat-hier'
      (kill-buffer buf)
      ;; Update variables and cache
      (vhdl-ext-proj-setcdr proj vhdl-ext-hierarchy-ghdl-alist vhdl-ext-hierarchy-ghdl-flat-hier)
      (vhdl-ext-hierarchy-build-entity-alist sources-filtered proj)
      (setq vhdl-ext-hierarchy-current-flat-hier vhdl-ext-hierarchy-ghdl-flat-hier)
      (when vhdl-ext-cache-enable
        (vhdl-ext-serialize vhdl-ext-hierarchy-ghdl-alist vhdl-ext-hierarchy-ghdl-cache-file)
        (vhdl-ext-serialize vhdl-ext-hierarchy-entity-alist vhdl-ext-hierarchy-entity-cache-file))) ; Updated after initial call to `vhdl-ext-proj-files'
    ;; Construct hierarchy struct after setting `vhdl-ext-hierarchy-current-flat-hier'
    (unless (assoc-string entity vhdl-ext-hierarchy-current-flat-hier t)
      (user-error "Could not find %s in the flat-hierarchy for project [%s].\nTry running `vhdl-ext-hierarchy-current-buffer' with prefix arg on current buffer" entity proj))
    (unless (cdr (assoc-string entity vhdl-ext-hierarchy-current-flat-hier t))
      (user-error "Current entity has no instances"))
    (vhdl-ext-hierarchy-extract--construct-node entity (hierarchy-new))))


;;;;; Tree-sitter
(defun vhdl-ext-hierarchy-tree-sitter-parse-file (file)
  "Return alist with entities and instances from FILE.

Each alist element car is a found entity in the file.
These elements cdr are the list of that entity's instances.
For each entity on the file, get instances of its first associated architecture.

There is however one limitation with tree-sitter parsing.  Since code is not
elaborated, all the instances of all the architectures associated to an entity
in a file will be merged in the flat hierarchy.

Instances have entity:INST format to make them unique for `hierarchy'
displaying.  Entities have no instance name since they are parsed on its
declaration."
  (let (arch-entity-name instances module-instances-alist module-instances-alist-entry)
    (with-temp-buffer
      (insert-file-contents file)
      (treesit-parser-create 'vhdl)
      (dolist (arch-node (vhdl-ts-nodes "architecture_body") module-instances-alist)
        (setq arch-entity-name (vhdl-ts-arch-entity-name arch-node))
        (setq instances nil)
        (dolist (inst-node (vhdl-ts-arch-instances-nodes arch-node))
          (push (concat (vhdl-ts--node-identifier-name inst-node) ":" (vhdl-ts--node-instance-name inst-node)) instances))
        (if (setq module-instances-alist-entry (assoc-string arch-entity-name module-instances-alist t))
            ;; Merge all the instances of current architecture for associated entity
            (setcdr module-instances-alist-entry `(,@(cdr module-instances-alist-entry) ,@(nreverse instances)))
          ;; Create new entry for the entity
          (push `(,arch-entity-name ,@(nreverse instances)) module-instances-alist))))))

(defun vhdl-ext-hierarchy-tree-sitter-extract (entity)
  "Extract hierarchy of MODULE using tree-sitter as a backend.

Populate `vhdl-ext-hierarchy-internal-alist' with alist of entities
and instances."
  (unless (eq vhdl-ext-hierarchy-backend 'tree-sitter)
    (error "Wrong backend!"))
  (vhdl-ext-hierarchy-extract--internal entity))


;;;;; Builtin
(defun vhdl-ext-hierarchy-builtin-parse-file (file)
  "Return alist with entities and instances from FILE.

Each alist element car is a found entity in the file.  These elements cdr are
the list of that entities associated architectures instances.

There is however one limitation with tree-sitter parsing.  Since code is not
elaborated, all the instances of all the architectures associated to an entity
in a file will be merged in the flat hierarchy.

Instances have entity:INST format to make them unique for `hierarchy'
displaying.  Entities have no instance name since they are parsed on its
declaration."
  (let (entities instances entity arch-end module-instances-alist module-instances-alist-entry)
    (with-temp-buffer
      (insert-file-contents file)
      (vhdl-ext-with-disabled-messages
        (vhdl-ext-with-no-hooks
          (vhdl-mode)))
      (setq entities (vhdl-ext-scan-buffer-entities))
      (while (vhdl-re-search-forward vhdl-ext-architecture-re nil t)
        (setq entity (match-string-no-properties 4))
        (when (member entity entities)
          (setq instances nil)
          ;; To calculate the architecture end point, we could call
          ;; `vhdl-ext-forward-sexp', which in turn calls `vhdl-forward-sexp'
          ;; and which in turn depends on correct indentation of the code.
          ;; Since this is something that would only work on Emacs VHDL
          ;; beautified code, let's grab all the instances since the beginning
          ;; of the first arch declaration until the beginning of next
          ;; entity/arch declaration, as a workaround.
          (setq arch-end (save-excursion
                           (when (vhdl-re-search-forward (concat "\\(" vhdl-ext-entity-re "\\)\\|\\(" vhdl-ext-architecture-re "\\)") nil t)
                             (match-beginning 0))))
          (while (vhdl-ext-find-entity-instance-fwd arch-end)
            (push (concat (match-string-no-properties 6) ":" (match-string-no-properties 1)) instances))
          (if (setq module-instances-alist-entry (assoc-string entity module-instances-alist t))
              ;; Merge all the instances of current architecture for associated entity
              (setcdr module-instances-alist-entry `(,@(cdr module-instances-alist-entry) ,@(nreverse instances)))
            ;; Create new entry for the entity
            (push `(,entity ,@(nreverse instances)) module-instances-alist)))))
    (nreverse module-instances-alist)))

(defun vhdl-ext-hierarchy-builtin-extract (entity)
  "Extract hierarchy of ENTITY using builtin Elisp backend.

Populate `vhdl-ext-hierarchy-internal-alist' with alist of modules
and instances."
  (unless (eq vhdl-ext-hierarchy-backend 'builtin)
    (error "Wrong backend!"))
  (vhdl-ext-hierarchy-extract--internal entity))

;;;; Frontends/navigation
;;;;; hierarchy.el
(defun vhdl-ext-hierarchy-twidget-buf--name ()
  "Return buffer name for twidget hierarchy buffer."
  (concat "*" (vhdl-ext-buffer-proj) "*"))

(defun vhdl-ext-hierarchy-twidget--buf-project ()
  "Return current project from twidget buffer name.

Assumes that hierarchy buffer name is `vhdl-ext-buffer-proj' with stars.
See `vhdl-ext-hierarchy-twidget-buf--name'."
  (string-remove-prefix "*" (string-remove-suffix "*" (buffer-name))))

(defun vhdl-ext-hierarchy-twidget-nav-open (&optional other-window)
  "Find definition of node/module at point.

Looks at value of `vhdl-ext-hierarchy-entity-alist' to check definition place of
entities.

If optional arg OTHER-WINDOW is non-nil find definition in other window."
  (interactive)
  (let ((entity (save-excursion
                  (widget-end-of-line)
                  (backward-sexp)
                  (thing-at-point 'symbol :no-props)))
        entities-files file line)
    (when entity
      (setq entities-files (vhdl-aget vhdl-ext-hierarchy-entity-alist (vhdl-ext-hierarchy-twidget--buf-project)))
      (setq file (nth 1 (assoc-string entity entities-files t)))
      (setq line (nth 2 (assoc-string entity entities-files t)))
      (if (and file line)
          (progn
            (if other-window
                (find-file-other-window file)
              (find-file file))
            (goto-char (point-min))
            (forward-line (1- line))
            (recenter '(4) t))
        (user-error "Could not find %s in `vhdl-ext-hierarchy-entity-alist'" entity)))))

(defun vhdl-ext-hierarchy-twidget-nav-open-other-window ()
  "Find definition of node/module at point in other window."
  (interactive)
  (vhdl-ext-hierarchy-twidget-nav-open :other-window))

(defun vhdl-ext-hierarchy-twidget-nav-init-expand ()
  "Init `tree-widget' expanding hierarchy.

INFO: Could do the same if adding the key argument :open t to `widget-create' in
`hierarchy' function `hierarchy-tree-display'.
INFO: Assumes it's initially collapsed, which is the case by default."
  (save-excursion
    (goto-char (point-min))
    (call-interactively #'widget-button-press)
    (call-interactively #'widget-forward)
    (while (not (bobp))
      (call-interactively #'widget-button-press)
      (call-interactively #'widget-forward))))

(defvar vhdl-ext-hierarchy-twidget-nav-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "SPC") 'widget-button-press)
    (define-key map (kbd "C-n") 'widget-forward)
    (define-key map (kbd "n")   'widget-forward)
    (define-key map (kbd "j")   'widget-forward)
    (define-key map (kbd "C-p") 'widget-backward)
    (define-key map (kbd "p")   'widget-backward)
    (define-key map (kbd "k")   'widget-backward)
    (define-key map (kbd "o")   'vhdl-ext-hierarchy-twidget-nav-open-other-window)
    (define-key map (kbd "C-o") 'vhdl-ext-hierarchy-twidget-nav-open-other-window)
    (define-key map (kbd "C-j") 'vhdl-ext-hierarchy-twidget-nav-open)
    map))

(define-minor-mode vhdl-ext-hierarchy-twidget-nav-mode
  "Instance navigation frontend for `tree-widget'."
  :lighter " vH"
  (message "Navigating hierarchy..."))

(defun vhdl-ext-hierarchy-twidget-display (hierarchy)
  "Display HIERARCHY using builtin `hierarchy' and `tree-widget' packages.

Show only module name, discard instance name after colon (mod:INST)."
  (unless (hierarchy-p hierarchy)
    (error "Hierarchy must be of hierarchy struct type"))
  (pop-to-buffer
   (hierarchy-tree-display
    hierarchy
    (lambda (item _) (insert (car (split-string (vhdl-ext-hierarchy--get-node-leaf item) ":"))))
    (get-buffer-create (vhdl-ext-hierarchy-twidget-buf--name))))
  ;; Navigation mode and initial expansion
  (vhdl-ext-hierarchy-twidget-nav-mode)
  (when vhdl-ext-hierarchy-twidget-init-expand
    (vhdl-ext-hierarchy-twidget-nav-init-expand)))

;;;;; outline
(defun vhdl-ext-hierarchy-outline-jump-to-file (&optional other-window)
  "Jump to module definition at point on navigation hierarchy file.
If OTHER-WINDOW is non-nil, open definition in other window."
  (interactive)
  (if other-window
      (xref-find-definitions-other-window (thing-at-point 'symbol t))
    (xref-find-definitions (thing-at-point 'symbol t))))

(defun vhdl-ext-hierarchy-outline-jump-to-file-other-window ()
  "Jump to module definition at point on navigation hierarchy file."
  (interactive)
  (vhdl-ext-hierarchy-outline-jump-to-file :other-window))

(define-minor-mode vhdl-ext-hierarchy-outline-nav-mode
  "Instance navigation frontend with `outline'.
Makes use of processed output under `outline-minor-mode' and `outline'."
  :lighter " vH"
  :keymap
  '(;; Hide/Show
    ("a"       . outline-show-all)
    ("i"       . outline-show-children)
    ("h"       . outline-show-children)
    ("l"       . outline-hide-sublevels)
    ("I"       . outline-show-branches)
    (";"       . outline-hide-other)
    ;; Movement
    ("u"       . outline-up-heading)
    ("C-c C-u" . outline-up-heading)
    ("n"       . outline-next-visible-heading)
    ("j"       . outline-next-visible-heading)
    ("p"       . outline-previous-visible-heading)
    ("k"       . outline-previous-visible-heading)
    ("C-c C-n" . outline-forward-same-level)
    ("C-c C-p" . outline-backward-same-level)
    ;; Jump
    ("o"       . vhdl-ext-hierarchy-outline-jump-to-file-other-window)
    ("C-o"     . vhdl-ext-hierarchy-outline-jump-to-file-other-window)
    ("RET"     . vhdl-ext-hierarchy-outline-jump-to-file)
    ("C-j"     . vhdl-ext-hierarchy-outline-jump-to-file))
  ;; Minor-mode code
  (setq-local outline-regexp "-- [\\*]+ ")
  (setq-local outline-regexp "-- [\\*]+ ")
  (setq-local outline-heading-alist
        '(("-- * " . 1)
          ("-- ** " . 2)
          ("-- *** " . 3)
          ("-- **** " . 4)
          ("-- ***** " . 5)
          ("-- ****** " . 6)
          ("-- ******* " . 7)
          ("-- ******** " . 8)))
  (setq-local outline-minor-mode-highlight 'override)
  (outline-minor-mode 1)
  (setq buffer-read-only t))

(defun vhdl-ext-hierarchy-outline-display (hierarchy)
  "Display HIERARCHY using `outline'.
Expects HIERARCHY to be a indented string."
  (let ((buf "*Vhdl-outline*"))
    (with-current-buffer (get-buffer-create buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert hierarchy)
      (vhdl-ext-replace-regexp-whole-buffer (concat "\\(?1:" vhdl-ext-identifier-sym-re "\\):\\(?2:" vhdl-ext-identifier-sym-re "\\)") "\\1")
      (goto-char (point-min))
      (vhdl-ext-replace-regexp-whole-buffer "  " "*")
      (vhdl-ext-replace-regexp-whole-buffer "*\\([a-zA-Z0-9_-]\\)" "* \\1")
      (vhdl-ext-replace-regexp-whole-buffer "^*" "-- *")
      ;; Parse not-used/not-found modules/files
      (goto-char (point-min))
      (re-search-forward "-- \\* ")
      (when (re-search-forward "-- \\* " nil t)
        (beginning-of-line)
        (open-line 3)
        (forward-line 2)
        (insert "-- * Not found entity references")
        (vhdl-ext-replace-string "-- * " "-- ** " (point) nil))
      ;; Insert local variables at the end of the file
      (goto-char (point-max))
      (newline 1)
      (insert "\n-- * Buffer local variables\n-- Local Variables:\n-- eval: (vhdl-mode 1)\n-- eval: (vhdl-ext-hierarchy-outline-nav-mode 1)\n-- End:\n")
      ;; Insert header to get some info of the file
      (goto-char (point-min))
      (open-line 1)
      (insert "-- Hierarchy generated by `vhdl-ext'\n")
      (vhdl-ext-with-no-hooks
        (vhdl-mode))
      (vhdl-ext-hierarchy-outline-nav-mode))
    (pop-to-buffer buf)))

;;;; Core
(defun vhdl-ext-hierarchy-serialize ()
  "Write variables to their cache files."
  (vhdl-ext-serialize vhdl-ext-hierarchy-internal-alist vhdl-ext-hierarchy-internal-cache-file)
  (vhdl-ext-serialize vhdl-ext-hierarchy-ghdl-alist vhdl-ext-hierarchy-ghdl-cache-file)
  (vhdl-ext-serialize vhdl-ext-hierarchy-entity-alist vhdl-ext-hierarchy-entity-cache-file))

(defun vhdl-ext-hierarchy-unserialize ()
  "Read cache files into their corresponding variables."
  (setq vhdl-ext-hierarchy-internal-alist (vhdl-ext-unserialize vhdl-ext-hierarchy-internal-cache-file))
  (setq vhdl-ext-hierarchy-ghdl-alist (vhdl-ext-unserialize vhdl-ext-hierarchy-ghdl-cache-file))
  (setq vhdl-ext-hierarchy-entity-alist (vhdl-ext-unserialize vhdl-ext-hierarchy-entity-cache-file)))

(defun vhdl-ext-hierarchy-setup ()
  "Setup hierarchy feature.
Read hierarchy cache if enabled."
  (when vhdl-ext-cache-enable
    (vhdl-ext-hierarchy-unserialize)))

(defun vhdl-ext-hierarchy-clear-cache (&optional all)
  "Clear hierarchy cache files for current project.

With prefix arg, clear cache for ALL projects."
  (interactive "P")
  (if (not all)
      (let ((proj (vhdl-ext-buffer-proj)))
        (unless proj
          (user-error "Not in a VHDL project buffer"))
        (vhdl-ext-proj-setcdr proj vhdl-ext-hierarchy-internal-alist nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-hierarchy-ghdl-alist nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-hierarchy-entity-alist nil)
        (vhdl-ext-hierarchy-serialize)
        (message "[%s] Cleared hierarchy cache!" proj))
    (setq vhdl-ext-hierarchy-internal-alist nil)
    (setq vhdl-ext-hierarchy-ghdl-alist nil)
    (setq vhdl-ext-hierarchy-entity-alist nil)
    (vhdl-ext-hierarchy-serialize)
    (message "Cleared hierarchy cache!")))

(defun vhdl-ext-hierarchy-extract (entity)
  "Construct hierarchy for ENTITY depending on selected backend."
  (cond (;; GHDL
         (eq vhdl-ext-hierarchy-backend 'ghdl)
         (vhdl-ext-hierarchy-ghdl-extract entity))
        (;; Tree-sitter
         (eq vhdl-ext-hierarchy-backend 'tree-sitter)
         (vhdl-ext-hierarchy-tree-sitter-extract entity))
        (;; Built-in
         (eq vhdl-ext-hierarchy-backend 'builtin)
         (vhdl-ext-hierarchy-builtin-extract entity))
        (;; Fallback
         t (error "Must set a proper extraction backend in `vhdl-ext-hierarchy-backend'"))))

(defun vhdl-ext-hierarchy-display (hierarchy)
  "Display HIERARCHY depending on selected frontend.

Handle conversion (if needed) of input extracted data depending on output
frontend.

E.g.: If displayed with outline it is needed to convert between a hierarchy
struct and an indented string."
  (let ((display-hierarchy hierarchy))
    (cond (;; Outline (conversion needed)
           (eq vhdl-ext-hierarchy-frontend 'outline)
           (when (hierarchy-p hierarchy)
             (setq display-hierarchy (vhdl-ext-hierarchy--convert-struct-to-string hierarchy)))
           (vhdl-ext-hierarchy-outline-display display-hierarchy))
          ;; Hierarchy (no conversion needed)
          ((eq vhdl-ext-hierarchy-frontend 'hierarchy)
           (setq display-hierarchy hierarchy)
           (vhdl-ext-hierarchy-twidget-display display-hierarchy))
          ;; Fallback
          (t (error "Must set a proper display frontend in `vhdl-ext-hierarchy-frontend'")))))

(defun vhdl-ext-hierarchy-parse (&optional verbose)
  "Return flat hierarchy of modules and instances of project.

Populates `vhdl-ext-hierarchy-internal-alist' for subsequent hierarchy
extraction and display.

With current-prefix or VERBOSE, dump output log."
  (interactive "P")
  (let* ((proj (vhdl-ext-buffer-proj))
         (files (vhdl-ext-proj-files proj))
         (num-files (length files))
         (num-files-processed 0)
         (log-file (concat vhdl-ext-hierarchy-internal-cache-file ".log"))
         (hier-progress-reporter (make-progress-reporter "[Hierarchy parsing]: " 0 num-files))
         flat-hierarchy data)
    (unless files
      (error "No files found for current buffer project.  Set `vhdl-ext-project-alist' accordingly?"))
    (when verbose
      (delete-file log-file))
    (dolist (file files)
      (when verbose
        (append-to-file (format "(%0d%%) [Hierarchy parsing] Processing %s\n" (/ (* num-files-processed 100) num-files) file) nil log-file))
      (progress-reporter-update hier-progress-reporter num-files-processed (format "[%s]" file))
      (setq data (cond ((eq vhdl-ext-hierarchy-backend 'tree-sitter)
                        (vhdl-ext-hierarchy-tree-sitter-parse-file file))
                       ((eq vhdl-ext-hierarchy-backend 'builtin)
                        (vhdl-ext-hierarchy-builtin-parse-file file))
                       (t
                        (error "Wrong backend selected!"))))
      (when data
        (dolist (entry data)
          (push entry flat-hierarchy)))
      (setq num-files-processed (1+ num-files-processed)))
    ;; Update hierarchy and entity alists and cache
    (vhdl-ext-proj-setcdr proj vhdl-ext-hierarchy-internal-alist flat-hierarchy)
    (vhdl-ext-hierarchy-build-entity-alist files proj)
    (when vhdl-ext-cache-enable
      (vhdl-ext-serialize vhdl-ext-hierarchy-internal-alist vhdl-ext-hierarchy-internal-cache-file)
      (vhdl-ext-serialize vhdl-ext-hierarchy-entity-alist vhdl-ext-hierarchy-entity-cache-file)) ; Updated after initial call to `vhdl-ext-proj-files'
    ;; Return value for async related function
    (list vhdl-ext-hierarchy-internal-alist vhdl-ext-hierarchy-entity-alist)))

(defun vhdl-ext-hierarchy-parse-async (&optional verbose)
  "Return flat hierarchy of modules and instances of project asynchronously.

Populates `vhdl-ext-hierarchy-internal-alist' for subsequent hierarchy
extraction and display.

With current-prefix or VERBOSE, dump output log."
  (interactive "P")
  (message "Starting hierarchy parsing for %s" (vhdl-ext-buffer-proj))
  (async-start
   `(lambda ()
      ,(async-inject-variables vhdl-ext-hierarchy-async-inject-variables-re)
      (require 'vhdl-ext)
      ;; Preserve cache on child Emacs process
      (setq vhdl-ext-hierarchy-internal-alist (vhdl-ext-unserialize vhdl-ext-hierarchy-internal-cache-file))
      (setq vhdl-ext-hierarchy-entity-alist (vhdl-ext-unserialize vhdl-ext-hierarchy-entity-cache-file))
      (vhdl-ext-hierarchy-parse ,@verbose))
   (lambda (result)
     (message "Finished analyzing hierarchy!")
     (setq vhdl-ext-hierarchy-internal-alist (car result))
     (setq vhdl-ext-hierarchy-entity-alist (cadr result)))))

(defun vhdl-ext-hierarchy-current-buffer ()
  "Extract and display hierarchy for entity of `current-buffer'."
  (interactive)
  (let* ((entity (vhdl-ext-select-file-entity))
         (hierarchy (vhdl-ext-hierarchy-extract entity)))
    (vhdl-ext-hierarchy-display hierarchy)))


(provide 'vhdl-ext-hierarchy)

;;; vhdl-ext-hierarchy.el ends here
