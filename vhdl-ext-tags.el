;;; vhdl-ext-tags.el --- Vhdl-ext Tags  -*- lexical-binding: t -*-

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

;; Tag collection for jump to definition/reference and semantic completion.

;;; Code:

(require 'async)
(require 'vhdl-ts-mode)
(require 'vhdl-ext-utils)

(defvar vhdl-ext-tags-defs-table nil)
(defvar vhdl-ext-tags-refs-table nil)
(defvar vhdl-ext-tags-inst-table nil)

(defconst vhdl-ext-tags-cache-dir (file-name-concat user-emacs-directory "vhdl-ext")
  "The directory where vhdl-ext cache files will be placed at.")
(defconst vhdl-ext-tags-defs-table-cache-file (file-name-concat vhdl-ext-tags-cache-dir "defs-table")
  "The file where `vhdl-ext' defs-table will be written to.")
(defconst vhdl-ext-tags-refs-table-cache-file (file-name-concat vhdl-ext-tags-cache-dir "refs-table")
  "The file where `vhdl-ext' refs-table will be written to.")
(defconst vhdl-ext-tags-inst-table-cache-file (file-name-concat vhdl-ext-tags-cache-dir "inst-table")
  "The file where `vhdl-ext' inst-table will be written to.")


;;;; Common
(cl-defun vhdl-ext-tags-table-push (&key table tag type desc file parent)
  "Add entry for TAG in hash-table TABLE.

It is needed to provide TYPE, description DESC and FILE properties to add the
entry in the table.

Optional arg PARENT is the module where TAG is defined/instantiated for dot
completion.

If there is no entry in the table for TAG add one.  Otherwise update the
existing one with current location properties."
  (let ((tag-value (gethash tag table))
        locs-plist loc-new parent-value parent-items)
    ;; First add parent and populate its items if it was provided. Create if it did not exist.
    (when parent
      (setq parent-value (or (gethash parent table)
                             (puthash parent (list :items nil :locs nil) table)))
      (setq parent-items (plist-get parent-value :items))
      (unless (member tag parent-items)
        (plist-put parent-value :items `(,@parent-items ,tag))
        (puthash parent parent-value table)))
    ;; Next add the tag if it was not present in the table or update existing tag properties if it was present.
    (if (not tag-value)
        (puthash tag `(:items nil :locs (,(vhdl-ext-tags-locs-props type desc file))) table)
      (setq locs-plist (plist-get tag-value :locs))
      (setq loc-new (vhdl-ext-tags-locs-props type desc file))
      (unless (member loc-new locs-plist)
        (push loc-new locs-plist)
        (plist-put tag-value :locs locs-plist)
        (puthash tag `(:items ,(plist-get tag-value :items) :locs ,locs-plist) table)))))

(defun vhdl-ext-tags-locs-props (type desc &optional file)
  "Return :locs properties for current tag.

These include tag TYPE, description DESC, the FILE and current line."
  `(:type ,type
    :desc ,desc
    :file ,(or file buffer-file-name)
    :line ,(line-number-at-pos)))

(defun vhdl-ext-tags-desc ()
  "Return string description for tag at point.

The descriptin determines what `xref' will show when a match is found."
  (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun vhdl-ext-tags-is-def-p (tag defs-table file pos)
  "Return non-nil if TAG is a definition in DEFS-TABLE.

TAG is a definition if:
 1) It is present in DEFS-TABLE
 2) Its entry property list in DEFS-TABLE has the same :file and :line values

Use FILE and POS arguments for comparison."
  (let (existing-def existing-def-props)
    (and defs-table
         (setq existing-def (gethash tag defs-table))
         (setq existing-def-props (plist-get existing-def :locs))
         (progn (catch 'exit
                  (dolist (prop-list existing-def-props)
                    (when (and (eq (plist-get prop-list :file) file)
                               (eq (plist-get prop-list :line) (line-number-at-pos pos)))
                      (throw 'exit t))))))))


;;;; Tree-sitter
(defconst vhdl-ext-tags-definitions-ts-re
  (regexp-opt
   '("entity_declaration"
     "architecture_body"
     "package_declaration"
     "package_body"
     "component_declaration"
     "constant_interface_declaration" ; Generics
     "signal_interface_declaration"   ; Ports
     "signal_declaration"
     "constant_declaration"
     "full_type_declaration"
     "element_declaration"
     "variable_declaration"
     "procedure_declaration"
     "function_declaration"
     "function_body"
     "procedure_body"
     "configuration_declaration"
     "context_declaration"
     "component_instantiation_statement")
   'symbols)
  "Regexp of tree-sitter node types to be used for tags definitions.

Need to be quoted as symbols to avoid unwanted matches.

Even though \"component_instantiation_statement\" is not a declaration, this is
only included to add items to the defs table for completion.")

(cl-defun vhdl-ext-tags-table-push-defs-ts (&key table inst-table file)
  "Push definitions inside hash table TABLE using tree-sitter.

FILE might be specified for the cases when a temp-buffer without an associated
file is being parsed.

INST-TABLE is the instances table, needed to separate between tags for
completion and navigation."
  (let* ((node (treesit-buffer-root-node 'vhdl))
         (tree (treesit-induce-sparse-tree
                node
                vhdl-ext-tags-definitions-ts-re
                nil 1000))
         (inst-table (or inst-table (make-hash-table :test #'equal))))
    (vhdl-ext-tags-table-push-defs-ts--recurse :table table
                                               :inst-table inst-table
                                               :node tree
                                               :file file)))

(cl-defun vhdl-ext-tags-table-push-defs-ts--recurse (&key table inst-table node parent file)
  "Push definitions recursively inside hash table TABLE using tree-sitter.

Traverse the tree starting at NODE.

PARENT is passed as an argument to build the :items prop list of TABLE.

FILE might be specified for the cases when a temp-buffer without an associated
file is being parsed.

INST-TABLE is the instances table, needed to separate between tags for
completion and navigation."
  (let* ((ts-node (car node))
         (children (cdr node))
         (type (treesit-node-type ts-node))
         (is-instance (and type (string-match vhdl-ts-instance-re type))))
    ;; Iterate over all the nodes of the tree
    (mapc (lambda (child-node)
            (vhdl-ext-tags-table-push-defs-ts--recurse :table table
                                                       :inst-table inst-table
                                                       :node child-node
                                                       :parent ts-node
                                                       :file file))
          children)
    ;; Push definitions of current node
    (when ts-node ; root ts-node will be nil
      (goto-char (treesit-node-start ts-node))
      (if is-instance
          (vhdl-ext-tags-table-push :table inst-table
                                    :tag (vhdl-ts--node-instance-name ts-node)
                                    :type (vhdl-ts--node-identifier-name ts-node)
                                    :file file
                                    :parent (vhdl-ts--node-identifier-name parent))
        (vhdl-ext-tags-table-push :table table
                                  :tag (vhdl-ts--node-identifier-name ts-node)
                                  :type type
                                  :desc (vhdl-ext-tags-desc)
                                  :file file
                                  :parent (vhdl-ts--node-identifier-name parent))))))

(cl-defun vhdl-ext-tags-table-push-refs-ts (&key table defs-table file)
  "Push references inside hash table TABLE using tree-sitter.

Optional definitions table DEFS-TABLE is used to filter out references that have
already been parsed as definitions.

FILE can be provided for the case when references are fetched from a
temp-buffer."
  (let (tag pos)
    (dolist (node (vhdl-ts-nodes vhdl-ts-identifier-re))
      (setq tag (treesit-node-text node :no-prop))
      (setq pos (treesit-node-start node))
      (unless (and defs-table
                   (vhdl-ext-tags-is-def-p tag defs-table file pos))
        (goto-char pos)
        (vhdl-ext-tags-table-push :table table
                                  :tag tag
                                  :desc (vhdl-ext-tags-desc)
                                  :file file)))))

(defun vhdl-ext-tags-get (&optional verbose)
  "Get tags of current project.
With current-prefix or VERBOSE, dump output log."
  (interactive "p")
  (let* ((proj (vhdl-ext-buffer-proj))
         (files (vhdl-ext-proj-files))
         (num-files (length files))
         (num-files-processed 0)
         (table (make-hash-table :test #'equal))
         (inst-table (make-hash-table :test #'equal))
         (log-file (file-name-concat vhdl-ext-tags-cache-dir "tags.log"))
         msg progress)
    (when verbose
      (delete-file log-file))
    ;; Definitions
    (dolist (file files)
      (with-temp-buffer
        (setq progress (/ (* num-files-processed 100) num-files))
        (setq msg (format "(%0d%%) [Tags collection] Processing %s" progress file))
        (when verbose
          (append-to-file (concat msg "\n") nil log-file))
        (message "%s" msg)
        (insert-file-contents file)
        (vhdl-ext-with-disabled-messages
          (vhdl-ts-mode))
        (vhdl-ext-tags-table-push-defs-ts :table table :inst-table inst-table :file file))
      (setq num-files-processed (1+ num-files-processed)))
    (vhdl-ext-proj-setcdr proj vhdl-ext-tags-defs-table table)
    (vhdl-ext-proj-setcdr proj vhdl-ext-tags-refs-table inst-table)
    (vhdl-ext-serialize vhdl-ext-tags-defs-table vhdl-ext-tags-defs-table-cache-file)
    (vhdl-ext-serialize vhdl-ext-tags-inst-table vhdl-ext-tags-inst-table-cache-file)
    ;; References
    (setq table (make-hash-table :test #'equal)) ; Clean table
    (setq num-files-processed 0)
    (dolist (file files)
      (with-temp-buffer
        (setq progress (/ (* num-files-processed 100) num-files))
        (setq msg (format "(%0d%%) [References collection] Processing %s" progress file))
        (when verbose
          (append-to-file (concat msg "\n") nil log-file))
        (message "%s" msg)
        (insert-file-contents file)
        (vhdl-ext-with-disabled-messages
          (vhdl-ts-mode))
        (vhdl-ext-tags-table-push-refs-ts :table table :defs-table (vhdl-aget vhdl-ext-tags-defs-table proj) :file file))
      (setq num-files-processed (1+ num-files-processed)))
    (vhdl-ext-proj-setcdr proj vhdl-ext-tags-refs-table table)
    (vhdl-ext-serialize vhdl-ext-tags-refs-table vhdl-ext-tags-refs-table-cache-file)
    ;; Return value for async processing
    `(,vhdl-ext-tags-defs-table ,vhdl-ext-tags-inst-table ,vhdl-ext-tags-refs-table)))

(defun vhdl-ext-tags-get-async (&optional verbose)
  "Create tags table asynchronously.
With current-prefix or VERBOSE, dump output log."
  (interactive "p")
  (unless (vhdl-ext-buffer-proj-root)
    (user-error "Not in a VHDL project buffer"))
  (message "Starting tag collection for %s" (vhdl-ext-buffer-proj-root))
  (async-start
   `(lambda ()
      ,(async-inject-variables vhdl-ext-async-inject-variables-re)
      (require 'vhdl-ext)
      (vhdl-ext-tags-get ,verbose))
   (lambda (result)
     (message "Finished collection tags!")
     ;; Tags definitions
     (setq vhdl-ext-tags-defs-table (nth 0 result))
     (setq vhdl-ext-tags-inst-table (nth 1 result))
     (setq vhdl-ext-tags-refs-table (nth 2 result)))))

(defun vhdl-ext-tags-setup ()
  "Setup tags feature, reading tags from cache."
  (setq vhdl-ext-tags-defs-table (vhdl-ext-unserialize vhdl-ext-tags-defs-table-cache-file))
  (setq vhdl-ext-tags-refs-table (vhdl-ext-unserialize vhdl-ext-tags-refs-table-cache-file))
  (setq vhdl-ext-tags-inst-table (vhdl-ext-unserialize vhdl-ext-tags-inst-table-cache-file)))

(defun vhdl-ext-tags-clear-cache (&optional all)
  "Clear tags cache files for current project.

With prefix arg, clear cache for ALL projects."
  (interactive "p")
  (if (not all)
      (let ((proj (vhdl-ext-buffer-proj)))
        (unless proj
          (user-error "Not in a VHDL project buffer"))
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-defs-table nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-refs-table nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-inst-table nil)
        (vhdl-ext-serialize vhdl-ext-tags-defs-table vhdl-ext-tags-defs-table-cache-file)
        (vhdl-ext-serialize vhdl-ext-tags-refs-table vhdl-ext-tags-refs-table-cache-file)
        (vhdl-ext-serialize vhdl-ext-tags-inst-table vhdl-ext-tags-inst-table-cache-file)
        (message "[%s] Cleared tags cache!" proj))
    (setq vhdl-ext-tags-defs-table nil)
    (setq vhdl-ext-tags-refs-table nil)
    (setq vhdl-ext-tags-inst-table nil)
    (vhdl-ext-serialize vhdl-ext-tags-defs-table vhdl-ext-tags-defs-table-cache-file)
    (vhdl-ext-serialize vhdl-ext-tags-refs-table vhdl-ext-tags-refs-table-cache-file)
    (vhdl-ext-serialize vhdl-ext-tags-inst-table vhdl-ext-tags-inst-table-cache-file)
    (message "Cleared all projects tags cache!")))


(provide 'vhdl-ext-tags)

;;; vhdl-ext-tags.el ends here
