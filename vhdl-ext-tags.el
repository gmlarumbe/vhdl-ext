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
;;
;; Tag collection for jump to definition/reference and semantic completion.
;;
;; `vhdl-ext-tags-get-async' relies on emacs-async: https://github.com/jwiegley/emacs-async:
;;
;; - Limitations with async tag collection:
;;
;;   - The `async' library has limitations with hash-tables:
;;      - async-start returns hash tables as lists: https://github.com/jwiegley/emacs-async/issues/164
;;      - Since the # is stripped (https://github.com/jwiegley/emacs-async/issues/145) and it's needed to
;;        properly represent hash-tables, the result is that this implementation requires a workaround
;;      - Plus, stripping the # also gave more errors:
;;        - e.g. stripping the # in the :test function #'vhdl-ext-string=, used for case-insensitive string
;;          comparison, made the child process to give an error about a not found test function
;;      - Solution:
;;        - Reading/writing to/from stored cached files
;;
;;   - Injecting the environment with the value of large hash tables (e.g. `vhdl-ext-tags-defs-table') in
;;   `async-start' via `async-inject-variables' takes a long time in the parent process
;;      - Solution:
;;        - Reading environment from stored cached files
;;
;;; Code:

(require 'async)
(require 'map)
(require 'vhdl-ts-mode)
(require 'vhdl-ext-utils)


(defcustom vhdl-ext-tags-fontify-matches t
  "Set to non-nil to fontify matches for xref.

This setting slightly increases processing time of `vhdl-ext-tags-get'."
  :type 'boolean
  :group 'vhdl-ext)


(defvar vhdl-ext-tags-file-hashes nil)

(defvar vhdl-ext-tags-defs-table nil)
(defvar vhdl-ext-tags-refs-table nil)
(defvar vhdl-ext-tags-inst-table nil)

(defvar vhdl-ext-tags-defs-file-tables nil)
(defvar vhdl-ext-tags-inst-file-tables nil)
(defvar vhdl-ext-tags-refs-file-tables nil)

(defvar vhdl-ext-tags-defs-current-file nil)
(defvar vhdl-ext-tags-inst-current-file nil)
(defvar vhdl-ext-tags-refs-current-file nil)

(defconst vhdl-ext-tags-defs-file-tables-cache-file (file-name-concat vhdl-ext-cache-dir "defs-file-tables")
  "The file where `vhdl-ext' defs-file-tables will be written to.")
(defconst vhdl-ext-tags-refs-file-tables-cache-file (file-name-concat vhdl-ext-cache-dir "refs-file-tables")
  "The file where `vhdl-ext' refs-file-tables will be written to.")
(defconst vhdl-ext-tags-inst-file-tables-cache-file (file-name-concat vhdl-ext-cache-dir "inst-file-tables")
  "The file where `vhdl-ext' inst-file-tables will be written to.")

(defconst vhdl-ext-tags-defs-table-cache-file (file-name-concat vhdl-ext-cache-dir "defs-table")
  "The file where `vhdl-ext' defs-table will be written to.")
(defconst vhdl-ext-tags-refs-table-cache-file (file-name-concat vhdl-ext-cache-dir "refs-table")
  "The file where `vhdl-ext' refs-table will be written to.")
(defconst vhdl-ext-tags-inst-table-cache-file (file-name-concat vhdl-ext-cache-dir "inst-table")
  "The file where `vhdl-ext' inst-table will be written to.")

(defconst vhdl-ext-tags-file-hashes-cache-file (file-name-concat vhdl-ext-cache-dir "file-hashes")
  "The file where `vhdl-ext' file-hashes will be written to.")

(defconst vhdl-ext-tags-cache-log-file (file-name-concat vhdl-ext-cache-dir "tags.log"))

(defconst vhdl-ext-tags-async-inject-variables-re
  (eval-when-compile
    (regexp-opt '("load-path"
                  "buffer-file-name"
                  "default-directory"
                  "vhdl-ext-feature-list"
                  "vhdl-ext-project-alist")
                'symbols)))


;;;; Common
(define-hash-table-test 'vhdl-ext-string= 'string-equal-ignore-case 'vhdl-ext-tags-sxhash)

(defun vhdl-ext-tags-sxhash (string)
  "Return hash code for `upcase' of STRING."
  (sxhash (upcase string)))

(defsubst vhdl-ext-tags-locs-props (type desc file line col)
  "Return :locs properties for current tag.

These include tag TYPE, description DESC, the FILE, current LINE and COL."
  `(:type ,type
    :desc ,desc
    :file ,file
    :line ,line
    :col ,col))

(defsubst vhdl-ext-tags-desc ()
  "Return string description for tag at point.

The description determines what `xref' will show when a match is found."
  (buffer-substring (line-beginning-position) (line-end-position)))

(cl-defsubst vhdl-ext-tags-table-push (&key table tag type desc file line col parent)
  "Add entry for TAG in hash-table TABLE.

It is needed to provide TYPE, description DESC, FILE, LINE and COL properties to
add the entry in the table.

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
        (puthash tag `(:items nil :locs (,(vhdl-ext-tags-locs-props type desc file line col))) table)
      (setq locs-plist (plist-get tag-value :locs))
      (setq loc-new (vhdl-ext-tags-locs-props type desc file line col))
      (unless (member loc-new locs-plist)
        (push loc-new locs-plist)
        (plist-put tag-value :locs locs-plist)
        (puthash tag `(:items ,(plist-get tag-value :items) :locs ,locs-plist) table)))))

(defun vhdl-ext-tags-table-remove-file-locs (file file-tables table)
  "Remove FILE tag locations in TABLE.

FILE-TABLES is the intermediate variable with a per-file hash table for current
project."
  (when (and file-tables
             (gethash file file-tables)
             table)
    (let ((file-tag-locs-table (gethash file file-tables))
          items-and-locs locs tag-loc)
      (maphash (lambda (key value)
                 (setq items-and-locs (gethash (car key) table))
                 (setq locs (plist-get items-and-locs :locs))
                 (setq tag-loc (vhdl-ext-tags-locs-props (plist-get value :type)
                                                         (plist-get value :desc)
                                                         (plist-get (cdr key) :file)
                                                         (plist-get (cdr key) :line)
                                                         (plist-get value :col)))
                 (when (member tag-loc locs)
                   (setf (cl-getf items-and-locs :locs) (remove tag-loc locs)))
                 (when (not (plist-get items-and-locs :locs))
                   (remhash (car key) table)))
               file-tag-locs-table))))

(defun vhdl-ext-tags-add-file-locs (file file-tables table)
  "Add FILE tag locations in TABLE.

FILE-TABLES is the intermediate variable with a per-file hash table for current
project."
  (let ((file-table (gethash file file-tables)))
    (maphash (lambda (key value)
               (vhdl-ext-tags-table-push :table table
                                         :tag (car key)
                                         :type (plist-get value :type)
                                         :desc (plist-get value :desc)
                                         :file (plist-get (cdr key) :file)
                                         :line (plist-get (cdr key) :line)
                                         :col (plist-get value :col)
                                         :parent (plist-get value :parent)))
             file-table)))


;;;; Tree-sitter
(defconst vhdl-ext-tags-definitions-ts-re
  (eval-when-compile
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
       "subtype_declaration"
       "element_declaration"
       "variable_declaration"
       "alias_declaration"
       "procedure_declaration"
       "function_declaration"
       "function_body"
       "procedure_body"
       "configuration_declaration"
       "context_declaration"
       "component_instantiation_statement")
     'symbols))
  "Regexp of tree-sitter node types to be used for tags definitions.

Need to be quoted as symbols to avoid unwanted matches.

Even though \"component_instantiation_statement\" is not a declaration, this is
only included to add items to the defs table for completion.")

(defun vhdl-ext-tags-table-push-defs-ts (file)
  "Push current FILE definitions using tree-sitter.

Update hash tables `vhdl-ext-tags-defs-current-file' and
`vhdl-ext-tags-inst-current-file'."
  (let* ((node (treesit-buffer-root-node 'vhdl))
         (tree (treesit-induce-sparse-tree
                node
                vhdl-ext-tags-definitions-ts-re
                nil 1000)))
    (vhdl-ext-tags-table-push-defs-ts--recurse :node tree
                                               :parent nil
                                               :file file)))

(cl-defun vhdl-ext-tags-table-push-defs-ts--recurse (&key node parent file)
  "Push current FILE definitions recursively using tree-sitter.

Traverse the tree starting at NODE.

PARENT is passed as an argument to build the :items prop list of
`vhdl-ext-tags-defs-current-file'."
  (let* ((ts-node (car node))
         (children (cdr node))
         (type (treesit-node-type ts-node))
         (is-instance (and type (string-match vhdl-ts-instance-re type))))
    ;; Iterate over all the nodes of the tree
    (mapc (lambda (child-node)
            (vhdl-ext-tags-table-push-defs-ts--recurse :node child-node
                                                       :parent ts-node
                                                       :file file))
          children)
    ;; Push definitions of current node
    (when ts-node ; root ts-node will be nil
      (goto-char (treesit-node-start ts-node))
      (if is-instance
          (puthash `(,(vhdl-ts--node-instance-name ts-node) ; Key plist
                     :file ,file
                     :line ,(line-number-at-pos))
                   `(:type ,(vhdl-ts--node-identifier-name ts-node) ; Value plist
                     :col ,(current-column)
                     :parent ,(vhdl-ts--node-identifier-name parent))
                   vhdl-ext-tags-inst-current-file)
        (puthash `(,(vhdl-ts--node-identifier-name ts-node) ; Key plist
                   :file ,file
                   :line ,(line-number-at-pos))
                 `(:type ,type ; Value plist
                   :desc ,(vhdl-ext-tags-desc)
                   :col ,(current-column)
                   :parent ,(vhdl-ts--node-identifier-name parent))
                 vhdl-ext-tags-defs-current-file)))))

(defun vhdl-ext-tags-table-push-refs-ts (file)
  "Push current FILE references using tree-sitter.

Update hash table `vhdl-ext-tags-refs-current-file'."
  (let (tag pos line)
    (dolist (node (vhdl-ts-nodes vhdl-ts-identifier-re))
      (setq tag (treesit-node-text node :no-prop))
      (setq pos (treesit-node-start node))
      (setq line (line-number-at-pos pos))
      (unless (gethash `(,tag :file ,file :line ,line) vhdl-ext-tags-defs-current-file) ; Unless it's already a definition
        (goto-char pos)
        (puthash `(,tag ; Key plist
                   :file ,file
                   :line ,(line-number-at-pos))
                 `(:desc ,(vhdl-ext-tags-desc) ; Value plist
                   :col ,(current-column))
                 vhdl-ext-tags-refs-current-file)))))


;;;; Tags collection and cache
(defun vhdl-ext-tags-proj-init (proj)
  "Initialize value of PROJ variables and hash-tables needed for tags collection."
  (dolist (var '(vhdl-ext-tags-file-hashes
                 vhdl-ext-tags-defs-file-tables
                 vhdl-ext-tags-inst-file-tables
                 vhdl-ext-tags-refs-file-tables))
    (unless (vhdl-aget (eval var) proj)
      (set var (cons (cons proj (make-hash-table :test 'equal)) (symbol-value var)))))
  ;; Final merged tables need to have case-insensitive string keys when querying via `gethash'
  (dolist (var '(vhdl-ext-tags-defs-table
                 vhdl-ext-tags-inst-table
                 vhdl-ext-tags-refs-table))
    (unless (vhdl-aget (eval var) proj)
      (set var (cons (cons proj (make-hash-table :test 'vhdl-ext-string=)) (symbol-value var))))))

(defun vhdl-ext-tags-get--process-file (file proj &optional file-was-removed verbose)
  "Auxiliary function to process FILE tags of project PROJ.

Steps:
 - Initialize tags variables
 - For removed files, remove corresponding file locs from tags tables
   (FILE-WAS-REMOVED should be non-nil)
 - Check current file hash and compare to previous stored ones to check if it
   has changed
 - Consider 3 different scenarios:
    - File did not change: skip that file and check next one
    - File changed: remove previous file locs, collect new file tags and update
      tables and file hashes
    - File is new: collect new file tags and update tables and file hashes (no
      need to remove any file locs).

Optional arg VERBOSE to display extra messages for debugging."
  (let ((proj-file-hashes (vhdl-aget vhdl-ext-tags-file-hashes proj))
        (proj-defs-file-tables (vhdl-aget vhdl-ext-tags-defs-file-tables proj))
        (proj-defs-table (vhdl-aget vhdl-ext-tags-defs-table proj))
        (proj-inst-file-tables (vhdl-aget vhdl-ext-tags-inst-file-tables proj))
        (proj-inst-table (vhdl-aget vhdl-ext-tags-inst-table proj))
        (proj-refs-file-tables (vhdl-aget vhdl-ext-tags-refs-file-tables proj))
        (proj-refs-table (vhdl-aget vhdl-ext-tags-refs-table proj))
        file-hash-new file-hash-old)
    ;; Reset current file tags
    (setq vhdl-ext-tags-defs-current-file (make-hash-table :test 'equal))
    (setq vhdl-ext-tags-inst-current-file (make-hash-table :test 'equal))
    (setq vhdl-ext-tags-refs-current-file (make-hash-table :test 'equal))
    ;; Process tags
    (if file-was-removed
        (progn ; Remove tags in reverse order: first locs from the table, then from intermediate tables, and finally from file-hashes
          (vhdl-ext-tags-table-remove-file-locs file proj-defs-file-tables proj-defs-table)
          (vhdl-ext-tags-table-remove-file-locs file proj-inst-file-tables proj-inst-table)
          (vhdl-ext-tags-table-remove-file-locs file proj-refs-file-tables proj-refs-table)
          (remhash file proj-defs-file-tables)
          (remhash file proj-inst-file-tables)
          (remhash file proj-refs-file-tables)
          (remhash file proj-file-hashes))
      ;; File not removed: Could be not modified, modified or added
      (with-temp-buffer
        (insert-file-contents file)
        (setq file-hash-new (secure-hash 'md5 (buffer-substring-no-properties (point-min) (point-max))))
        (setq file-hash-old (gethash file proj-file-hashes))
        (if (string= file-hash-new file-hash-old)
            (when verbose (message "Skipping file: %s" file)) ; Not modified
          ;; Modified/added
          (puthash file file-hash-new proj-file-hashes)
          ;; If file has changed remove old tags
          (when file-hash-old
            (vhdl-ext-tags-table-remove-file-locs file proj-defs-file-tables proj-defs-table)
            (vhdl-ext-tags-table-remove-file-locs file proj-inst-file-tables proj-inst-table)
            (vhdl-ext-tags-table-remove-file-locs file proj-refs-file-tables proj-refs-table))
          ;; If file is new or has changed, collect tags
          (if vhdl-ext-tags-fontify-matches
              (vhdl-ext-with-disabled-messages
                (vhdl-ext-with-no-hooks ; Avoid spending time on any possible hooks, just on fontifying to get text properties
                  (vhdl-ts-mode)
                  (font-lock-ensure)))
            (treesit-parser-create 'vhdl)) ; Not running `vhdl-ts-mode' avoids unnecessary hooks for this task
          (vhdl-ext-tags-table-push-defs-ts file) ; Populates `vhdl-ext-tags-defs-current-file' and `vhdl-ext-tags-inst-current-file'
          (vhdl-ext-tags-table-push-refs-ts file) ; Populates `vhdl-ext-tags-refs-current-file'
          ;; Update file tables
          (puthash file vhdl-ext-tags-defs-current-file proj-defs-file-tables)
          (puthash file vhdl-ext-tags-inst-current-file proj-inst-file-tables)
          (puthash file vhdl-ext-tags-refs-current-file proj-refs-file-tables)
          ;; Update tables
          (vhdl-ext-tags-add-file-locs file proj-defs-file-tables proj-defs-table)
          (vhdl-ext-tags-add-file-locs file proj-inst-file-tables proj-inst-table)
          (vhdl-ext-tags-add-file-locs file proj-refs-file-tables proj-refs-table))))))

(defun vhdl-ext-tags-get (&optional verbose)
  "Get tags of current project.
With current-prefix or VERBOSE, dump output log."
  (interactive "P")
  (let* ((proj (vhdl-ext-buffer-proj))
         (files (vhdl-ext-proj-files proj))
         (files-removed (seq-difference (map-keys (vhdl-aget vhdl-ext-tags-file-hashes proj)) files))
         (num-files (+ (length files-removed) (length files)))
         (num-files-processed 0)
         (log-file vhdl-ext-tags-cache-log-file)
         (tags-progress-reporter (make-progress-reporter "[Tags collection]: " 0 num-files)))
    (vhdl-ext-tags-proj-init proj)
    (when verbose
      (delete-file log-file))
    (dolist (file files-removed)
      (progress-reporter-update tags-progress-reporter num-files-processed (format "[%s]" file))
      (vhdl-ext-tags-get--process-file file proj :file-was-removed verbose)
      (setq num-files-processed (1+ num-files-processed)))
    (dolist (file files)
      (when verbose
        (append-to-file (format "(%0d%%) [Tags collection] Processing %s\n" (/ (* num-files-processed 100) num-files) file) nil log-file))
      (progress-reporter-update tags-progress-reporter num-files-processed (format "[%s]" file))
      (vhdl-ext-tags-get--process-file file proj nil verbose)
      (setq num-files-processed (1+ num-files-processed)))
    (message "Finished collection of tags!")))

(defun vhdl-ext-tags-get-async (&optional verbose)
  "Create tags table asynchronously.
With current-prefix or VERBOSE, dump output log."
  (interactive "P")
  (let ((proj-root (vhdl-ext-buffer-proj-root)))
    (unless proj-root
      (user-error "Not in a VHDL project buffer"))
    (message "Starting tag collection for %s" proj-root)
    (async-start
     `(lambda ()
        ,(async-inject-variables vhdl-ext-tags-async-inject-variables-re)
        (require 'vhdl-ext)
        (vhdl-ext-tags-unserialize)   ; Read environment in child process
        (vhdl-ext-tags-get ,@verbose) ; Update variables in child process
        (vhdl-ext-tags-serialize))    ; Update cache file in childe process
     (lambda (_result)
       (vhdl-ext-tags-unserialize)
       (message "Finished collection of tags!"))))) ; Update parent process from cache file

(defun vhdl-ext-tags-serialize ()
  "Write variables to their cache files."
  (message "Serializing `vhdl-ext' tags cache...")
  (dolist (var `((,vhdl-ext-tags-defs-file-tables . ,vhdl-ext-tags-defs-file-tables-cache-file)
                 (,vhdl-ext-tags-refs-file-tables . ,vhdl-ext-tags-refs-file-tables-cache-file)
                 (,vhdl-ext-tags-inst-file-tables . ,vhdl-ext-tags-inst-file-tables-cache-file)
                 (,vhdl-ext-tags-defs-table       . ,vhdl-ext-tags-defs-table-cache-file)
                 (,vhdl-ext-tags-inst-table       . ,vhdl-ext-tags-inst-table-cache-file)
                 (,vhdl-ext-tags-refs-table       . ,vhdl-ext-tags-refs-table-cache-file)
                 (,vhdl-ext-tags-file-hashes      . ,vhdl-ext-tags-file-hashes-cache-file)))
    (vhdl-ext-serialize (car var) (cdr var)))
  (message "Serialized `vhdl-ext' tags cache!"))

(defun vhdl-ext-tags-unserialize ()
  "Read cache files into their corresponding variables."
  (dolist (var `((vhdl-ext-tags-defs-file-tables . ,vhdl-ext-tags-defs-file-tables-cache-file)
                 (vhdl-ext-tags-refs-file-tables . ,vhdl-ext-tags-refs-file-tables-cache-file)
                 (vhdl-ext-tags-inst-file-tables . ,vhdl-ext-tags-inst-file-tables-cache-file)
                 (vhdl-ext-tags-defs-table       . ,vhdl-ext-tags-defs-table-cache-file)
                 (vhdl-ext-tags-inst-table       . ,vhdl-ext-tags-inst-table-cache-file)
                 (vhdl-ext-tags-refs-table       . ,vhdl-ext-tags-refs-table-cache-file)
                 (vhdl-ext-tags-file-hashes      . ,vhdl-ext-tags-file-hashes-cache-file)))
    (set (car var) (vhdl-ext-unserialize (cdr var)))))

(defun vhdl-ext-tags-setup ()
  "Setup tags feature: cache read at startup and write before exit."
  (when vhdl-ext-cache-enable
    (vhdl-ext-tags-unserialize)
    (add-hook 'kill-emacs-hook #'vhdl-ext-tags-serialize)))

(defun vhdl-ext-tags-clear-cache (&optional all)
  "Clear tags cache files for current project.

With prefix arg, clear cache for ALL projects."
  (interactive "P")
  (if (not all)
      (let ((proj (vhdl-ext-buffer-proj)))
        (unless proj
          (user-error "Not in a VHDL project buffer"))
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-defs-table nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-refs-table nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-inst-table nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-defs-file-tables nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-refs-file-tables nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-inst-file-tables nil)
        (vhdl-ext-proj-setcdr proj vhdl-ext-tags-file-hashes nil)
        (vhdl-ext-tags-serialize)
        (message "[%s] Cleared tags cache!" proj))
    (setq vhdl-ext-tags-defs-table nil)
    (setq vhdl-ext-tags-refs-table nil)
    (setq vhdl-ext-tags-inst-table nil)
    (setq vhdl-ext-tags-defs-file-tables nil)
    (setq vhdl-ext-tags-refs-file-tables nil)
    (setq vhdl-ext-tags-inst-file-tables nil)
    (setq vhdl-ext-tags-file-hashes nil)
    (vhdl-ext-tags-serialize)
    (message "Cleared all projects tags cache!")))


(provide 'vhdl-ext-tags)

;;; vhdl-ext-tags.el ends here
