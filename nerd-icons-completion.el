;;; nerd-icons-completion.el --- Add icons to completion candidates -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (nerd-icons "0.0.1") (compat "30"))
;; URL: https://github.com/rainstormstudio/nerd-icons-completion
;; Keywords: convenient, files, icons

;; This program is free software: you can redistribute it and/or modify
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

;; Add nerd-icons to completion candidates.
;; nerd-icons-completion is inspired by
;; `all-the-icons-completion': https://github.com/iyefrat/all-the-icons-completion

;;; Code:

(require 'nerd-icons)
(require 'compat)

(defgroup nerd-icons-completion nil
  "Add icons to completion candidates."
  :group 'appearance
  :group 'convenience
  :prefix "nerd-icons-completion")

(defcustom nerd-icons-completion-icon-size 1.0
  "The default icon size in completion."
  :group 'nerd-icons-completion
  :type 'float)

(defcustom nerd-icons-completion-category-icons
  '((command       . (nerd-icons-codicon "nf-cod-symbol_method"      nerd-icons-blue))
    (theme         . (nerd-icons-faicon  "nf-fae-palette_color"      nerd-icons-yellow))
    (symbol        . (nerd-icons-mdicon  "nf-md-symbol"              nerd-icons-dblue))
    (variable      . (nerd-icons-codicon "nf-cod-symbol_variable"    nerd-icons-lpurple))
    (function      . (nerd-icons-codicon "nf-cod-symbol_method"      nerd-icons-blue))
    (package       . (nerd-icons-codicon "nf-cod-package"            nerd-icons-orange))
    (symbol-help   . (nerd-icons-mdicon  "nf-md-help_box"            nerd-icons-lpurple))
    (face          . (nerd-icons-mdicon  "nf-md-format_color_fill"   nerd-icons-pink))
    (input-method  . (nerd-icons-faicon  "nf-fa-keyboard"            nerd-icons-blue-alt))
    (org-roam-node . (nerd-icons-codicon "nf-cod-note"               nerd-icons-silver))
    (imenu         . (nerd-icons-octicon "nf-oct-dot_fill"           nerd-icons-lblue))
    (kill-ring     . (nerd-icons-mdicon  "nf-md-text"                nerd-icons-silver))
    (coding-system . (nerd-icons-codicon "nf-cod-file_binary"        nerd-icons-lpurple)))
  "Alist of icons for completion categories."
  :group 'nerd-icons-completion
  :type '(alist :key-type symbol
                :value-type (list symbol string symbol)))

(defcustom nerd-icons-completion-eglot-icons
  '((1  . (nerd-icons-codicon "nf-cod-text_size"          font-lock-doc-face))               ;; Text
    (2  . (nerd-icons-codicon "nf-cod-symbol_method"      font-lock-function-name-face))     ;; Method
    (3  . (nerd-icons-codicon "nf-cod-symbol_method"      font-lock-function-name-face))     ;; Function
    (4  . (nerd-icons-codicon "nf-cod-triangle_right"     font-lock-function-name-face))     ;; Constructor
    (5  . (nerd-icons-codicon "nf-cod-symbol_field"       font-lock-variable-name-face))     ;; Field
    (6  . (nerd-icons-codicon "nf-cod-symbol_variable"    font-lock-variable-name-face))     ;; Variable
    (7  . (nerd-icons-codicon "nf-cod-symbol_class"       font-lock-type-face))              ;; Class
    (8  . (nerd-icons-codicon "nf-cod-symbol_interface"   font-lock-type-face))              ;; Interface
    (9  . (nerd-icons-codicon "nf-cod-file_submodule"     font-lock-preprocessor-face))      ;; Module
    (10 . (nerd-icons-codicon "nf-cod-symbol_property"    font-lock-variable-name-face))     ;; Property
    (11 . (nerd-icons-codicon "nf-cod-symbol_ruler"       font-lock-constant-face))          ;; Unit
    (12 . (nerd-icons-codicon "nf-cod-symbol_field"       font-lock-builtin-face))           ;; Value
    (13 . (nerd-icons-codicon "nf-cod-symbol_enum"        font-lock-builtin-face))           ;; Enum
    (14 . (nerd-icons-codicon "nf-cod-symbol_keyword"     font-lock-keyword-face))           ;; Keyword
    (15 . (nerd-icons-codicon "nf-cod-symbol_snippet"     font-lock-string-face))            ;; Snippet
    (16 . (nerd-icons-codicon "nf-cod-symbol_color"       success))                          ;; Color
    (17 . (nerd-icons-codicon "nf-cod-file"               font-lock-string-face))            ;; File
    (18 . (nerd-icons-codicon "nf-cod-references"         font-lock-variable-name-face))     ;; Reference
    (19 . (nerd-icons-codicon "nf-cod-folder"             font-lock-string-face))            ;; Folder
    (20 . (nerd-icons-codicon "nf-cod-symbol_enum_member" font-lock-builtin-face))           ;; EnumMember
    (21 . (nerd-icons-codicon "nf-cod-symbol_constant"    font-lock-constant-face))          ;; Constant
    (22 . (nerd-icons-codicon "nf-cod-symbol_structure"   font-lock-variable-name-face))     ;; Struct
    (23 . (nerd-icons-codicon "nf-cod-symbol_event"       font-lock-warning-face))           ;; Event
    (24 . (nerd-icons-codicon "nf-cod-symbol_operator"    font-lock-comment-delimiter-face)) ;; Operator
    (25 . (nerd-icons-codicon "nf-cod-list_unordered"     font-lock-type-face)))             ;; TypeParameter
  "Alist of icons for eglot completion.

This should map the kind to `eglot--kind-names'."
  :group 'nerd-icons-completion
  :type '(alist :key-type int
                :value-type (list symbol string symbol)))

(defface nerd-icons-completion-dir-face
  '((t nil))
  "Face for the directory icon."
  :group 'nerd-icons-faces)

(cl-defgeneric nerd-icons-completion-get-icon (_cand _cat)
  "Return the icon for the candidate CAND of completion category CAT."
  "")

(cl-defmethod nerd-icons-completion-get-icon (_cand (cat symbol))
  "Return the icon for completion category CAT."
  (if-let* ((spec (cdr (assoc cat nerd-icons-completion-category-icons)))
            (icon-fn (nth 0 spec))
            (icon-name (nth 1 spec))
            (face (nth 2 spec)))
      (concat (funcall icon-fn icon-name :height nerd-icons-completion-icon-size :face face) " ")
    ""))

(cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql file)))
  "Return the icon for the candidate CAND of completion category file."
  (cond ((string-match-p "\\/$" cand)
         (concat
          (nerd-icons-icon-for-dir cand :face 'nerd-icons-completion-dir-face
                                        :height nerd-icons-completion-icon-size)
          " "))
        (t (concat (nerd-icons-icon-for-file cand :height nerd-icons-completion-icon-size)
                   " "))))

(cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql project-file)))
  "Return the icon for the candidate CAND of completion category project-file."
  (nerd-icons-completion-get-icon cand 'file))

(cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql eglot-capf)))
  "Return the icon for the candidate CAND of completion category eglot-capf."
  (if-let* ((orig (get-text-property 0 'eglot--lsp-item cand))
            (kind (plist-get orig :kind))
            (spec (cdr (assoc kind nerd-icons-completion-eglot-icons)))
            (icon-fn (nth 0 spec))
            (icon-name (nth 1 spec))
            (face (nth 2 spec)))
      (concat (funcall icon-fn icon-name :height nerd-icons-completion-icon-size :face face) " ")
    ""))

(cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql eglot)))
  "Return the icon for the candidate CAND of completion category eglot."
  (nerd-icons-completion-get-icon cand 'eglot-capf))

(cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql buffer)))
  "Return the icon for the candidate CAND of completion category buffer."
  (let* ((mode (buffer-local-value 'major-mode (get-buffer cand)))
         (icon (nerd-icons-icon-for-mode mode :height nerd-icons-completion-icon-size))
         (parent-icon (nerd-icons-icon-for-mode
                       (get mode 'derived-mode-parent)
                       :height nerd-icons-completion-icon-size)))
    (concat
     (if (symbolp icon)
         (if (symbolp parent-icon)
             (nerd-icons-faicon "nf-fa-sticky_note_o" :height nerd-icons-completion-icon-size)
           parent-icon)
       icon)
     " ")))

(autoload 'bookmark-get-filename "bookmark")
(cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql bookmark)))
  "Return the icon for the candidate CAND of completion category bookmark."
  (if-let* ((fname (bookmark-get-filename cand)))
      (nerd-icons-completion-get-icon fname 'file)
    (concat (nerd-icons-octicon "nf-oct-bookmark"
                                :face 'nerd-icons-completion-dir-face
                                :height nerd-icons-completion-icon-size) " ")))

(defun nerd-icons-completion-completion-metadata-get (orig metadata prop)
  "Meant as :around advice for `completion-metadata-get', Add icons as prefix.
ORIG should be `completion-metadata-get'
METADATA is the metadata.
PROP is the property which is looked up."
  (if (eq prop 'affixation-function)
      (let ((cat (funcall orig metadata 'category))
            (aff (or (funcall orig metadata 'affixation-function)
                     (when-let* ((ann (funcall orig metadata 'annotation-function)))
                       (lambda (cands)
                         (mapcar (lambda (x) (list x "" (funcall ann x))) cands))))))
        (cond
         ((and (eq cat 'multi-category) aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (let ((orig (get-text-property 0 'multi-category cand)))
                           (list cand
                                 (concat (nerd-icons-completion-get-icon (cdr orig) (car orig))
                                         prefix)
                                 suffix)))))
                    (funcall aff cands))))
         ((and cat aff)
          (lambda (cands)
            (mapcar (lambda (x)
                      (pcase-exhaustive x
                        (`(,cand ,prefix ,suffix)
                         (list cand
                               (concat (nerd-icons-completion-get-icon cand cat)
                                       prefix)
                               suffix))))
                    (funcall aff cands))))
         ((eq cat 'multi-category)
          (lambda (cands)
            (mapcar (lambda (x)
                      (let ((orig (get-text-property 0 'multi-category x)))
                        (list x (nerd-icons-completion-get-icon (cdr orig) (car orig)) "")))
                    cands)))
         (cat
          (lambda (cands)
            (mapcar (lambda (x)
                      (list x (nerd-icons-completion-get-icon x cat) ""))
                    cands)))
         (aff)))
    (funcall orig metadata prop)))

;; For the byte compiler
(defvar marginalia-mode)
;;;###autoload
(defun nerd-icons-completion-marginalia-setup ()
  "Hook to `marginalia-mode-hook' to bind `nerd-icons-completion-mode' to it."
  (nerd-icons-completion-mode (if marginalia-mode 1 -1)))

;;;###autoload
(define-minor-mode nerd-icons-completion-mode
  "Add icons to completion candidates."
  :global t
  (if nerd-icons-completion-mode
      (progn
        (advice-add #'completion-metadata-get :around #'nerd-icons-completion-completion-metadata-get)
        (advice-add (compat-function completion-metadata-get) :around #'nerd-icons-completion-completion-metadata-get))
    (progn
      (advice-remove #'completion-metadata-get #'nerd-icons-completion-completion-metadata-get)
      (advice-remove (compat-function completion-metadata-get) #'nerd-icons-completion-completion-metadata-get))))

(provide 'nerd-icons-completion)
;;; nerd-icons-completion.el ends here
