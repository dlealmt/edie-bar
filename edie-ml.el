;;; edie-ml.el --- Widget markup language for Edie. -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Package-Requires: ((emacs "28.1"))

;; This file is part of Edie.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'dom)
  (require 'map)
  (require 'pcase)
  (require 'subr-x))

(require 'dom)
(require 'map)
(require 'xml)

(defvar edie-ml-icon-directory "~/.cache/material-design/svg")

(defcustom edie-ml-icon-padding-top 0
  nil
  :type 'natnum)

(defcustom edie-ml-icon-size 24
  nil
  :type 'natnum)

(defcustom edie-ml-text-padding-top 0
  nil
  :type 'natnum)

(defvar edie-ml--dom nil)

(defun edie-ml-render-svg (spec)
  ""
  (let ((edie-ml--dom `(frame ((frame . ,(selected-frame))) ,(copy-tree spec))))
    (edie-ml-svg edie-ml--dom)))

(defun edie-ml-create-image (svg)
  ""
  (create-image (edie-ml--stringify svg) 'svg t :scale 1))

(defun edie-ml-insert-image (svg)
  (let* ((marker (point-marker))
         (image (edie-ml-create-image svg)))
    (insert-image image)
    (dom-set-attribute svg 'image marker)))

(cl-defgeneric edie-ml-svg (node))

(cl-defgeneric edie-ml-width (node))

(cl-defgeneric edie-ml-height (node))

(cl-defgeneric edie-ml-x (node))

(cl-defgeneric edie-ml-y (node))

(cl-defgeneric edie-ml-child-x (parent child))

(cl-defgeneric edie-ml-child-y (parent child ))

;; frame
(cl-defmethod edie-ml-width ((frame (head frame)))
  (frame-pixel-width (dom-attr frame 'frame)))

(cl-defmethod edie-ml-height ((frame (head frame)))
  (frame-pixel-height (dom-attr frame 'frame)))

(cl-defmethod edie-ml-svg ((frame (head frame)))
  (edie-ml-svg (car (edie-ml--children frame))))

(cl-defmethod edie-ml-child-x ((frame (head frame)) child)
  0)

;; box
(cl-defmethod edie-ml-width ((box (head box)))
  (if-let ((w (dom-attr box 'width)))
      w
    (let ((total 0)
          (spacing (or (dom-attr box 'spacing) 0))
          (pad-x (or (dom-attr box 'pad-x) 0)))
      (dolist (c (edie-ml--children box))
        (when-let ((w (edie-ml-width c)))
          (setq total (+ total w))))
      (+ total (* (1- (length (dom-children box))) spacing) (* pad-x 2)))))

(cl-defmethod edie-ml-height ((box (head box)))
  (edie-ml-height (edie-ml--parent box)))

(cl-defmethod edie-ml-x ((box (head box)))
  (edie-ml-child-x (edie-ml--parent box) box))

(cl-defmethod edie-ml-y ((_ (head box)))
  0)

(cl-defmethod edie-ml-child-x ((box (head box)) child)
  (+ (or (dom-attr box 'pad-x) 0)
     (cond
      ((eq (dom-attr child 'align) 'right)
       (edie-ml-inner-width box))
      (t
       (pcase-let* ((x 0)
                    (spacing (or (dom-attr box 'spacing) 0))
                    ((seq head &rest rest) (edie-ml--children box)))
         (while (and head (not (eq head child)))
           (setq x (+ x (edie-ml-width head) spacing))
           (setq head (car rest)
                 rest (cdr rest)))
         x)))))

(cl-defmethod edie-ml-transform (node)
  (cond
   ((eq (dom-attr node 'align) 'right)
    (format "translate(-%d)" (edie-ml-width node)))
   (t
    "none")))

(cl-defmethod edie-ml-svg ((node (head box)))
  ""
  (let ((width (edie-ml-width node))
        x transform)
    (edie-ml--make-svg-node
     (map-merge
      'alist
      `((width . ,width)
        (height . ,(edie-ml-height node))
        (x . ,(edie-ml-x node))
        (y . ,(edie-ml-y node))
        (transform . ,(edie-ml-transform node))))
     (edie-ml--svg-list (edie-ml--children node)))))

;; icon
(cl-defmethod edie-ml-width ((node (head icon)))
  edie-ml-icon-size)

(cl-defmethod edie-ml-height ((node (head icon)))
  edie-ml-icon-size)

(cl-defmethod edie-ml-x ((icon (head icon)))
  (edie-ml-child-x (edie-ml--parent icon) icon))

(cl-defmethod edie-ml-y ((icon (head icon)))
  (+ (/ (- (edie-ml-height (edie-ml--parent icon)) (edie-ml-height icon)) 2)
     edie-ml-icon-padding-top))

(cl-defmethod edie-ml-svg ((node (head icon)))
  ""
  (let ((svg (thread-last
               (file-name-concat edie-ml-icon-directory (dom-attr node 'name))
               (format "%s.svg")
               (xml-parse-file)
               (car)))
        (width (edie-ml-width node))
        (height (edie-ml-height node)))
    (dom-set-attribute svg 'width (edie-ml-width node))
    (dom-set-attribute svg 'height (edie-ml-height node))
    (dom-set-attribute svg 'x (edie-ml-x node))
    (dom-set-attribute svg 'y (edie-ml-y node))
    (dom-set-attribute svg 'viewBox (format "0 0 %d %d" width height))
    svg))

;; text
(cl-defmethod edie-ml-width ((text (head text)))
  (+ (* (or (dom-attr text 'pad-x) 0) 2) (* (frame-char-width) (length (dom-text text)))))

(cl-defmethod edie-ml-height ((text (head text)))
  (edie-ml-height (edie-ml--parent text)))

(cl-defmethod edie-ml-x ((text (head text)))
  (edie-ml-child-x (edie-ml--parent text) text))

(cl-defmethod edie-ml-y ((text (head text)))
  (let* ((pheight (edie-ml-height (edie-ml--parent text))))
    (+ edie-ml-text-padding-top (/ pheight 2))))

(cl-defmethod edie-ml-svg ((node (head text)))
  ""
  (let* ((string (car (edie-ml--children node)))
         (cwidth (frame-char-width))
         (from 0)
         (pad-x (or (dom-attr node 'pad-x) 0))
         (svg (edie-ml--make-svg-node `((width . ,(edie-ml-width node))
                                        (height . ,(edie-ml-height node))
                                        (x . ,(edie-ml-x node)))
                                      nil)))
    (while from
      (let* ((to (next-single-property-change from 'face string))
             (face (plist-get (text-properties-at from string) 'face))
             (fg (edie-ml--color-hex (edie-ml--face-attribute face :foreground)))
             (bg (edie-ml--color-hex (edie-ml--face-attribute face :background)))
             (family (edie-ml--face-attribute face :family))
             (substr (substring-no-properties string from to))
             (svg-substr (edie-ml--make-svg-node
                          `((x . ,(+ (* from cwidth) pad-x))
                            (width . ,(* cwidth (length substr)))
                            (height . ,(edie-ml-height node)))
                          (list
                           (dom-node 'rect `((width . "100%") (height . "100%") (fill . ,bg)))
                           (dom-node 'text `((fill . ,fg)
                                             (y . ,(edie-ml-y node))
                                             (font-family . ,family))
                                     (xml-escape-string substr))))))
        (dom-append-child svg svg-substr)
        (setq from to)))
    (cl-flet ((pad-rect (children width)
                (let ((fill (thread-first
                              children
                              (car)
                              (dom-children)
                              (car)
                              (dom-attr 'fill))))
                  (dom-node 'rect `((width . ,width) (height . "100%") (fill . ,fill))))))
      (dom-add-child-before svg (pad-rect (dom-children svg) (* pad-x 2)))
      (dom-add-child-before svg (pad-rect (reverse (dom-children svg)) "100%")))
    svg))

(defun edie-ml--face-attribute (faces attribute)
  (let ((faces (append (ensure-list faces) '(default)))
        value)
    (while (not value)
      (setq value (face-attribute-specified-or (face-attribute (pop faces) attribute) nil)))
    value))

(cl-defun edie-ml--stringify (spec)
  ""
  (pcase spec
    ((pred stringp) spec)
    ((seq tag attrs &rest children)
     (format "<%s%s>%s</%s>"
             tag
             (string-join (map-apply (lambda (k v) (format " %s=\"%s\"" k v)) attrs))
             (string-join (mapcar #'edie-ml--stringify children))
             tag))
    (_ (error "Don't know how to convert `%S' to string" spec))))

(defun edie-ml--make-svg-node (attributes children)
  (edie-ml--make-node
   'svg
   (map-merge
    'alist
    attributes
    '((xmlns . "http://www.w3.org/2000/svg")
      (xmlns:edie . "http://github.com/dleal-mojotech/edie")))
   children))

(defun edie-ml--svg-list (nodes)
  (let (lst)
    (dolist (n nodes (nreverse lst))
      (push (edie-ml-svg n) lst))))

(defun edie-ml--make-node (tag attributes children)
  (apply #'dom-node tag attributes children))

(defun edie-ml--parent (node)
  (dom-parent edie-ml--dom node))

(defalias 'edie-ml--children #'dom-children)

(defun edie-ml-inner-width (node)
  (let-alist (dom-attributes node)
    (- .width (* (or .pad-x 0) 2))))

(defun edie-ml--color-hex (name)
  (if (string-prefix-p "#" name)
      name
    (apply #'color-rgb-to-hex (nconc (color-name-to-rgb name) (list 2)))) )

(provide 'edie-ml)
;;; edie-ml.el ends here
