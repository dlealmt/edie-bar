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

(require 'map)
(require 'xml)

(defvar edie-ml-icon-directory "~/.cache/material-design/svg")

(defvar edie-ml-unit-x 10.5)
(defvar edie-ml-unit-y nil)

(defun edie-ml--render (spec)
  ""
  (if (listp spec)
    (let ((node spec)
          next new)
      (while (not (eq node (setq next (edie-ml-render node))))
        (setq node next))
      (setq new (seq-take node 2))
      (dolist (c (dom-children node) new)
        (dom-append-child new (edie-ml--render c))))
    spec))

(defun edie-ml--measure (spec parent)
  (edie-ml-measure spec parent)
  (when (listp spec)
    (dolist (c (dom-children spec))
      (edie-ml--measure c spec)))
  (edie-ml-measure spec parent)
  spec)

(defun edie-ml--svg (spec)
  (let ((frame (dom-node
                'frame
                `((width . ,(frame-pixel-width))
                  (height . ,(frame-pixel-height)))
                spec)))
    (thread-first
      (edie-ml--render spec)
      (edie-ml--measure frame)
      (edie-ml-svg))))

(defun edie-ml-create-image (spec)
  ""
  (thread-first
    (edie-ml--svg spec)
    (edie-ml--stringify)
    (create-image 'svg t :scale 1)))

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

(cl-defgeneric edie-ml-render (node)
  ""
  node)

(cl-defgeneric edie-ml-measure (node parent)
  ""
  (unless (dom-attr node 'height)
    (dom-set-attribute node 'height (dom-attr parent 'height))))

(cl-defgeneric edie-ml-svg (node))

(cl-defmethod edie-ml-measure ((_ string) _parent)
  nil)

;; text

(defun edie-ml--specified-face-attributes (face attribute-filter)
  ""
  (let ((all (face-all-attributes face (selected-frame)))
        (filtered nil))
    (pcase-dolist (`(,attr . ,val) all filtered)
      (when (and (not (eq val 'unspecified)) (memq attr attribute-filter))
        (setf (alist-get attr filtered) val)))))

(defun edie-ml--face-attributes-at (point str attribute-filter)
  "Get a subset of face attributes at POINT in STR.

ATTRIBUTE-FILTER is the list of face attributes that interest us.

Only returns attributes that are specified (i.e., their value is
something other than `unspecified') for the faces found at point"
  (when-let ((props (text-properties-at point str))
             (face-prop (plist-get props 'face)))
    (let ((attrs nil))
      (dolist (face (if (listp face-prop) face-prop (list face-prop)) attrs)
        (setq attrs (map-merge 'alist
                               (edie-ml--specified-face-attributes face attribute-filter)
                               attrs))))))

(defun edie-ml--face-attributes-to-svg (face-attributes)
  "Convert FACE-ATTRIBUTES to SVG presentation attributes.

The `:foreground' and `:background' attributes both map to `fill'
so if both are in FACE-ATTRIBUTES, `fill' will be overwritten."
  (let ((alist nil))
    (pcase-dolist (`(,attr . ,val) face-attributes alist)
      (cond
       ((eq attr :family) (push (cons 'font-family val) alist))
       ((eq attr :foreground) (push (cons 'fill val) alist))
       ((eq attr :height) (push (cons 'font-size (format "%fpt" (/ val 10.0))) alist))
       ((eq attr :background) (push (cons 'fill val) alist))))))

(defun edie-ml--text (node tspans backgrounds)
  ""
  (let ((default-attrs (edie-ml--face-attributes-to-svg
                        (face-all-attributes 'default (selected-frame)))))
    (edie-ml--make-svg-node
     `((height . ,(dom-attr node 'height))
       (width . ,(dom-attr node 'width))
       (x . ,(dom-attr node 'x)))
     (nconc
      backgrounds
      (list (edie-ml--make-node
             'text
             (map-merge
              'alist
              default-attrs
              '((y . "50%")
                (dominant-baseline . "middle")
                ("xml:space" . "preserve")))
             tspans))))))

(defun edie-ml--text-span (string)
  ""
  (let* ((base-attrs (thread-last
                       '(:family :foreground :height)
                       (edie-ml--face-attributes-at 0 string)
                       (edie-ml--face-attributes-to-svg)))
         (svg-attrs (map-merge 'alist
                               `((alignment-baseline . "central"))
                               base-attrs)))
    (dom-node 'tspan svg-attrs (xml-escape-string (substring-no-properties string)))))

(defun edie-ml--text-background (string attributes)
  ""
  (let* ((default-attrs (edie-ml--face-attributes-to-svg
                         (edie-ml--specified-face-attributes 'default '(:background))))
         (base-attrs (thread-last
                       '(:background)
                       (edie-ml--face-attributes-at 0 string)
                       (edie-ml--face-attributes-to-svg)))
         (svg-attrs (map-merge 'alist
                               `((x . ,(* (alist-get 'x attributes) edie-ml-unit-x))
                                 (width . ,(* (length string) edie-ml-unit-x))
                                 (height . "100%"))
                               default-attrs
                               base-attrs
                               attributes)))
    (dom-node 'rect svg-attrs)))

(cl-defmethod edie-ml-measure ((node (head text)) parent)
  (unless (dom-attr node 'height)
    (dom-set-attribute node 'height (dom-attr parent 'height)))
  (unless (dom-attr node 'width)
    (dom-set-attribute node 'width (* (frame-char-width) (length (dom-texts node))))))

(cl-defmethod edie-ml-svg ((node (head text)))
  ""
  (if (listp (car (dom-children node)))
      node
    (let ((string (car (dom-children node)))
          (point 0)
          (tspans nil)
          (backgrounds nil))
      (while point
        (let* ((next-point (next-single-property-change point 'face string))
               (string (substring string point next-point))
               (this-text (edie-ml--text-span string))
               (prev-text (car tspans))
               (this-bg (edie-ml--text-background string `((x . ,point))))
               (prev-bg (car backgrounds)))
          (cond
           ((not this-text)
            (error "`this-text' should always be set"))
           ((equal (dom-attributes this-text) (dom-attributes prev-text))
            (dom-append-child prev-text (dom-text this-text)))
           (t
            (push this-text tspans)))
          (cond
           ((not prev-bg)
            (push this-bg backgrounds))
           ((equal (dom-attr this-bg 'fill) (dom-attr prev-bg 'fill))
            (dom-set-attribute
             prev-bg 'width (+ (dom-attr prev-bg 'width) (dom-attr this-bg 'width))))
           (t
            (push this-bg backgrounds)))
          (setq point next-point)))
      (edie-ml--text node (nreverse tspans) backgrounds))))

;; box
(cl-defmethod edie-ml-measure ((node (head box)) parent)
  (unless (dom-attr node 'width)
    (let ((total 0))
      (dolist (c (dom-children node))
        (when (dom-attr c 'width)
          (unless (dom-attr c 'x)
            (dom-set-attribute c 'x total))
          (setq total (+ total (dom-attr c 'width)))))
      (when (> total 0)
        (dom-set-attribute node 'width total))))
  (unless (dom-attr node 'height)
    (dom-set-attribute node 'height (dom-attr parent 'height)))
  (cond
   ((eq (dom-attr node 'align) 'right)
    (when-let ((width (dom-attr node 'width)))
      (dom-set-attribute node 'transform (format "translate(-%d)" width)))
    (when-let ((x (dom-attr parent 'width)))
      (dom-set-attribute node 'x x)))))

(cl-defmethod edie-ml-svg ((node (head box)))
  ""
  (edie-ml--make-svg-node (dom-attributes node) (edie-ml--svg-nodes (dom-children node))))

;; widget
(cl-defmethod edie-ml-svg ((node (head widget)))
  ""
  (pcase-let* ((edie-ml-unit-x (or edie-ml-unit-x (frame-char-width)))
               (edie-ml-unit-y (or edie-ml-unit-y (frame-char-height)))
               ((map height width) (dom-attributes node)))
    (edie-ml--make-svg-node
     `((width . ,(or (and width (* width edie-ml-unit-x)) (frame-pixel-width)))
       (height . ,(or (and height (* height edie-ml-unit-y)) (frame-pixel-height))))
     (edie-ml--svg-nodes (dom-children node)))))

(defun edie-ml--svg-nodes (nodes)
  ""
  (let ((svgs nil))
    (dolist (n nodes (nreverse svgs))
      (push (edie-ml-svg n) svgs))))

(defun edie-ml--make-svg-node (attributes children)
  (edie-ml--make-node
   'svg
   (map-merge
    'alist
    attributes
    '((version . "1.1")
      (xmlns . "http://www.w3.org/2000/svg")
      (xmlns:xlink . "http://www.w3.org/1999/xlink")))
   children))

(defun edie-ml--make-node (tag attributes children)
  (apply #'dom-node tag attributes children))

(provide 'edie-ml)
;;; edie-ml.el ends here
