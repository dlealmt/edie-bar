;;; edie-widget.el --- Base library for edie widgets -*- lexical-binding: t -*-

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
  (require 'dom))

(require 'dom)
(require 'edie-ml)
(require 'svg)

(defun edie-widget-render-to (frame spec)
  ""
  (with-selected-frame frame
    (with-current-buffer (window-buffer (frame-root-window))
      (delete-region (point-min) (point-max))
      (let* ((update (lambda () (edie-widget-render-to frame spec)))
             (state (edie-widget--render-tree spec update))
             (svg (edie-ml-render-svg state)))
        (set-frame-parameter frame 'edie-bar:state state)
        (set-frame-parameter frame 'edie-bar:svg svg)
        (edie-ml-insert-image svg)
        svg))))

(defun edie-widget-propertize (string spec)
  (let* ((s (edie-widget--render-tree spec nil))
         (svg (edie-ml-render-svg s)))
    (put-text-property 0 (length string) 'display (edie-ml-create-image svg) string)
    string))

(cl-defgeneric edie-widget-render (widget _)
  ""
  widget)

(defun edie-widget--render-tree (spec update)
  ""
  (pcase spec
    ((pred stringp) spec)
    (_
     (pcase-let (((and next (seq tag attributes &rest children)) (edie-widget-render spec update))
                 (nchildren))
       `(,tag
         ,attributes
         ,@(dolist (c children (nreverse nchildren))
             (push (edie-widget--render-tree c update) nchildren)))))))

(provide 'edie-widget)
;;; edie-widget.el ends here
