;;; edie-bar-clock.el --- Clock widget for edie-bar -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 David Leal

;; Author: David Leal <dleal@mojotech.com>
;; Maintainer: David Leal <dleal@mojotech.com>
;; Created: 2022
;; Version: 0.0.1

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

(require 'map)
(require 'pcase)

(defvar edie-bar-clock--svg nil)
(defvar edie-bar-clock--current-time nil)
(defvar edie-bar-clock--current-time-timer nil)

;;;###autoload
(defun edie-bar-clock (enable &optional props)
  (when edie-bar-clock--current-time-timer
    (cancel-timer edie-bar-clock--current-time-timer))

  (when enable
    (pcase-let (((map :width :height) props))
      (setq edie-bar-clock--svg (svg-create 150 32 :id "edie-bar-clock"))

      (edie-bar-clock--refresh)

      (setq edie-bar-clock-current-time-timer
            (run-at-time (- 60 (car (decode-time))) 60 #'edie-bar-clock--refresh)))))

(defun edie-bar-clock-widget ()
  '(text (icon )))

(defun edie-bar-clock--refresh ()
  (edie-bar-clock--render)
  (edie-bar-trigger-refresh edie-bar-clock--svg))

(defun edie-bar-clock--render ()
  (svg-text edie-bar-clock--svg
            (format-time-string "%H:%M")
            :id "edie-bar-clock--text" :fill "#fff" :x 0 :y 22))

(provide 'edie-bar-clock)
;;; edie-bar-clock.el ends here
