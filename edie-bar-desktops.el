;;; edie-bar.el --- A desktop bar -*- lexical-binding: t -*-

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

(defconst edie-bar-desktops-color-active "#fe8019")
(defconst edie-bar-desktops-color-used "#ffae6c")

(defvar edie-bar-desktops--svg nil)

(defun edie-bar-desktops ()
  (let ((used (thread-last
                (ewmh-window-list)
                (seq-map (pcase-lambda ((map :desktop)) desktop))
                (seq-uniq)))
        (curdsk (ewmh-desktop-current)))
    (mapconcat (lambda (i)
                 (cond
                  ((= i (map-elt curdsk :id))
                   (embar-widget--icon embar-widget-desktop-list-icon
                                       'embar-widget-desktop-list-active))
                  ((seq-contains-p used i)
                   (embar-widget--icon embar-widget-desktop-list-icon
                                      'embar-widget-desktop-list-used))
                  (t
                   (embar-widget--icon embar-widget-desktop-list-icon
                                      'embar-widget-desktop-list-normal))))
               (seq-map (pcase-lambda ((map :id)) id) (ewmh-desktop-list))
               embar-widget-desktop-list-separator)))

(provide 'edie-bar-desktops)
;;; edie-bar-desktops.el ends here
