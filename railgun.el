;;; railgun.el --- only my railgun  -*- lexical-binding: t -*-

;; Author: gynamics
;; Maintainer: gynamics
;; Package-Version: 0.1
;; Package-Requires: ((power-mode))
;; URL: https://github.com/gynamics/railgun.el
;; Keywords: games


;; This file is not part of GNU Emacs

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
;; Well, once I tried neovide and fell in love with its 'useless' 
;; cursor motion effects ... however, there has been an Emacs package
;; that implemented particle effects, so --
;;
;; let's play with it.

;;; Code:
(require 'power-mode)

(defun railgun--spawn-particles-at-point (position)
  "Spawn particales at POSITION."
   (unless power-mode--particle-timer
     (setq power-mode--particle-timer
           (run-with-timer 0 0.05
                           #'power-mode--animate-particles)))
   (let ((count (power-mode--random-range 2 2))
         (color (power-mode--foreground-color-before-point))
         (parent-frame (selected-frame)))
     (dotimes (_ count)
       (when-let ((frame (pop power-mode--particle-dead-frames)))
         (push frame power-mode--particle-live-frames)
         (let ((x (car position))
               (y (cdr position)))
         (modify-frame-parameters
            frame
            `((parent-frame . ,parent-frame)
              (background-color . ,color)
              (power-mode--life . 3)
              (power-mode--vx . ,(power-mode--random-range -3 3))
              (power-mode--vy . ,(power-mode--random-range -3 3))
              (power-mode--x . ,x)
              (power-mode--y . ,y)
              (left . ,x)
              (top . ,y)
              (visibility . t))))))))

(defvar railgun-render-method #'railgun--spawn-particles-at-point
  "Default render method of railgun.")

(defun railgun-line-xy (px py nx ny)
  "Moving cursor with a trailing visual effect from coord (PX PY) to (NX NY)."
  (let* ((sx (- nx px))
         (sy (- ny py))
         (dx 0)
         (dy 0)
         (comdiv (min (abs sx) (abs sy))))
    (if (zerop comdiv)
        (progn
          (unless (zerop sx) (setq dx (/ sx (abs sx))))
          (unless (zerop sy) (setq dy (/ sy (abs sy))))
          (setq comdiv (max (abs sx) (abs sy))))
      (progn
        (setq dx (/ sx comdiv))
        (setq dy (/ sy comdiv))))
    (let ((cnt 0))
      (while (< cnt comdiv)
        (let ((delta (floor (sqrt (- comdiv cnt)))))
          (setq cnt (+ cnt delta)
                px  (+ px (* delta dx))
                py  (+ py (* delta dy)))
          (railgun--spawn-particles-at-point `(,px . ,py)))
        ))
    ))

(defun railgun-line (start end)
  "Moving cursor with a trailing visual effect from position START to END."
  (unless (or (< start (window-start))
              (> start (window-end))
              (< end   (window-start))
              (> end   (window-end)))
    (let ((start-x-y   (posn-x-y (posn-at-point start)))
          (end-x-y     (posn-x-y (posn-at-point  end))))
          (railgun-line-xy (car start-x-y)
                           (cdr start-x-y)
                           (car end-x-y)
                           (cdr end-x-y)))))

(define-minor-mode railgun-mode
  "Only my railgun!"
  :init-value nil
  :lighter "Railgun"

  (defvar last-post-command-position 0
    "Holds the cursor position from the last run of post-command-hooks.")
  (make-variable-buffer-local 'last-post-command-position)  

  (defun railgun-post-command-callback ()
    (unless (equal (point) last-post-command-position)
      (railgun-line last-post-command-position (point))
      (setq last-post-command-position (point))))

  (power-mode)
  (if railgun-mode
      (progn
        (setq last-post-command-position (point))
        (add-to-list 'post-command-hook #'railgun-post-command-callback))
    (delete #'railgun-post-command-callback post-command-hook))
  )

(provide 'railgun)

;;; railgun.el ends here
