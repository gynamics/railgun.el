;;; railgun.el --- Only my railgun  -*- lexical-binding: t -*-

;; Author: gynamics
;; Maintainer: gynamics
;; Package-Version: 0.1
;; Package-Requires: ((emacs "25.1") (power-mode "0.1"))
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
;; that implemented particle effects, so, let's play with it.
;;
;; btw, it is never a nice idea to hack the code.

;;; Code:
(require 'power-mode)

(defun railgun--spawn-particles-at-point (position power)
  "Spawn particales at POSITION with POWER."
   (unless power-mode--particle-timer
     (setq power-mode--particle-timer
           (run-with-timer 0 0.1 #'power-mode--animate-particles)))
   (let ((count (power-mode--random-range 0 (min (ceiling (sqrt power)) 3)))
         (color (power-mode--foreground-color-before-point))
         (parent-frame (selected-frame))
         (char-width (frame-char-width)))
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
              (power-mode--vx . ,(power-mode--random-range -5 5))
              (power-mode--vy . ,(power-mode--random-range -3 3))
              (power-mode--x . ,x)
              (power-mode--y . ,y)
              (height . ,(/ char-width 3))
              (width . ,(/ char-width 3))
              (left . ,x)
              (top . ,y)
              (visibility . t))))))))

(defun railgun--line-xy (p n)
  "Moving cursor with a trailing visual effect from (X . Y) position P to N."
  (let* ((px (car p))
         (py (cdr p))
         (nx (car n))
         (ny (cdr n))
         (sx (- nx px))
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
          (railgun--spawn-particles-at-point `(,px . ,py) delta))))))

(defvar-local railgun--last-point-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(defvar-local railgun--debounce-interval 0.2
  "Debounce interval for coordinate updating, in seconds.")

(defvar-local railgun--debounce-lock nil
  "A simple lock variable, t for locked and nil for unlocked.")

(defun railgun-post-command-callback ()
  "Draw particle frames after each command that moved current point."
  (when (setq railgun--debounce-lock t)
    (when-let ((start (posn-at-point railgun--last-point-position))
               (end (posn-at-point)))
      (let ((start-xy (posn-x-y start))
            (end-xy (posn-x-y end)))
        (let ((delta-x (abs (- (car start-xy) (car end-xy))))
              (delta-y (abs (- (cdr start-xy) (cdr end-xy)))))
          (cond
           ((and (= delta-x 0) (= delta-y 0)))
           ((or (= delta-x 1) (= delta-y 1))
            (railgun--spawn-particles-at-point end-xy 1))
           (t (railgun--line-xy start-xy end-xy))))))
    (setq railgun--last-point-position (point))
    ;; release lock with delay
    (run-at-time railgun--debounce-interval nil
                 (lambda () (setq railgun--debounce-lock nil)))))

;;;###autoload
(define-minor-mode railgun-mode
  "Only my railgun!"
  :init-value nil
  :lighter "⚡"

  (if railgun-mode
      (progn
        ;; do somework that done by power-mode first
        (add-hook 'delete-frame-functions
                  #'power-mode--delete-frame-function)
        (add-hook 'window-size-change-functions
                  #'power-mode--window-size-change-function)
        (add-hook 'window-selection-change-functions
                  #'power-mode--window-selection-change-function)

        ;; Create dummy buffer.
        (setq power-mode--dummy-buffer (power-mode--make-dummy-buffer))
        ;; Make particle frames.
        (dotimes (_ power-mode-particle-limit)
          (setq power-mode--particle-dead-frames
                (cons (power-mode--make-particle-frame (selected-frame))
                        power-mode--particle-dead-frames)))
        ;; now shot the railgun
        (setq railgun--last-point-position (point))
        (add-to-list 'post-command-hook #'railgun-post-command-callback))

    (progn
      ;; release railgun
      (delete #'railgun-post-command-callback post-command-hook)
      ;; then clear power-mode
      (remove-hook 'delete-frame-functions
                   #'power-mode--delete-frame-function)
      (remove-hook 'window-size-change-functions
                   #'power-mode--window-size-change-function)
      (remove-hook 'window-selection-change-functions
                   #'power-mode--window-selection-change-function)
      ;; Delete particle frames.
      (dolist (frame power-mode--particle-live-frames)
        (delete-frame frame))
      (setq power-mode--particle-live-frames nil)
      (dolist (frame power-mode--particle-dead-frames)
        (delete-frame frame))
      (setq power-mode--particle-dead-frames nil)
      ;; Kill dummy buffer.
      (kill-buffer power-mode--dummy-buffer)
      (setq power-mode--dummy-buffer nil)
      ;; Kill timers.
      (when power-mode--streak-timeout-timer
        (power-mode--streak-timeout))
      (when power-mode--shake-timer
        (cancel-timer power-mode--shake-timer)
        (setq power-mode--shake-timer nil))
      (when power-mode--particle-timer
        (cancel-timer power-mode--particle-timer)
        (setq power-mode--particle-timer nil)))))

(provide 'railgun)

;;; railgun.el ends here
