;;; marquee-header.el --- Code interface for displaying marquee in header.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-07-21 12:03:34

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Code interface for displaying marquee in header.
;; Keyword: animation header interface library marquee
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs090218/marquee-header

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
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
;; Code interface for displaying marquee in header.
;;

;;; Code:


(defgroup marquee-header nil
  "Code interface for displaying marquee in header."
  :prefix "marquee-header-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/marquee-header"))


(defcustom marquee-header-display-time 3.0
  "How long you want to show the marquee message."
  :type 'float
  :group 'marquee-header)

(defcustom marquee-header-direction 'left
  "The direction this marquee is going towards to."
  :type '(choice (const :tag "none" none)
                 (const :tag "left" left)
                 (const :tag "right" right))
  :group 'marquee-header)


(defvar-local marquee-header--message ""
  "Current message.")

(defvar-local marquee-header--message-decoration ""
  "Decorate the current message for displaying animation.")

(defvar-local marquee-header--time 0.0
  "Current show time.")

(defvar-local marquee-header--speed 0.0
  "The animation speed for time calculation.")

(defvar-local marquee-header--timer nil
  "Timer pointer for updating marquee animation.")

(defvar-local marquee-header--direction nil
  "Record the marquee direction.")

(defvar-local marquee-header--previous-header-line-format nil
  "Record down the previous header format.")

(defvar marquee-header--previous-window nil
  "Record down the previous selected window.")

(defvar-local marquee-header--frame-counter 0
  "Count the frame.")


(defun marquee-header--padding (w)
  "Get the whitespace padding with W."
  (let ((padding-str "")
        (padding-cnt 0))
    (while (< padding-cnt w)
      (setq padding-str (concat padding-str " "))
      (setq padding-cnt (1+ padding-cnt)))
    padding-str))

(defun marquee-header--cancel-timer ()
  "Cancel all timer for marquee."
  (when (and marquee-header--timer
             (timerp marquee-header--timer))
    (cancel-timer marquee-header--timer))
  (setq marquee-header--timer nil))

(defun marquee-header--revert-header ()
  "Reset header line format to previous value."
  (setq header-line-format marquee-header--previous-header-line-format)
  (marquee-header--cancel-timer)
  (setq marquee-header--previous-window nil))

(defun marquee-header--display-header (cw)
  "Display the header animation with current selected window CW."
  (save-selected-window
    (select-window cw)
    (if (equal marquee-header--direction 'none)
        (progn
          (setq header-line-format marquee-header--message)  ; Just displayed it directly.
          (setq marquee-header--timer
                (run-at-time marquee-header--time
                             nil
                             'marquee-header--revert-header)))
      (setq marquee-header--speed (/ marquee-header--time (window-width)))
      (cond ((equal marquee-header--direction 'left)
             ;; Remove the first character.
             (setq marquee-header--message-decoration
                   (substring marquee-header--message-decoration
                              1 (length marquee-header--message-decoration))))
            ((equal marquee-header--direction 'right)
             (setq marquee-header--message-decoration
                   (concat " " marquee-header--message-decoration))))
      (setq header-line-format marquee-header--message-decoration)
      (setq marquee-header--frame-counter (1- marquee-header--frame-counter))
      (if (= 0 marquee-header--frame-counter)
          (marquee-header--revert-header)
        (marquee-header--cancel-timer)
        (setq marquee-header--timer
              (run-at-time marquee-header--speed
                           nil
                           'marquee-header--display-header cw))))))

;;;###autoload
(defun marquee-header-notify (msg &optional time direction)
  "Show the marquee notification with MSG.
TIME is the time that will show on screen.  DIRECTION is for marquee animation."
  (setq marquee-header--previous-header-line-format header-line-format)
  (if (and msg
           (stringp msg))
      (setq marquee-header--message msg)
    (error "Can't display marquee header without appropriate message"))
  (setq marquee-header--time (if (and time
                                      (numberp time))
                                 time
                               marquee-header-display-time))
  (setq marquee-header--direction (if (and direction
                                           (or (equal direction 'none)
                                               (equal direction 'left)
                                               (equal direction 'right)))
                                      direction
                                    marquee-header-direction))
  (cond ((equal direction 'left)
         (setq marquee-header--message-decoration (concat (marquee-header--padding (window-width)) marquee-header--message)))
        ((equal direction 'right)
         (setq marquee-header--message-decoration marquee-header--message)))
  (setq marquee-header--frame-counter (+ (window-width) (length marquee-header--message)))  ; Reset frame counter.
  (marquee-header--display-header (selected-window)))


(provide 'marquee-header)
;;; marquee-header.el ends here
