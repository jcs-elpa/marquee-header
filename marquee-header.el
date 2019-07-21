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
  :greedy 'marquee-header)


(defvar-local marquee-header--message ""
  "Current message.")

(defvar-local marquee-header--time 0.0
  "Current show time.")

(defvar-local marquee-header--speed 0.0
  "The animation speed for time calculation.")

(defvar-local marquee-header--timer nil
  "Timer pointer for updating marquee animation.")

(defvar-local marquee-header--direction nil
  "Record the marquee direction.")


(defun marquee-header--display-header ()
  "Display the header animation."
  (if marquee-header--speed
      (progn

        (setq marquee-header--timer
              (run-with-idle-timer marquee-header--speed
                                   nil
                                   marquee-header--display-header)))

    ))

;;;###autoload
(defun marquee-header-notify (msg &optional time direction)
  "Show the marquee notification with MSG.  TIME is the time that will show on
screen. DIRECTION is for marquee animation."
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
  (setq marquee-header--speed (if (equal marquee-header--direction 'none)
                                  nil
                                (/ marquee-header--time (window-width))))
  (marquee-header--display-header))


(provide 'marquee-header)
;;; marquee-header.el ends here
