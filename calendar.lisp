;;;; A calendar based on the fact there are 52 cards in a deck, and 52
;;;; weeks in a year

(defun calendar (date)
  (let ((result))
    (multiple-value-bind (year month day) (cl-ppcre:scan date)
      (setf result nil))
    result))

;;;; Plan
;;;
;;; figure out the days of a month
;;; add up all days in the months until present month
;;; add the final days
;;; divide by 52 and use that as an index into card deck
;;; figure out how many days into that card we are

;;; Leap years
