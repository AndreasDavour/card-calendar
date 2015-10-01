;;;; A calendar based on the fact there are 52 cards in a deck, and 52
;;;; weeks in a year

(asdf:load-system "cl-ppcre")

(defun calendar (date)
  "Returns the card in a regular deck of cards,
matching the number of that week of the year, 
and the day in that week. Date expected in YYYY-MM-DD
format."
  (let* ((result)
	 (split-data (cl-ppcre:split "-" date))
	 (year (first split-data))
	 (month (second split-data))
	 (day (third split-data)))
;    (multiple-value-bind (year month day) (cl-ppcre:scan date)
					;     (setf result nil))
    split-data))

;;;; Plan
;;;
;;; figure out the days of a month
;;; add up all days in the months until present month
;;; add the final days
;;; divide by 52 and use that as an index into card deck
;;; figure out how many days into that card we are

;;; Leap years
;;;

(defun leap-yearp (year)
  "Predicate checking true for leap years"
  (cond
    ((not (zerop (mod year 4)))
     ; common year
     nil)
    ((not (zerop (mod year 100)))
     ; leap year
     t)
    ((not (zerop (mod year 400)))
     ; common year
     nil)
    (t
     ; leap year
     t)))

;;; days in a month
(defun days-in-february (year)
  (if (leap-yearp year)
      29
      28))

(defparameter number-of-days '(("January" . 31)
			       ("February" . 0)
			       ("March" . 31)
			       ("April" . 30)
			       ("May" . 31)
			       ("June" . 30)
			       ("July". 31)
			       ("August" . 31)
			       ("September" . 30)
			       ("October" . 31)
			       ("November" . 30)
			       ("December" . 31)))

(defparameter months '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

(defun sum-days-to-month (month yr)
"Sums the number of days until MONTH, exclusive"
  (let ((sum 0))
    (loop :for m :in months
	  :until (string= (first (assoc m number-of-days :test #'string=)) month)
	  :if (string= m "February")
	    :do (setf sum (+ sum (days-in-february yr)))
	  :else
	    :do (setf sum (+ sum (rest (assoc m number-of-days :test #'string=))))
	  :end)
    sum))
