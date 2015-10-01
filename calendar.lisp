;;;; A calendar based on the fact there are 52 cards in a deck, and 52
;;;; weeks in a year

(asdf:load-system "cl-ppcre")

;;;; Plan
;;;
;;; figure out the days of a month
;;; add up all days in the months until present month
;;; add the final days
;;; divide by 52 and use that as an index into card deck
;;; figure out how many days into that card we are

;;; Leap years and infrastructure data structures
;;;

(defun leap-yearp (year)
  "Predicate checking true for leap years"
  (cond
    ((not (zerop (mod (parse-integer year) 4)))     nil)
    ((not (zerop (mod (parse-integer year) 100)))   t)
    ((not (zerop (mod (parse-integer year) 400)))   nil)
    (t                                              t)))

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


;;; worker functions

(defun sum-days-to-month (month yr)
"Sums the number of days until MONTH, exclusive"
  (let ((sum 0)
	(month-name (nth (1- month) months)))
    ;; sometimes I wonder if LOOP is butt-ugly, or just neat...
    (loop :for m :in months
	  :until (string= (first (assoc m number-of-days :test #'string=)) month-name)
	  ;; just sum the days of each month, except February which need extra tests
	  :if (string= m "February")
	    :do (setf sum (+ sum (days-in-february yr)))
	  :else
	    :do (setf sum (+ sum (rest (assoc m number-of-days :test #'string=))))
	  :end)
    sum))

(defun sum-days-until-now (d m y)
  "Sums the Days until this Month (exclusive), 
adding the Days of the present Month, adding 
leap day if needed."
  ;; hiding the string->integer conversion here
  ;; in order to make top function a bit prettier
  (+ (parse-integer d) (sum-days-to-month (parse-integer m) y)))


;;; top function
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
    (sum-days-until-now day month year)))

