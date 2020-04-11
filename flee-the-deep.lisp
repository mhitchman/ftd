(defpackage :flee-the-deep
  (:use :common-lisp)
  (:export #:main))

(in-package :flee-the-deep)

(defun draw (map-arr window)
  (destructuring-bind (height width) (array-dimensions map-arr)
    (dotimes (x height)
      (dotimes (y width)
        (charms:write-char-at-point
         window
         (aref map-arr x y)
         x y)))))

(defun main ()
  (charms:with-curses ()
    (charms:clear-window charms:*standard-window*)
    (multiple-value-bind (height width)
        (charms:window-dimensions charms:*standard-window*)
      (let ((map-arr (make-array
                      `(,height ,width)
                      :element-type 'standard-char
                      :initial-element #\.)))
        (draw map-arr charms:*standard-window*)))
    (charms:refresh-window charms:*standard-window*)
    (charms:get-char charms:*standard-window*)))
