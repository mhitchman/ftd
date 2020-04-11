(defpackage :flee-the-deep
  (:use :common-lisp)
  (:export #:main))

(in-package :flee-the-deep)

(defclass tile ()
  ((display-char
    :type 'standard-char
    :initform #\.
    :accessor display-char)
   (occupiable
    :type 'boolean
    :initform t
    :accessor occupiable)))

(defclass creature ()
  ((name
    :initarg :name
    :reader name)
   (x
    :initarg :x-coord
    :accessor x-coord)
   (y
    :initarg :y-coord
    :accessor y-coord)
   (display-char
    :initarg :display-char
    :reader display-char)))

(defun make-creature (name x y display-char)
  (make-instance 'creature :name name :x-coord x :y-coord y :display-char display-char))

(defparameter *player* (make-creature 'player 0 0 #\@))

(defun draw-map (map-arr window)
  (destructuring-bind (height width) (array-dimensions map-arr)
    (dotimes (x height)
      (dotimes (y width)
        (charms:write-char-at-point
         window
         (display-char (aref map-arr x y))
         x y)))))

(defun draw-player (window)
  (charms:write-char-at-point window (display-char *player*) (x-coord *player*) (y-coord *player*)))

(defun draw (map-arr window)
  (draw-map map-arr window)
  (draw-player window))

(defun check-destination-tile (x y map-arr)
  (if (or (> x (array-dimension map-arr 0))
          (< x 0)
          (> y (array-dimension map-arr 1))
          (< y 0))
      nil
      (occupiable (aref map-arr x y))))

(defun move-player (x y map-arr)
  (let ((new-x (+ x (x-coord *player*)))
        (new-y (+ y (y-coord *player*))))
    (when (check-destination-tile new-x new-y map-arr)
      (with-accessors ((x x-coord) (y y-coord)) *player*
        (setf x new-x y new-y)))))

(defun process-input (input-char map-arr)
  (case input-char
    ((#\w) (move-player 0 -1 map-arr))
    ((#\s) (move-player 0 1 map-arr))
    ((#\a) (move-player -1 0 map-arr))
    ((#\d) (move-player 1 0 map-arr))
    ((#\q) 'quit)))

(defun game-loop (map-arr)
  (draw map-arr charms:*standard-window*)
  (process-input
   (charms:get-char charms:*standard-window*) map-arr))

(defun main ()
  (charms:with-curses ()
    (charms/ll:curs-set 0)
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:clear-window charms:*standard-window*)
    (multiple-value-bind (height width)
        (charms:window-dimensions charms:*standard-window*)
      (let ((map-arr (make-array
                      `(,height ,width)
                      :element-type 'tile)))
        ;; initialize the array with differenct elements
        (dotimes (x (array-dimension map-arr 0))
          (dotimes (y (array-dimension map-arr 1))
            (setf (aref map-arr x y) (make-instance 'tile))))
        (loop named game-loop do
          (when (eq 'quit
                    (game-loop map-arr))
            (return-from game-loop))
          (charms:refresh-window charms:*standard-window*))))))
