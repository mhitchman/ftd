(defpackage :flee-the-deep
  (:use :common-lisp)
  (:export #:main))

(in-package :flee-the-deep)

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
         (aref map-arr x y)
         x y)))))

(defun draw-player (window)
  (charms:write-char-at-point window (display-char *player*) (x-coord *player*) (y-coord *player*)))

(defun draw (map-arr window)
  (draw-map map-arr window)
  (draw-player window))

(defun move-player (x y map-arr)
  (let ((new-x (+ x (x-coord *player*)))
        (new-y (+ y (y-coord *player*))))
    (when (ignore-errors (aref map-arr new-x new-y))
      (with-accessors ((x x-coord) (y y-coord)) *player*
        (setf x new-x y new-y)))))

(defun process-input (input-char map-arr)
  (case input-char
    ((#\w) (move-player 0 -1 map-arr))
    ((#\s) (move-player 0 1 map-arr))
    ((#\a) (move-player -1 0 map-arr))
    ((#\d) (move-player 1 0 map-arr))
    ((#\q) 'quit)))

(defun main ()
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:clear-window charms:*standard-window*)
    (multiple-value-bind (height width)
        (charms:window-dimensions charms:*standard-window*)
      (loop named game-loop do
        (when (eq 'quit (let ((map-arr (make-array
                                        `(,height ,width)
                                        :element-type 'standard-char
                                        :initial-element #\.)))
                          (draw map-arr charms:*standard-window*)
                          (process-input
                           (charms:get-char charms:*standard-window*)
                           map-arr)))
          (return-from game-loop))
        (charms:refresh-window charms:*standard-window*)))))
