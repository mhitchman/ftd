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
    :reader display-char)
   (color
    :initarg :color
    :reader color)))

(defun make-creature (name x y display-char color)
  (make-instance 'creature :name name :x-coord x :y-coord y :display-char display-char :color color))

(defun make-beast (map-arr)
  (destructuring-bind (width height) (array-dimensions map-arr)
    (let ((red-width (floor (/ width 2)))
          (start-x)
          (start-y))
      ;; get a random empty starting position on the right side of screen
      (loop named rand-start
            for x = (+ (random red-width) red-width)
            for y = (random height)
            if (occupiable (aref map-arr x y))
              do (setf start-x x
                       start-y y)
              and do (return-from rand-start))
      (make-creature "beast" start-x start-y #\B +red/black+))))

(defvar *player*)
(defvar *beast*)

(defun safe-write-char (window char x y)
  "Bounds checking write-char-at-point"
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (if (and (< x width)
             (< y height))
        (charms:write-char-at-point window char x y)
        nil)))


(defun p-buff (length)
  (if (oddp length)
      (floor (/ length 2))
      (1- (/ length 2))))

(defun in-view (map-arr window)
  "Get the area of the map the window can view centred on the player"
  (multiple-value-bind (w-width w-height) (charms:window-dimensions window)
    (let* ((x-offset (p-buff w-width))
           (y-offset (p-buff w-height))
           (x (- (x-coord *player*) x-offset))
           (y (- (y-coord *player*) y-offset))
           (width w-width)
           (height w-height))
      (alexandria:maxf x 0)
      (alexandria:maxf y 0)
      (alexandria:minf width (array-dimension map-arr 0))
      (alexandria:minf height (array-dimension map-arr 1))
      (list x y width height))))


(defun draw-map (map-arr window corn-x corn-y width height)
  (dotimes (dx width)
    (let ((current-x (+ corn-x dx)))
      (dotimes (dy height)
        (let ((current-y (+ corn-y dy)))
          (when (and (< current-x (array-dimension map-arr 0))
                     (< current-y (array-dimension map-arr 1)))
            (with-color (color (aref map-arr current-x current-y))
              (safe-write-char
               window
               (display-char (aref map-arr current-x current-y))
               dx
               dy))))))))

(defun between-p (num min max)
  (and (<= min num) (<= num max)))


(defun draw-creature (creature window
                      corn-x corn-y
                      view-width view-height)
  (let ((creature-x (x-coord creature))
        (creature-y (y-coord creature)))
    (when (and (between-p creature-x corn-x (+ corn-x (1- view-width)))
               (between-p creature-y corn-y (+ corn-y (1- view-height))))
      (with-color (color creature)
        (safe-write-char
         window
         (display-char creature)
         (- creature-x corn-x)
         (-  creature-y corn-y))))))


(defun draw (map-arr window)
  (charms:clear-window window)
  (destructuring-bind (x y view-width view-height)
      (in-view map-arr window)
    (draw-map map-arr window x y view-width view-height)
    (draw-creature *player* window x y view-width view-height)
    (draw-creature *beast* window x y view-width view-height)))


(defun check-destination-tile (x y map-arr)
  (if (or (> x (1- (array-dimension map-arr 0)))
          (< x 0)
          (> y (1- (array-dimension map-arr 1)))
          (< y 0))
      nil
      (occupiable (aref map-arr x y))))

(defun move-player (x y map-arr)
  (let ((new-x (+ x (x-coord *player*)))
        (new-y (+ y (y-coord *player*))))
    (when (check-destination-tile new-x new-y map-arr)
      (with-accessors ((x x-coord) (y y-coord)) *player*
        (setf x new-x y new-y)))))

(defun calc-slope (x1 y1 x2 y2)
  (let ((y (- y1 y2))
        (x (- x1 x2)))
    (if (or (zerop y) (zerop x))
        0
        (/ y x))))


(defun al-player-in-sight-p (map-arr)
  (let ((player-x (x-coord *player*))
        (player-y (y-coord *player*))
        (beast-x (x-coord *beast*))
        (beast-y (y-coord *beast*)))
    (if (loop named ray with v = 0.01 do
      (let ((x (round (alexandria:lerp v beast-x player-x)))
            (y (round (alexandria:lerp v beast-y player-y))))
        (cond ((not (occupiable (aref map-arr x y)))
               (return-from ray nil))
              ((and (= player-x x) (= player-y y))
               (return-from ray
                 (values t))))
        (incf v 0.01)))
        (values t (list (+ beast-x (alexandria:clamp
                                    (- player-x beast-x) -1 1))
                        (+ beast-y (alexandria:clamp
                                    (- player-y beast-y) -1 1))))
        (values nil '(0 0)))))

(defun player-in-sight-p (map-arr)
  "Draw a line to the player and see if anything is in the way"
  (let* ((player-x (x-coord *player*))
         (player-y (y-coord *player*))
         (beast-x (x-coord *beast*))
         (beast-y (y-coord *beast*))
         (dir (alexandria:clamp (- player-x beast-x) -1 1))
         (slope (calc-slope player-x player-y beast-x beast-y)))
    (loop named ray
          with x = (+ beast-x dir)
          for y = (+ beast-y (round (* x slope))) do
            (cond ((not (occupiable (aref map-arr x y)))
                   (return-from ray
                     (values nil (list 0 0))))
                  ((and (= player-x x)
                        (= player-y y))
                   (return-from ray
                     (values t (list dir (alexandria:clamp (- player-y beast-y) -1 1)))))
                  (t (incf x dir))))))


(defun player-down-p ()
  (> (y-coord *player*) (y-coord *beast*)))

(defun player-up-p ()
  (< (y-coord *player*) (y-coord *beast*)))

(defun player-right-p ()
  (> (x-coord *player*) (x-coord *beast*)))

(defun player-left-p ()
  (< (x-coord *player*) (x-coord *beast*)))


(defun weighted-random (dirs)
  (let ((new-direction (loop for dir in dirs
                             if (cdr dir)
                               return dir)))
    (if new-direction
        (car new-direction)
        (car (nth (random (length dirs)) dirs)))))

(defun beast-random-pos (map-arr)
  "Returns a random position that the beast can move to that is occupiable"
  (let* ((dir `(((0 1) . ,(player-down-p))
                ((0 -1) . ,(player-up-p))
                ((1 0) . ,(player-right-p))
                ((-1 0) . ,(player-left-p))))
         (beast-pos (list (x-coord *beast*) (y-coord *beast*)))
         (new-positions
           (loop for x in dir
                 collect (cons (mapcar #'+ (car x) beast-pos) (cdr x)))))
    (setf new-positions (remove-if
                         (lambda (position)
                           (not (occupiable
                                 (aref map-arr (car position) (cadr position))))) new-positions :key #'car))
    (weighted-random new-positions)))


(defun move-beast (map-arr)
  (multiple-value-bind (in-sight direction) (al-player-in-sight-p map-arr)
    (destructuring-bind (new-x new-y) (if in-sight
                                          direction
                                          (beast-random-pos map-arr))
      (when (occupiable (aref map-arr new-x new-y))
        (setf (x-coord *beast*) new-x
              (y-coord *beast*) new-y)))))

(defun process-input (input-char map-arr)
  (case input-char
    ((#\w) (move-player 0 -1 map-arr))
    ((#\s) (move-player 0 1 map-arr))
    ((#\a) (move-player -1 0 map-arr))
    ((#\d) (move-player 1 0 map-arr))
    ((#\q) 'quit)))

(defun game-over-p ()
  "Game over when beast and player occupy the same space"
  (and (= (x-coord *beast*) (x-coord *player*))
       (= (y-coord *beast*) (y-coord *player*))))

(defconstant +map-width+ 40)
(defconstant +map-height+ 40)

(defun run-game ()
  (let ((map-arr (make-array (list +map-width+ +map-height+)
                             :element-type 'tile
                             :initial-element (make-instance 'tile)))
        (border-wall-tile (make-wall-tile #\#))
        (h-wall-tile (make-wall-tile #\-))
        (v-wall-tile (make-wall-tile #\|))
        (*player* (make-creature 'player 1 1 #\@ +yellow/black+)))
    (create-border map-arr border-wall-tile)
    (gen-maze map-arr 1 1
              (- +map-width+ 2)
              (- +map-height+ 2)
              3
              (aref map-arr 1 1)
              h-wall-tile
              v-wall-tile)
    (place-exit map-arr)
    (setf *beast* (make-beast map-arr))
    (loop named game-loop for result = (game-loop map-arr) do
      (progn (charms:refresh-window charms:*standard-window*)
             (cond ((eq 'quit result) (return-from game-loop))
                   ((eq 'game-over result)
                    (display-game-over)
                    (return-from game-loop))
                   ((eq 'won result)
                    (display-you-won)
                    (return-from game-loop)))))))

(defun left-mazep (map-arr)
  (with-accessors ((x x-coord) (y y-coord)) *player*
    (exit (aref map-arr x y))))

(defun game-loop (map-arr)
  (draw map-arr charms:*standard-window*)
  (let ((action
          (process-input
           (charms:get-char charms:*standard-window* :ignore-error t)
           map-arr)))
    (when (left-mazep map-arr) (return-from game-loop 'won))
    (move-beast map-arr)
    (when (game-over-p)
      (return-from game-loop 'game-over))
    action))

(defun create-border (map-arr tile)
  (destructuring-bind (width height) (array-dimensions map-arr)
    (dotimes (x width)
      (setf (aref map-arr x 0) tile
            (aref map-arr x (1- height)) tile))
    (dotimes (y height)
      (setf (aref map-arr 0 y) tile
            (aref map-arr (1- width) y) tile))))

(defun centre-format-string (width text)
  (format nil "~~~a:@<~a~~>" width text))

(defun display-message (message)
  (charms:clear-window charms:*standard-window*)
  (multiple-value-bind (width height) (charms:window-dimensions charms:*standard-window*)
    (let ((format-string (centre-format-string width message))
          (y (floor (/ height 2))))
      (charms:write-string-at-point charms:*standard-window*
                                    (format nil format-string) 0 y)))
  (charms:refresh-window charms:*standard-window*)
  (sleep 1)
  (charms:get-char charms:*standard-window*))


(defun display-game-over ()
  (display-message "Game Over!"))


(defun display-you-won ()
  (display-message "You Escaped!"))


(defun display-title-screen ()
  (let ((width (charms:window-dimensions charms:*standard-window*)))
    (charms:write-string-at-point charms:*standard-window* (format nil (centre-format-string width "Flee the Deep!")) 0 3)
    (charms:write-string-at-point charms:*standard-window* (format nil (centre-format-string width "w,a,s,d to move")) 0 6)
    (charms:write-string-at-point charms:*standard-window* (format nil (centre-format-string width "q to quit")) 0 7)
    (charms:write-string-at-point charms:*standard-window* (format nil (centre-format-string width "Press any key to start!")) 0 9))
  (charms:refresh-window charms:*standard-window*)
  (sleep 0.5)
  (charms:get-char charms:*standard-window* :ignore-error t))


(defun main ()
  (charms:with-curses ()
    (charms/ll:curs-set 0)
    (setup-colors)
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:clear-window charms:*standard-window*)
    (display-title-screen)
    (run-game)))
