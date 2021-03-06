(in-package :flee-the-deep)

(defclass tile ()
  ((display-char
    :type 'standard-char
    :initarg :display-char
    :initform #\.
    :reader display-char)
   (occupiable
    :type 'boolean
    :initarg :occupiable
    :initform t
    :reader occupiable)
   (color
    :initarg :color
    :initform +blue/black+
    :reader color)
   (exit
    :type 'boolean
    :initarg :exit
    :initform nil
    :reader exit)))

(defun make-wall-tile (char)
  (make-instance 'tile :display-char char :occupiable nil :color +white/black+))

(defun make-exit-tile ()
  (make-instance 'tile :display-char #\> :occupiable t :color +white/black+ :exit t))

(defun place-exit (map-arr)
  (let ((bottom-x (- (array-dimension map-arr 0) 2))
        (bottom-y (- (array-dimension map-arr 1) 2)))
    (setf (aref map-arr bottom-x bottom-y) (make-exit-tile))))

(defun get-orientation (width height)
  (cond
    ((< width height) 'horizontal)
    ((< height width) 'vertical)
    (t (if (= 0 (random 2))
           'horizontal
           'vertical))))

(defun gen-maze (map-arr
                 x y
                 width height
                 resolution
                 floor-tile h-wall-tile v-wall-tile)
  (when (or (<= width resolution)
            (<= height resolution))
    (return-from gen-maze))
  (let* ((orientation (get-orientation width height))
         (wall-x (+ x (if (eq orientation 'horizontal)
                          0
                          (+ (random (- width 2)) 1))))
         (wall-y (+ y (if (eq orientation 'horizontal)
                          (+ (random (- height 2)) 1)
                          0)))
         (door-x (+ wall-x (if (eq orientation 'horizontal)
                               (random (1- width))
                               0)))
         (door-y (+ wall-y (if (eq orientation 'horizontal)
                               0
                               (random (1- height)))))
         (dx (if (eq orientation 'horizontal) 1 0))
         (dy (if (eq orientation 'horizontal) 0 1))
         (wall-length (if (eq orientation 'horizontal) width height)))
    
    ;; draw the wall
    (dotimes (i wall-length)
      (setf (aref map-arr wall-x wall-y)
            (if (eq orientation'horizontal)
                h-wall-tile
                v-wall-tile))
      (incf wall-x dx)
      (incf wall-y dy))

    
    (setf (aref map-arr door-x door-y) floor-tile)
    (setf (aref map-arr
                (if (eq orientation 'horizontal)
                    (1+ door-x)
                    door-x)
                (if (eq orientation 'horizontal)
                    door-y
                    (1+ door-y)))
          floor-tile)
    (let ((nx x)
          (ny y)
          (w (if (eq orientation 'horizontal)
                 width
                 (- wall-x x)))
          (h (if (eq orientation 'horizontal)
                 (- wall-y y)
                 height)))
      (gen-maze map-arr
                nx ny
                w h
                resolution
                floor-tile h-wall-tile v-wall-tile))
    (let ((nx (if (eq orientation 'horizontal)
                  x
                  (1+ wall-x)))
          (ny (if (eq orientation 'horizontal)
                  (1+ wall-y)
                  y))
          (w (if (eq orientation 'horizontal)
                 width
                 (1- (- (+ x width) wall-x))))
          (h (if (eq orientation 'horizontal)
                 (1- (- (+ y height) wall-y))
                 height)))
      (gen-maze map-arr
                nx ny
                w h
                resolution
                floor-tile h-wall-tile v-wall-tile))))
