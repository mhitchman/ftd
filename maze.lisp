(in-package "flee-the-deep")

(defun get-orientation (width height)
  (cond
    ((< width height) 'horizontal)
    ((< height width) 'vertical)
    (t (if (= 0 (random 2))
           'horizontal
           'vertical))))

(defun gen-maze (map-arr x y width height resolution floor-tile wall-tile)
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
                               (random width)
                               0)))
         (door-y (+ wall-y (if (eq orientation 'horizontal)
                               0
                               (random height))))
         (dx (if (eq orientation 'horizontal) 1 0))
         (dy (if (eq orientation 'horizontal) 0 1))
         (wall-length (if (eq orientation 'horizontal) width height)))
    ;; draw the wall
    (dotimes (i wall-length)
      (setf (aref map-arr wall-x wall-y) wall-tile)
      ;; (draw-map map-arr *stand-win*)
      ;; (charms:refresh-window *stand-win*)
      ;; (break)
      (incf wall-x dx)
      (incf wall-y dy))

    
    (setf (aref map-arr door-x door-y) floor-tile)

    (draw-map map-arr *stand-win*)
    (charms:refresh-window *stand-win*)
    (break)
    (let ((nx x)
          (ny y)
          (w (if (eq orientation 'horizontal)
                 width
                 (- wall-x x)))
          (h (if (eq orientation 'horizontal)
                 (- wall-y y)
                 height)))
      (gen-maze map-arr nx ny w h resolution floor-tile wall-tile))
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
      (gen-maze map-arr nx ny w h resolution floor-tile wall-tile))))
