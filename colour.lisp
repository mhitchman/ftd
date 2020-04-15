(in-package :flee-the-deep)

; pinched from http://turtleware.eu/posts/cl-charms-crash-course.html
(defmacro define-color-pair ((name pair) foreground background)
  `(progn
     (defparameter ,name
       (progn (charms/ll:init-pair ,pair ,foreground ,background)
              (charms/ll:color-pair ,pair)))))


(defun start-color ()
  (when (eql (charms/ll:has-colors) charms/ll:FALSE)
    (error "Your terminal does not support color."))
  (let ((ret-code (charms/ll:start-color)))
    (if (= ret-code 0) T
        (error "start-color error ~s." ret-code))))

(defun setup-colors ()
  (start-color)
  (define-color-pair (+white/black+ 1) charms/ll:color_white charms/ll:color_black)
  (define-color-pair (+red/black+ 2) charms/ll:color_red charms/ll:color_black)
  (define-color-pair (+grey/black+ 3) charms/ll:color_magenta charms/ll:color_black)
  (define-color-pair (+yellow/black+ 4) charms/ll:color_yellow charms/ll:color_black)
  ;; who the hell needs magenta anyway. Grey is way more useful
  (charms/ll:init-color charms/ll:color_magenta 132 132 132)
  (charms/ll:init-color charms/ll:color_red 252 5 5))

 ;; nabbed from http://www.tldp.org/HOWTO/NCURSES-Programming-HOWTO/color.html
(defmacro with-color (color &body body)
  (alexandria:once-only (color)
    `(unwind-protect
          (progn
            (charms/ll:attron ,color)
            ,@body)
       (charms/ll:attroff ,color))))
