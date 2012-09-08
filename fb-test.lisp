(defpackage :fb-test
  (:use :cl :ccl :vecto)
  (:export #:test-graphics))

(in-package :fb-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-interface-dir :linux))

(defun ioctl-integer (fd command value)
  (let ((retval (#_ioctl fd command :signed-fullword value)))
    (when (< 0 retval)
      (ccl::%errno-disp (ccl::%get-errno)))
    retval))

(defun ioctl-pointer (fd command pointer)
  (let ((retval (#_ioctl fd command :address pointer)))
    (unless (zerop retval)
      (ccl::%errno-disp (ccl::%get-errno)))
    retval))

(defmacro with-open-fd ((var path) &body body)
  `(let ((,var (ccl::fd-open ,path #$O_RDWR)))
     (unless (< -1 ,var)
       (ccl::%errno-disp (ccl::%get-errno)))
     (unwind-protect
          (progn ,@body)
       (#_close ,var))))

(defun say (fmt &rest args)
  (format t "~?~%" fmt args)
  (finish-output))

(defun framebuffer-info ()
  (with-open-fd (fb "/dev/fb0")
    (rletz ((screeninfo :fb_var_screeninfo))
      (ioctl-pointer fb #$FBIOGET_VSCREENINFO screeninfo)
      (list :xres (pref screeninfo :fb_var_screeninfo.xres)
            :yres (pref screeninfo :fb_var_screeninfo.yres)
            :bits-per-pixel (pref screeninfo :fb_var_screeninfo.bits_per_pixel)))))

(defun make-color (red green blue)
  (declare (fixnum red green blue) (optimize speed (safety 0)))
  (logior (ldb (byte 5 3) blue)
          (ash (ldb (byte 6 2) green) 5)
          (ash (ldb (byte 6 3) red) 11)))

(defclass framebuffer ()
  ((width :reader width
          :initarg :width)
   (height :reader height
           :initarg :height)
   (pointer :reader pointer%
            :initarg :pointer)
   (screensize :reader screensize%
               :initarg :screensize)))

(defun open-framebuffer (&key (background-color '(0 0 0)))
  (with-open-fd (tty "/dev/tty1")
    (say "switching to graphics mode")
    (ioctl-integer tty #$KDSETMODE #$KD_GRAPHICS))
  (destructuring-bind (&key xres yres bits-per-pixel) (framebuffer-info)
    (assert (= bits-per-pixel 16))
    (with-open-fd (fb "/dev/fb0")
      (let* ((screensize (/ (* xres yres bits-per-pixel) 8))
             (framebuffer (make-instance 'framebuffer
                                         :width xres
                                         :height yres
                                         :pointer (#_mmap (%null-ptr) screensize (logior #$PROT_READ #$PROT_WRITE) #$MAP_SHARED fb 0)
                                         :screensize screensize)))
        (#_memset (pointer% framebuffer) (apply #'make-color background-color) screensize) 
        framebuffer))))

(defun close-framebuffer (framebuffer)
  (#_munmap (pointer% framebuffer) (screensize% framebuffer))
  (with-open-fd (tty "/dev/tty1")
    (say "switching to text mode")
    (ioctl-integer tty #$KDSETMODE #$KD_TEXT)))

(defmacro with-framebuffer ((framebuffer &rest args) &body body)
  `(let ((,framebuffer (apply #'open-framebuffer ,args)))
     (unwind-protect
          (progn ,@body)
       (close-framebuffer ,framebuffer))))

(defun copy-image-to-framebuffer (image framebuffer)
  (assert (eql (zpng:color-type image) :truecolor-alpha))
  (let ((image-data (zpng:image-data image))
        (framebuffer-data (pointer% framebuffer)))
    (dotimes (y (zpng:height image))
      (dotimes (x (zpng:width image))
        (let ((png-offset (ash (+ x (* y (zpng:width image))) 2))
              (framebuffer-offset (ash (+ x (* y (width framebuffer))) 1)))
          (setf (%get-unsigned-word framebuffer-data framebuffer-offset)
                (make-color (aref image-data png-offset)
                            (aref image-data (+ png-offset 1))
                            (aref image-data (+ png-offset 2)))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; demo
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun radiant-lambda (framebuffer)
  (with-canvas (:width (width framebuffer) :height (height framebuffer))
    (let ((din-font (get-font "/home/pi/DINRg___.ttf"))
          (times-font (get-font "/home/pi/times.ttf"))
          (step (/ pi 7)))
      (set-rgb-stroke 1.0 1.0 1.0)
      (set-rgb-fill 1.0 1.0 1.0)
      (set-font din-font 80)
      (draw-string 0 100 "LisPi")
      (set-font din-font 30)
      (draw-string 0 600 "Rasperry Pi running")
      (set-font din-font 20)
      (draw-string 0 580 (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))
      (draw-string 0 560 "Drawn using VECTO")
      (scale 7 7)
      (translate 120 45)
      (set-font times-font 40)
      (draw-centered-string 0 -10 #(#x3BB))
      (set-rgb-stroke 1 0 0)
      (centered-circle-path 0 0 35)
      (stroke)
      (set-rgba-stroke 1.0 1.0 1.0 0.5)
      (set-line-width 4)
      (dotimes (i 14)
        (with-graphics-state
          (rotate (* i step))
          (move-to 30 0)
          (line-to 40 0)
          (stroke)))
      (copy-image-to-framebuffer (vecto::image vecto::*graphics-state*) framebuffer))))

(defun test-graphics ()
  (with-framebuffer (fb)
    (radiant-lambda fb)
    (read-line)))
