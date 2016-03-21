(in-package #:cl-vulkan-samples)

(defclass sample-window (glop:window)
  ((exit :initform nil :accessor exit)
   (draw :initform nil :accessor draw)))

(defmethod glop:on-event ((w sample-window) (e glop:key-event))
  (format t "key pressed, closing window...~%")
  (setf (exit w) t)
  (glop:destroy-window w))


(defmacro with-window (window-var (width height &key (title "cl-vulkan-sample"))
                       &body body)
  `(glop:with-window (,window-var "vk" ,width ,height
                                  :win-class 'sample-window
                                  :title ,title
                                  ;; try to not initialize GL, but don't error
                                  ;; on unpatched glop
                                  :allow-other-keys t
                                  :gl nil)
     (setf (info :window) ,window-var)
     ,@body))

(defun run-event-loop (window render &key seconds)
  (loop with frames = 0
        with start = (get-internal-real-time)
        with timeout-start = (get-internal-real-time)
        until (exit window)
        until (and seconds (> seconds)
                   (/ (- (get-internal-real-time) timeout-start)
                      internal-time-units-per-second))
        while (glop:dispatch-events window :blocking nil :on-foo nil)
        do (with-simple-restart (continue "continue")
             (funcall render))
           (incf frames)
           (when (> (- (get-internal-real-time) start)
                    (* 2 internal-time-units-per-second))
             (format t "~s frames in ~s sec = ~s fps~%"
                     frames
                     (float
                      (/ (- (get-internal-real-time) start)
                         internal-time-units-per-second))
                     (float
                      (/ frames
                         (/ (- (get-internal-real-time) start)
                            internal-time-units-per-second))))
             (setf frames 0
                   start (get-internal-real-time)))))

;; return windowing system specific values to be passed to surface creation
(defun window-id (win)
  #+os-windows (glop:win32-window-id win)
  #+os-linux (glop:x11-window-id))

(defun window-display/hinstance/etc (win)
  #+os-windows (glop::win32-window-module-handle win)
  #+os-linux (glop:x11-window-display))
