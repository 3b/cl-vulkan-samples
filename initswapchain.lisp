(in-package #:cl-vulkan-samples)

(defmacro with-wsi-surface ((surface-var) &body body)
  `(with-win32-surface-khr (,surface-var (info :instance)
                                 (window-display/hinstance/etc (info :window))
                                 (window-id (info :window)))
     (setf (info :surface) ,surface-var)
     ,@body))

(defun init-swapchain-sample ()
  (init-global-layer-properties)

  ;; Set up swapchain:
  ;; - Get supported uses for all queues
  ;; - Try to find a queue that supports both graphics and present
  ;; - If no queue supports both, find a present queue and make sure we have a
  ;;   graphics queue
  ;; - Get a list of supported formats and use the first one
  ;; - Get surface properties and present modes and use them to create a swap
  ;;   chain
  ;; - Create swap chain buffers
  ;; - For each buffer, create a color attachment view and set its layout to
  ;;   color attachment
  (with-sample-instance (instancae :app "cl-vulkan-samples init-command-buffer")
    (init-enumerate-device)
    (with-window win (512 512 :title "Swapchain Initialization Sample")
      (with-wsi-surface (surface)
        ;; once we have a window, we can attach a vulkan "surface" to
        ;; it using the platform specific WSI extensions (already
        ;; loaded by WITH-SAMPLE-INSTANCE)

        ;; then select queues with support for drawing to that surface
        (select-presentable-graphics-queue)

        ;; create a device and queues
        (with-sample-device (device)
          (run-event-loop win (lambda ()) :seconds 1))))






    ))
