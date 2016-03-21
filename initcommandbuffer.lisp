(in-package #:cl-vulkan-samples)

(defun init-command-buffer-sample
  (init-global-layer-properties)
  (init-instance-extension-names)

  (with-sample-instance (instancae :app "cl-vulkan-samples init-command-buffer")
    (with-debug-report ()
     (let ((*info* (list :instance instancae)))

       (init-enumerate-device)
       (select-queue-family)
       (vk:with-device (device (info :gpu)
                        ;; using default queue priority of 0.0
                        :queue-family-index (info :graphics-queue-family))
         (with-window (win (512 512 :title "Command Buffer Sample")))





;;    init_global_layer_properties(info);
;;    init_instance_extension_names(info);
;;    init_device_extension_names(info);
;;    init_instance(info, sample_title);
;;    init_enumerate_device(info);
;;    init_window_size(info, 500, 500);
;;    init_connection(info);
;;    init_window(info);
    init_swapchain_extension(info);
    init_device(info);


         )))))
