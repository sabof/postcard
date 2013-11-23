(defvar-local window-change-notify-function 'ignore)
(defvar-local window-change-notify-format 'characters)
(defvar-local window-change-notify-window-alist nil)

;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------

(defmacro wcn/acons (key value place)
  `(let ((existing (assoc ',key ,place)))
     (if existing
         (setcdr existing ,value)
       (setq ,place
             (acons ',key ,value ,place)))))


;; -----------------------------------------------------------------------------

(defun window-change-notify-hook ()
  (let (( string (with-temp-buffer
                   (apply 'window-change-notify-function
                          (if (eq window-change-notify-format 'pixels)
                              (list (es-window-inside-pixel-width)
                                    (es-window-inside-pixel-height))
                            (list (window-body-width)
                                  (window-body-height))))
                   (buffer-string))))

    ))

;; -----------------------------------------------------------------------------

(define-derived-mode window-change-notify-mode special-mode
    "Window change notify mode"
    "Window change notify mode"
  (erase-buffer)
  (insert "$\n")
  (add-hook 'window-configuration-change-hook
            'window-change-notify-hook nil t)
  (window-change-notify-hook)
  )

;; -----------------------------------------------------------------------------
;; Example
;; -----------------------------------------------------------------------------

(defun $-fill (width height)
  (erase-buffer)
  (insert "\n")
  (cl-loop repeat (- height 2) do
           (insert " ")
           (cl-loop repeat (- width 2) do
                    (insert "$"))
           (insert "\n")
           ))

(define-derived-mode $-mode window-change-notify-mode
    "$" "$"
  (setq window-change-notify-function)
  (if $-mode
      (window-change-notify)
    (window-change-notify -1)))

(provide 'window-change-notify)
;;; win-change-notify.el ends here
