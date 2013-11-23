(defvar window-change-notify-function 'ignore)
(defvar window-change-notify-format 'characters)

(defun window-change-notify-hook ()
  (apply 'window-change-notify-function
         (if (eq window-change-notify-format 'pixels)
             (list (es-window-inside-pixel-width)
                   (es-window-inside-pixel-height))
           (list (window-body-width)
                 (window-body-height)))))

;; -----------------------------------------------------------------------------

(define-minor-mode window-change-notify-minor-mode
    nil nil nil nil
    (if window-change-notify
        (add-hook 'window-configuration-change-hook 'window-change-notify-hook nil t)
      (remove-hook 'window-configuration-change-hook 'window-change-notify-hook t)))

;; -----------------------------------------------------------------------------

(define-derived-mode window-change-notify-mode special-mode
    "Window change notify mode"
    "Window change notify mode"
  (erase-buffer)
  (insert "$\n")
  (add-hook 'window-configuration-change-hook 'window-change-notify-hook nil t)
  )

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

(define-minor-mode $-mode
    nil nil nil nil
    (setq-local window-change-notify-function)
    (if $-mode
        (window-change-notify)
      (window-change-notify -1)))

(provide 'window-change-notify)
;;; win-change-notify.el ends here
