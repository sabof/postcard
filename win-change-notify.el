;; -*- lexical-binding: t -*-
(defvar-local window-change-notify-function 'ignore)
(defvar-local window-change-notify-format 'characters)
(defvar-local window-change-notify-window-alist nil)

;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------

(defmacro wcn/acons (key value place)
  `(let ((existing (assoc ,key ,place)))
     (if existing
         (progn
           (when (overlayp (cdr existing))
             (delete-overlay (cdr existing)))
           (setcdr existing ,value))
       (setq ,place (cl-acons ,key ,value ,place)))))

(defun wcn/clear-overlays ()
  (delete-all-overlays)
  (setq window-change-notify-window-alist))

(defun wcn/redraw-all-windows ()
  (wcn/clear-overlays)
  (cl-dolist (window (get-buffer-window-list nil nil t))
    (with-selected-window window
      (window-change-notify-hook))))

;; -----------------------------------------------------------------------------

(cl-defun window-change-notify-hook ()
  (let* (( redraw-func window-change-notify-function)
         ( string (with-temp-buffer
                    (apply redraw-func
                           (if (eq window-change-notify-format 'pixels)
                               (list (es-window-inside-pixel-width)
                                     (es-window-inside-pixel-height))
                             (list (window-body-width)
                                   (window-body-height))))
                    ;; "abc"
                    (buffer-string)
                    ))
         ( ov (make-overlay (point-min) (point-max))))
    (overlay-put ov 'display string)
    (overlay-put ov 'window 'selected-window)
    (wcn/acons (selected-window)
               ov
               window-change-notify-window-alist)
    (set-window-start nil (point-min))
    ))

;; -----------------------------------------------------------------------------

(define-derived-mode window-change-notify-mode special-mode
    "Window change notify mode"
    "Window change notify mode"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "$\n"))
  (add-hook 'window-configuration-change-hook
            'window-change-notify-hook nil t)
  (wcn/redraw-all-windows))

;; -----------------------------------------------------------------------------
;; Example
;; -----------------------------------------------------------------------------

(defun $-fill (width height)
  (insert "\n")
  (cl-loop repeat (- height 2) do
           (insert " ")
           (cl-loop repeat (- width 2) do
                    (insert "$"))
           (insert "\n")
           ))

(define-derived-mode $-mode window-change-notify-mode
    "$" "$"
  (setq window-change-notify-function '$-fill)
  (wcn/redraw-all-windows))

(provide 'window-change-notify)
;;; win-change-notify.el ends here
