;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar-local window-change-notify-function 'ignore)
(defvar-local window-change-notify-format 'characters)
(defvar-local window-change-notify-window-alist nil)

;; -----------------------------------------------------------------------------
;; Utils
;; -----------------------------------------------------------------------------

(defun wcn/clear-overlays ()
  (delete-all-overlays)
  (setq window-change-notify-window-alist))

(defun wcn/cleanup ()
  (setq window-change-notify-window-alist
        (cl-remove-if-not
         (lambda (pair)
           (or (window-live-p (car pair))
               (progn
                 (delete-overlay (cdr (assq 'overlay (cdr pair))))
                 nil)))
         window-change-notify-window-alist)))

(defun wcn/redraw-all-windows ()
  (wcn/clear-overlays)
  (cl-dolist (window (get-buffer-window-list nil nil t))
    (with-selected-window window
      (window-change-notify-hook t))))

(defun wcn/post-command-hook ()
  (deactivate-mark)
  (set-window-start nil (point-min)))

;; -----------------------------------------------------------------------------

(cl-defun window-change-notify-hook (&optional force)
  ;; FIXME: This runs once for every window. Not tragic, but still unnecessary.
  (wcn/cleanup)
  (let* (( redraw-func window-change-notify-function)
         ( do-redraw force)
         ( alist (or (cdr (assq (selected-window)
                                window-change-notify-window-alist))
                     (let* (( temp-overlay (make-overlay (point-min) (point-max)))
                            ( temp-alist (list (cons 'window (selected-window))
                                               (cons 'width (window-width))
                                               (cons 'height (window-height))
                                               (cons 'overlay temp-overlay)
                                               )))
                       (overlay-put temp-overlay 'window (selected-window))
                       (setq window-change-notify-window-alist
                             (cl-acons (selected-window) temp-alist
                                       window-change-notify-window-alist))
                       (setq do-redraw t)
                       temp-alist
                       )))
         ( ov (cdr (assq 'overlay alist))))
    (unless do-redraw
      (setq do-redraw
            (or (not (equal (cdr (assq 'height alist)) (window-height)))
                (not (equal (cdr (assq 'width alist)) (window-width))))))
    (when do-redraw
      (overlay-put ov 'display
                   (with-temp-buffer
                    (funcall redraw-func)
                    (buffer-string)))
      (setf (cdr (assq 'height alist)) (window-height))
      (setf (cdr (assq 'width alist)) (window-width)))
    (set-window-start nil (point-min))
    ))

;; -----------------------------------------------------------------------------

(define-derived-mode window-change-notify-mode special-mode
    "Window change notify mode"
    "Window change notify mode"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "$\n"))
  (setq-local auto-window-vscroll nil)
  (setq-local cursor-type nil)
  (add-hook 'window-configuration-change-hook
            'window-change-notify-hook nil t)
  (add-hook 'post-command-hook
            'wcn/post-command-hook
            nil t)
  (wcn/redraw-all-windows))

;; -----------------------------------------------------------------------------
;; Example
;; -----------------------------------------------------------------------------

(defun $-fill ()
  (let ((width (window-body-width))
        (height (window-body-height)))
    (insert "\n")
    (cl-loop repeat (- height 2) do
             (insert " ")
             (cl-loop repeat (- width 2) do
                      (insert "$"))
             (insert "\n"))
    (add-text-properties
     (point-min) (point-max)
     (list 'face `(:foreground ,(es-color-random-hex))))))

(define-derived-mode $-mode window-change-notify-mode
    "$" "$"
  (setq window-change-notify-function '$-fill)
  (wcn/redraw-all-windows))

(provide 'window-change-notify)
;;; win-change-notify.el ends here
