;; -*- lexical-binding: t -*-
(require 'cl-lib)

(defvar-local window-change-notify-function 'ignore)
(defvar-local window-change-notify-format 'characters)
(defvar-local window-change-notify-window-alist nil)

(defvar window-change-notify-current-main-overlay nil)
(defvar window-change-notify-current-left-overlay nil)
(defvar window-change-notify-current-top-overlay nil)
(defvar window-change-notify-current-newline-overlay nil)
(defvar wcn/debug nil)

;; -----------------------------------------------------------------------------
;; Window Change Notify
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
                 (delete-overlay (cdr (assq 'top-overlay (cdr pair))))
                 (delete-overlay (cdr (assq 'newline-overlay (cdr pair))))
                 (delete-overlay (cdr (assq 'main-overlay (cdr pair))))
                 (delete-overlay (cdr (assq 'left-overlay (cdr pair))))
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

(defun wcn/revert ()
  (interactive)
  (funcall major-mode))

;; Relevant:
;; frame-set-background-mode
;; frame-background-mode (variable)
;; (custom-theme-set-variables
;;  'gruber-darker
;;  '(frame-brackground-mode (quote dark)))

;; Should be... but apparently isn't
;; (frame-terminal-default-bg-mode (selected-frame))

;; Also should be it, but seems unreliable
;; (custom-variable-theme-value 'frame-brackground-mode)

;;
;; (cl-every 'wcn/color-dark-p
;;           '("#555" "#500" "#050"))

;; (cl-notany 'wcn/color-dark-p
;;            '("#fff" "#ff0" "#faa"))

(defun wcn/color-dark-p (color)
  (< (cl-loop for color in (color-values color)
              maximizing color)
     ;; 65280 Maximum
     35280
     ))

(defun window-change-notify-set-top-margin (height &optional pixels)
  (cl-assert (and window-change-notify-current-newline-overlay
                  window-change-notify-current-top-overlay))
  (if (<= height 0)
      (progn
        (overlay-put window-change-notify-current-top-overlay
                     'invisible t)
        (overlay-put window-change-notify-current-newline-overlay
                     'invisible t)
        (overlay-put window-change-notify-current-top-overlay
                     'display '(space :width (0) :height (0)))
        (overlay-put window-change-notify-current-newline-overlay
                     'display '(space :width (0) :height (0))))
    (overlay-put window-change-notify-current-top-overlay
                 'display `(space :height ,(if pixels (list height) height)))
    (overlay-put window-change-notify-current-top-overlay
                 'invisible nil)

    (overlay-put window-change-notify-current-newline-overlay
                 'display nil)
    (overlay-put window-change-notify-current-newline-overlay
                 'invisible nil)
    ))

(defun window-change-notify-set-left-margin (width &optional pixels)
  (cl-assert window-change-notify-current-left-overlay)
  (if (<= width 0)

      (progn
        ;; Invisible doesn't seem to do anything
        ;; (overlay-put window-change-notify-current-left-overlay
        ;;              'invisible t)
        (overlay-put window-change-notify-current-left-overlay
                     'display '(space :width (0) :height (0))))

    (progn
      (overlay-put window-change-notify-current-left-overlay
                   'display `(space :width ,(if pixels (list width) width)))
      (overlay-put window-change-notify-current-left-overlay
                   'invisible nil)))
  ;; (setq tmp3 window-change-notify-current-left-overlay)
  )

(cl-defun window-change-notify-hook (&optional force)
  ;; FIXME: This runs once for every window. Not tragic, but still unnecessary.
  (wcn/cleanup)
  (let* (( redraw-func window-change-notify-function)
         ( do-redraw force)
         ( alist (or (cdr (assq (selected-window)
                                window-change-notify-window-alist))
                     (let* (( top-overlay-offset 1)
                            ( newline-overlay-offset 2)
                            ( left-overlay-offset 3)
                            ( main-overlay-offset 4)
                            ( temp-top-overlay
                              (make-overlay top-overlay-offset
                                            (1+ top-overlay-offset)))
                            ( temp-newline-overlay
                              (make-overlay newline-overlay-offset
                                            (1+ newline-overlay-offset)))
                            ( temp-left-overlay
                              (make-overlay left-overlay-offset
                                            (1+ left-overlay-offset)))
                            ( temp-main-overlay
                              (make-overlay main-overlay-offset
                                            (point-max)))
                            ( temp-alist (list (cons 'width (window-width))
                                               (cons 'height (window-height))
                                               (cons 'main-overlay temp-main-overlay)
                                               (cons 'left-overlay temp-left-overlay)
                                               (cons 'newline-overlay temp-newline-overlay)
                                               (cons 'top-overlay temp-top-overlay)
                                               )))

                       (overlay-put temp-main-overlay 'window (selected-window))

                       (overlay-put temp-top-overlay 'window (selected-window))
                       (overlay-put temp-left-overlay 'window (selected-window))
                       (overlay-put temp-newline-overlay 'window (selected-window))

                       ;; (overlay-put temp-top-overlay 'display `(space :height (51) :width 1))
                       ;; (overlay-put temp-left-overlay 'display `(space :height 1 :width (51)))

                       (overlay-put temp-top-overlay 'invisible t)
                       (overlay-put temp-left-overlay 'invisible t)
                       (overlay-put temp-newline-overlay 'invisible t)
                       (overlay-put temp-newline-overlay 'line-height t)

                       (setq window-change-notify-window-alist
                             (cl-acons (selected-window) temp-alist
                                       window-change-notify-window-alist))
                       (setq do-redraw t)
                       temp-alist
                       )))
         ( ov (cdr (assq 'main-overlay alist))))
    (unless do-redraw
      (setq do-redraw
            (or (not (equal (cdr (assq 'height alist)) (window-height)))
                (not (equal (cdr (assq 'width alist)) (window-width))))))
    (when do-redraw
      (let* (( window-change-notify-current-main-overlay ov)
             ( window-change-notify-current-left-overlay
               (cdr (assq 'left-overlay alist)))
             ( window-change-notify-current-top-overlay
               (cdr (assq 'top-overlay alist)))
             ( window-change-notify-current-newline-overlay
               (cdr (assq 'newline-overlay alist)))
             ( result (funcall redraw-func)))
        (when result
          (overlay-put ov 'display result))
        (setf (cdr (assq 'height alist)) (window-height))
        (setf (cdr (assq 'width alist)) (window-width))))
    (set-window-start nil (point-min))
    ))

(defun wcn/report (&optional symbol)
  (set (or symbol 'tmp2)
       (mapcar (lambda (pair)
                 (if (overlayp (cdr pair))
                     (cons (car pair)
                           (overlay-properties
                            (cdr pair)))
                   pair))
               (cdr (assoc (selected-window)
                           window-change-notify-window-alist)))))

(define-derived-mode window-change-notify-mode special-mode
    "Window change notify mode"
    "Window change notify mode"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "$\n$\n"))
  (setq-local auto-window-vscroll nil)
  (setq-local cursor-type nil)
  (add-hook 'window-configuration-change-hook
            'window-change-notify-hook nil t)
  (local-set-key (kbd "g") 'wcn/revert)
  (add-hook 'post-command-hook
            'wcn/post-command-hook
            nil t)
  )

;; -----------------------------------------------------------------------------
;; Examples
;; -----------------------------------------------------------------------------

;; $

(defun $-fill ()
  (with-temp-buffer
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
       (list 'face `(:foreground ,(es-color-random-hex)))))
    (buffer-string)))

(define-derived-mode $-mode window-change-notify-mode
    "$" "$"
  (setq window-change-notify-function '$-fill)
  (wcn/redraw-all-windows))


(defun $ ()
  (interactive)
  (with-current-buffer (get-buffer-create "*$*")
    ($-mode)
    (pop-to-buffer (current-buffer))))

;; Picture

(defvar picture-card-picutre
  (let* ((root (file-name-directory
                (or load-file-name buffer-file-name))))
    (lambda ()
      (condition-case error
          (if (wcn/color-dark-p (face-attribute 'default :background))
              (concat root "logo-dark.png")
            (concat root "logo-light.png"))
        (error (concat root "logo-light.png"))))))

(defun picture-card-fill ()
  (let* (( window-width (es-window-inside-pixel-width))
         ( window-height (es-window-inside-pixel-height))
         ( image-file (if (stringp picture-card-picutre)
                          picture-card-picutre
                        (funcall picture-card-picutre)))
         ( image-type (image-type image-file nil nil))
         ( image-spec (list 'image
                            :type image-type
                            :file image-file))
         ( image-dimensions (image-size image-spec t))
         ( x (/ (- window-width (car image-dimensions)) 2))
         ( y (/ (- window-height (cdr image-dimensions)) 2)))
    (when wcn/debug
      (overlay-put window-change-notify-current-top-overlay
                   'face `(:background ,(es-color-random-hex)))
      (overlay-put window-change-notify-current-left-overlay
                   'face `(:background ,(es-color-random-hex))))
    (window-change-notify-set-top-margin y t)
    (window-change-notify-set-left-margin x t)
    image-spec))

(define-derived-mode picuture-card-mode window-change-notify-mode
    "Picture" "Picture"
  (setq window-change-notify-function 'picture-card-fill)
  (wcn/redraw-all-windows))

(defun picuture-card ()
  (interactive)
  (with-current-buffer (get-buffer-create "*picture-card*")
    (picuture-card-mode)
    (pop-to-buffer (current-buffer))))

(provide 'window-change-notify)
;;; win-change-notify.el ends here
