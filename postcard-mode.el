;; -*- lexical-binding: t -*-
(require 'cl-lib)

(defvar-local postcard-function 'ignore)
(defvar-local postcard-format 'characters)
(defvar-local postcard-window-alist nil)

(defvar postcard-current-main-overlay nil)
(defvar postcard-current-left-overlay nil)
(defvar postcard-current-top-overlay nil)
(defvar postcard-current-newline-overlay nil)
(defvar postcard--debug nil)

;; -----------------------------------------------------------------------------
;; Window Change Notify
;; -----------------------------------------------------------------------------

(defun postcard--clear-overlays ()
  (delete-all-overlays)
  (setq postcard-window-alist))

(defun postcard--cleanup ()
  (setq postcard-window-alist
        (cl-remove-if-not
         (lambda (pair)
           (or (window-live-p (car pair))
               (progn
                 (delete-overlay (cdr (assq 'top-overlay (cdr pair))))
                 (delete-overlay (cdr (assq 'newline-overlay (cdr pair))))
                 (delete-overlay (cdr (assq 'main-overlay (cdr pair))))
                 (delete-overlay (cdr (assq 'left-overlay (cdr pair))))
                 nil)))
         postcard-window-alist)))

(defun postcard--redraw-all-windows ()
  (postcard--clear-overlays)
  (cl-dolist (window (get-buffer-window-list nil nil t))
    (with-selected-window window
      (postcard-hook t))))

(defun postcard--post-command-hook ()
  (deactivate-mark)
  (set-window-start nil (point-min)))

(defun postcard--revert ()
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
;; (cl-every 'postcard--color-dark-p
;;           '("#555" "#500" "#050"))

;; (cl-notany 'postcard--color-dark-p
;;            '("#fff" "#ff0" "#faa"))

(defun postcard--color-dark-p (color)
  (< (cl-loop for color in (color-values color)
              maximizing color)
     ;; 65280 Maximum
     35280
     ))

(defun postcard-set-top-margin (height &optional pixels)
  (cl-assert (and postcard-current-newline-overlay
                  postcard-current-top-overlay))
  (if (<= height 0)
      (progn
        (overlay-put postcard-current-top-overlay
                     'invisible t)
        (overlay-put postcard-current-newline-overlay
                     'invisible t)
        (overlay-put postcard-current-top-overlay
                     'display '(space :width (0) :height (0)))
        (overlay-put postcard-current-newline-overlay
                     'display '(space :width (0) :height (0))))
    (overlay-put postcard-current-top-overlay
                 'display `(space :height ,(if pixels (list height) height)))
    (overlay-put postcard-current-top-overlay
                 'invisible nil)

    (overlay-put postcard-current-newline-overlay
                 'display nil)
    (overlay-put postcard-current-newline-overlay
                 'invisible nil)
    ))

(defun postcard-set-left-margin (width &optional pixels)
  (cl-assert postcard-current-left-overlay)
  (if (<= width 0)

      (progn
        ;; Invisible doesn't seem to do anything
        ;; (overlay-put postcard-current-left-overlay
        ;;              'invisible t)
        (overlay-put postcard-current-left-overlay
                     'display '(space :width (0) :height (0))))

    (progn
      (overlay-put postcard-current-left-overlay
                   'display `(space :width ,(if pixels (list width) width)))
      (overlay-put postcard-current-left-overlay
                   'invisible nil)))
  ;; (setq tmp3 postcard-current-left-overlay)
  )

(cl-defun postcard-hook (&optional force)
  ;; FIXME: This runs once for every window. Not tragic, but still unnecessary.
  (postcard--cleanup)
  (let* (( redraw-func postcard-function)
         ( do-redraw force)
         ( alist (or (cdr (assq (selected-window)
                                postcard-window-alist))
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

                       (setq postcard-window-alist
                             (cl-acons (selected-window) temp-alist
                                       postcard-window-alist))
                       (setq do-redraw t)
                       temp-alist
                       )))
         ( ov (cdr (assq 'main-overlay alist))))
    (unless do-redraw
      (setq do-redraw
            (or (not (equal (cdr (assq 'height alist)) (window-height)))
                (not (equal (cdr (assq 'width alist)) (window-width))))))
    (when do-redraw
      (let* (( postcard-current-main-overlay ov)
             ( postcard-current-left-overlay
               (cdr (assq 'left-overlay alist)))
             ( postcard-current-top-overlay
               (cdr (assq 'top-overlay alist)))
             ( postcard-current-newline-overlay
               (cdr (assq 'newline-overlay alist)))
             ( result (funcall redraw-func)))
        (when result
          (overlay-put ov 'display result))
        (setf (cdr (assq 'height alist)) (window-height))
        (setf (cdr (assq 'width alist)) (window-width))))
    (set-window-start nil (point-min))
    ))

(defun postcard--report (&optional symbol)
  (set (or symbol 'tmp2)
       (mapcar (lambda (pair)
                 (if (overlayp (cdr pair))
                     (cons (car pair)
                           (overlay-properties
                            (cdr pair)))
                   pair))
               (cdr (assoc (selected-window)
                           postcard-window-alist)))))

(define-derived-mode postcard-mode special-mode
    "Window change notify mode"
    "Window change notify mode"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "$\n$\n"))
  (setq-local auto-window-vscroll nil)
  (setq-local cursor-type nil)
  (add-hook 'window-configuration-change-hook
            'postcard-hook nil t)
  (local-set-key (kbd "g") 'postcard--revert)
  (add-hook 'post-command-hook
            'postcard--post-command-hook
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

(define-derived-mode $-mode postcard-mode
    "$" "$"
  (setq postcard-function '$-fill)
  (postcard--redraw-all-windows))


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
          (if (postcard--color-dark-p (face-attribute 'default :background))
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
    (when postcard--debug
      (overlay-put postcard-current-top-overlay
                   'face `(:background ,(es-color-random-hex)))
      (overlay-put postcard-current-left-overlay
                   'face `(:background ,(es-color-random-hex))))
    (postcard-set-top-margin y t)
    (postcard-set-left-margin x t)
    image-spec))

(define-derived-mode picture-card-mode postcard-mode
    "Picture" "Picture"
  (setq postcard-function 'picture-card-fill)
  (postcard--redraw-all-windows))

(defun picuture-card ()
  (interactive)
  (with-current-buffer (get-buffer-create "*picture-card*")
    (picuture-card-mode)
    (pop-to-buffer (current-buffer))))

(provide 'postcard-mode)
;;; postcard-mode.el ends here
