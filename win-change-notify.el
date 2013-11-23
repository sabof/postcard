;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar-local window-change-notify-function 'ignore)
(defvar-local window-change-notify-format 'characters)
(defvar-local window-change-notify-window-alist nil)

(defvar window-change-notify-current-main-overlay nil)
(defvar window-change-notify-current-left-overlay nil)
(defvar window-change-notify-current-top-overlay nil)
(defvar window-change-notify-current-newline-overlay nil)

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

;; -----------------------------------------------------------------------------

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
                                            (1+ main-overlay-offset)))
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

;; -----------------------------------------------------------------------------

(define-derived-mode window-change-notify-mode special-mode
    "Window change notify mode"
    "Window change notify mode"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "$\n$\n"))
  (setq-local auto-window-vscroll nil)
  ;; (setq-local cursor-type nil)
  (add-hook 'window-configuration-change-hook
            'window-change-notify-hook nil t)
  (local-set-key (kbd "g") 'wcn/revert)
  ;; (add-hook 'post-command-hook
  ;;           'wcn/post-command-hook
  ;;           nil t)
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

;; Picture

(defvar picture-card-picutre-file
  (concat (file-name-directory
           (or load-file-name buffer-file-name))
          "1980-mad-max-poster2.jpg"))

(defun picture-card-fill ()
  (let* (( window-width (es-window-inside-pixel-width))
         ( window-height (es-window-inside-pixel-height))
         ( image-type (image-type picture-card-picutre-file nil nil))
         ( image-spec (list 'image :type image-type :file picture-card-picutre-file))
         ( image-dimensions (image-size image-spec t))
         ( x (max 0 (/ (- window-width (car image-dimensions)) 2)))
         ( y (max 0 (/ (- window-height (cdr image-dimensions)) 2))))
    ;; (debug nil image-dimensions)
    ;; This should just work

    ;; (let ((inhibit-read-only t))
    ;;   (goto-char (point-min))
    ;;   (insert (propertize " " 'display '(space :height (70) ))
    ;;           (propertize "\n" 'line-height t))
    ;;   nil)

    ;; This should also just work
    window-change-notify-current-newline-overlay
    window-change-notify-current-top-overlay
    (overlay-put window-change-notify-current-left-overlay
                 'display `(space :width (,x)))
    (overlay-put window-change-notify-current-left-overlay
                 'invisible nil)

    (if (zerop y)
        (progn
          (overlay-put window-change-notify-current-top-overlay
                       'invisible t)
          (overlay-put window-change-notify-current-newline-overlay
                       'invisible t))
      (progn
        (overlay-put window-change-notify-current-top-overlay
                     'display `(space :height (,y)))
        (overlay-put window-change-notify-current-top-overlay
                     'invisible nil)
        (overlay-put window-change-notify-current-newline-overlay
                     'invisible nil))
      )
    ;; (insert-image image-spec)
    image-spec
    ;; "x"
    ))

(define-derived-mode picuture-card-mode window-change-notify-mode
    "Picture" "Picture"
  (setq window-change-notify-function 'picture-card-fill)
  (wcn/redraw-all-windows))

(provide 'window-change-notify)
;;; win-change-notify.el ends here
