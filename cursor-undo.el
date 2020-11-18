(require 'elscreen)

(defvar cursor-undo-moving-threshold
  150
  "there is no reason for the value")
(defvar cursor-undo-position-history
  `(
    (,(elscreen-get-current-screen) . ())
    )
  "(screennumber . (marker0 (recent) marker1...))
(screennumber . (marker0 (recent) marker1...))
...")
(defvar cursor-undo-position-history-size 20)

(defvar cursor-undo-last-marker nil)
(defvar cursor-undo-last-elscreen-number nil)


(defun cursor-undo-get-history ()
  (cdr (assoc (elscreen-get-current-screen) cursor-undo-position-history)))

(defun cursor-undo-set-history (new-history)
  ;; append new value
  (setq cursor-undo-position-history
	(append cursor-undo-position-history
		`((,(elscreen-get-current-screen) . ,new-history))))
  ;; delete old value, this is because `assoc' return first element
  ;; matched key
  (setq cursor-undo-position-history
	(delete (assoc (elscreen-get-current-screen) cursor-undo-position-history)
		cursor-undo-position-history))
  )


(defun cursor-undo-save-current-position ()
  (setq cursor-undo-last-marker (point-marker))
  (setq cursor-undo-last-elscreen-number (elscreen-get-current-screen))
  )

(defun cursor-undo-pre-command-hook ()
  (unless (eq this-command 'cursor-undo-undo-position)
    (cursor-undo-save-current-position))
  )


(defun cursor-undo-have-to-save (marker)
  (let
      ((last-position (marker-position marker))
       (last-buffer (marker-buffer marker)))
    (and
     ;; Save only when the following conditions are satisfied
     ;; - moving between same screens
     (eq cursor-undo-last-elscreen-number (elscreen-get-current-screen))
     ;; - current buffer is not a minibuffer
     (not (minibufferp last-buffer))
     (or
      ;; Save if any of the following conditions are satisfied
      ;; 1. current buffer and the buffer before moving are different
      (not (eq last-buffer (current-buffer)))
      ;; 2. there is a distance of `cursor-undo-moving-threshold' or
      ;; more between the position before and after the move.  e.g.,
      ;; move from the middle of the buffer to the beginning of the
      ;; buffer
      (> (abs (- last-position (point)))
      	 cursor-undo-moving-threshold)
      ;; 3. movement history exsists and still exist history-marked
      ;; buffer, and there is a distance of
      ;; `cursor-undo-moving-threshold' or more between the last
      ;; movement history and the current position.  e.g.,
      ;; long-distance movement by repeated `next-line'
      (and (car (cursor-undo-get-history))
	   (buffer-live-p (marker-buffer (car (cursor-undo-get-history))))
      	   (> (abs (- last-position (marker-position (car (cursor-undo-get-history)))))
      	      cursor-undo-moving-threshold))
      ))
    ))

(defun cursor-undo-post-command-hook ()
  (unless (or (eq this-command 'cursor-undo-undo-position)
  	      (not (markerp cursor-undo-last-marker)))
    (when (cursor-undo-have-to-save cursor-undo-last-marker)
      ;; TO DEBUG
      ;; (message "%s -> %s" cursor-undo-last-marker (point-marker))
      (cursor-undo-set-history (cons cursor-undo-last-marker (cursor-undo-get-history)))
      ;; Limit the number of histories to
      ;; `cursor-undo-position-history-size'
      (when (> (length (cursor-undo-get-history)) cursor-undo-position-history-size)
	(cursor-undo-set-history (butlast (cursor-undo-get-history))))
      )))


(defun cursor-undo-undo-position ()
  "Undo cursor position. Cursor-position history is saved on each
screen of `elscreen'.

Save cursor position when...
- buffer is changed
- the cursor is moved a long distance."
  (interactive)
  (if (not (cursor-undo-get-history))
      (message "position histroy is empty")
    (let*
	((last-marker (car (cursor-undo-get-history)))
	 (last-position (marker-position last-marker))
	 (last-buffer (marker-buffer last-marker)))
      (when (buffer-live-p last-buffer)
	(switch-to-buffer last-buffer)
	(goto-char last-position)
	))
      (cursor-undo-set-history (cdr (cursor-undo-get-history)))
    ))


(defun cursor-undo-init ()
  (add-hook 'pre-command-hook 'cursor-undo-pre-command-hook)
  (add-hook 'post-command-hook 'cursor-undo-post-command-hook)

  ;; create/delete memory for created/deleted screen in
  ;; cursor-undo-position-history
  (add-hook 'elscreen-create-hook
	    (lambda ()
	      (setq cursor-undo-position-history
		    (append cursor-undo-position-history
			    `((,this-created-screen . ()))))
	      ))
  (add-hook 'elscreen-kill-hook
	    (lambda ()
	      (setq cursor-undo-position-history
		    (delete (assoc this-killed-screen cursor-undo-position-history)
			    cursor-undo-position-history))
	      ))
  )


(provide 'cursor-undo)
