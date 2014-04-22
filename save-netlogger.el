
(find-file "*Current-Net-Totals-YTD*")
(find-file "*Previous-Net-Totals-YTD*")

(defun open-checkin-file (file)
  (interactive  (list (read-file-name "Open checkin file:"))) 
  (find-file file)
  (setq checkin-file (file-name-nondirectory file))
  )


;;(new-log-entry)
(defun netlog-update ()
  (interactive)
  (switch-to-buffer  "*Current-Net-Totals-YTD*")
  (setq call "NOCALL")
  (set-buffer checkin-file)
  (beginning-of-buffer)
  (clear-buffer "*Previous-Net-Totals-YTD*")
  (copy-buffer-contents "*Current-Net-Totals-YTD*" "*Previous-Net-Totals-YTD*") 
  (clear-buffer "*Current-Net-Totals-YTD*")
  (update-current-totals)
  (set-buffer "*Current-Net-Totals-YTD*")
  (align-regexp (point-min) (point-max) "[0-9]+$")
  )

(defun update-current-totals () 
  ;;(interactive)
  (while (not (string-equal call "EOF"))
    (setq call (get-call)) ;; Get the call from the NetLogger Log
    
    (if (not (string-equal call "EOF"))
	(progn
	  (setq current-total (get-total call))  ;; If it is a new checkin it will add the call and ne 1 for 1st checkin
	(setq new-total (increment-total current-total))
	(new-total)
	)
      )
    )
  )

(defun copy-buffer-contents (from to)
  (set-buffer from)
  (copy-to-buffer to (point-min) (point-max))
  )

(defun clear-buffer(buffer)
  "Kill all of the text in the current buffer."
  ;;(interactive)
  (set-buffer buffer)
  (clipboard-kill-region 1 (point-max))
  (beginning-of-buffer)
  )

(defun get-call ()
  ;;(interactive)
  (save-excursion)
  (set-buffer checkin-file)
  (search-forward-regexp "|")
  (mark-word)
  (copy-to-register 1 (region-beginning) (region-end))
  (forward-line 1)
  (get-register 1)
  )

(defun new-total ()
  ;;(interactive)
  (set-buffer "*Current-Net-Totals-YTD*")
  (insert (concat (concat  (concat call " ") new-total ) "\n"))
  )

(defun get-total (call)
  ;;(interactive)
  (save-excursion)
  (set-buffer "*Previous-Net-Totals-YTD*")
  (beginning-of-buffer)  
  (search-forward-regexp call nil t)
  (setq start ( +  1 (point)))
  (end-of-line)
  (setq end (point))
  (copy-to-register 2 start end)
  (get-register 2)
  )	

(defun increment-total (total)
  (number-to-string (+ (string-to-number total) 1))
  )


(defun save-and-exit ()
(interactive)
(save-some-buffers)
(kill-emacs)
)
  
