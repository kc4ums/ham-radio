;;; wsjtx-connect.el --- Connect to WSJT-X via UDP and display raw data

(defvar wsjtx-host "127.0.0.1"
  "Host address for WSJT-X UDP server (localhost by default).")
(defvar wsjtx-port 2237
  "Default UDP port for WSJT-X communication.")
(defvar wsjtx-buffer-name "*WSJT-X-Raw-Data*"
  "Name of the buffer to display WSJT-X data.")
(defvar wsjtx-process nil
  "Network process for WSJT-X UDP connection.")

(defun wsjtx-connect ()
  "Connect to WSJT-X via UDP and start receiving raw data."
  (interactive)
  (if wsjtx-process
      (message "Already connected to WSJT-X!")
    (let ((buffer (get-buffer-create wsjtx-buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "Connecting to WSJT-X UDP server...\n"))
      ;; Open UDP connection
      (setq wsjtx-process
            (make-network-process :name "wsjtx"
                                 :buffer buffer
                                 :host wsjtx-host
                                 :service wsjtx-port
                                 :family 'ipv4
                                 :type 'datagram))
      (when wsjtx-process
        (set-process-filter wsjtx-process 'wsjtx-process-filter)
        (set-process-sentinel wsjtx-process 'wsjtx-process-sentinel)
        (message "Connected to WSJT-X UDP server. Data will appear in %s"
                 wsjtx-buffer-name)))))

(defun wsjtx-process-filter (process packet)
  "Process incoming WSJT-X UDP packets and display them."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      ;; Convert raw packet to readable string (hex dump for simplicity)
      (insert (format "Received at %s: %s\n"
                      (current-time-string)
                      (mapconcat (lambda (byte) (format "%02x" byte))
                                 (string-to-list packet) " "))))))

(defun wsjtx-process-sentinel (process event)
  "Handle WSJT-X connection events."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (format "\nConnection event at %s: %s\n"
                        (current-time-string) event))))
    (cond
     ((string-match "connection broken" event)
      (setq wsjtx-process nil)
      (message "WSJT-X connection closed"))
     ((string-match "open" event)
      (message "WSJT-X connection established")))))

(defun wsjtx-disconnect ()
  "Disconnect from the WSJT-X UDP server."
  (interactive)
  (if (not wsjtx-process)
      (message "Not connected to WSJT-X!")
    (delete-process wsjtx-process)
    (setq wsjtx-process nil)
    (with-current-buffer wsjtx-buffer-name
      (goto-char (point-max))
      (insert "\nDisconnected from WSJT-X.\n"))
    (message "Disconnected from WSJT-X")))

;; Keybindings
(global-set-key (kbd "C-c w c") 'wsjtx-connect)
(global-set-key (kbd "C-c w d") 'wsjtx-disconnect)

(provide 'wsjtx-connect)
;;; wsjtx-connect.el ends here
