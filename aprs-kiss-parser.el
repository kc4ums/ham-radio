;;; aprs-kiss-parser.el --- Parse APRS data from a serial TNC in KISS mode

(require 'serial-term)  ;; For serial port communication

(defvar aprs-serial-port "/dev/ttyUSB0"  ;; Default serial port, adjust as needed
  "Serial port connected to the TNC.")

(defvar aprs-baud-rate 9600  ;; Common baud rate for TNCs
  "Baud rate for the serial connection.")

(defvar aprs-buffer "*APRS Data*"  ;; Buffer to display parsed data
  "Buffer name for APRS data output.")

(defvar aprs-process nil
  "Process object for the serial connection.")

;; KISS special characters
(defconst kiss-fend #xC0  ;; Frame End
  "KISS Frame End character.")
(defconst kiss-fesc #xDB  ;; Frame Escape
  "KISS Frame Escape character.")
(defconst kiss-tfend #xDC  ;; Transposed Frame End
  "KISS Transposed Frame End character.")
(defconst kiss-tfesc #xDD  ;; Transposed Frame Escape
  "KISS Transposed Frame Escape character.")

(defun aprs-start-serial ()
  "Start the serial connection to the TNC."
  (interactive)
  (unless (and aprs-process (process-live-p aprs-process))
    (setq aprs-process
          (serial-term aprs-serial-port aprs-baud-rate))
    (with-current-buffer (process-buffer aprs-process)
      (rename-buffer aprs-buffer)
      (set-process-filter aprs-process 'aprs-process-filter))
    (message "Connected to TNC on %s at %d baud" aprs-serial-port aprs-baud-rate)))

(defun aprs-stop-serial ()
  "Stop the serial connection to the TNC."
  (interactive)
  (when (and aprs-process (process-live-p aprs-process))
    (delete-process aprs-process)
    (setq aprs-process nil)
    (message "Disconnected from TNC")))

(defun aprs-process-filter (proc string)
  "Process filter to handle incoming serial data from the TNC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert string)
      (aprs-parse-kiss-data))))

(defun aprs-parse-kiss-data ()
  "Parse KISS-framed APRS data from the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((frame-start nil)
          (frame-end nil)
          (kiss-data nil))
      (while (search-forward (char-to-string kiss-fend) nil t)
        (setq frame-end (point))
        (when frame-start
          (let ((frame (buffer-substring-no-properties frame-start (1- frame-end))))
            (setq kiss-data (aprs-decode-kiss frame))
            (when kiss-data
              (aprs-parse-aprs kiss-data))))
        (setq frame-start frame-end))
      (delete-region (point-min) frame-start))))

(defun aprs-decode-kiss (frame)
  "Decode a KISS frame, handling escape characters."
  (let ((decoded "")
        (i 0)
        (len (length frame)))
    (while (< i len)
      (let ((char (aref frame i)))
        (cond
         ((= char kiss-fesc)
          (setq i (1+ i))
          (if (< i len)
              (let ((next-char (aref frame i)))
                (cond
                 ((= next-char kiss-tfend)
                  (setq decoded (concat decoded (char-to-string kiss-fend))))
                 ((= next-char kiss-tfesc)
                  (setq decoded (concat decoded (char-to-string kiss-fesc))))
                 (t
                  (setq decoded (concat decoded (char-to-string char))))))))
         (t
          (setq decoded (concat decoded (char-to-string char)))))
        (setq i (1+ i))))
    ;; Skip the first byte (KISS command byte) and return the rest
    (if (> (length decoded) 1)
        (substring decoded 1)
      nil)))

(defun aprs-parse-aprs (data)
  "Parse the APRS data from a decoded KISS frame."
  (when (> (length data) 0)
    (let* ((aprs-string (decode-coding-string data 'utf-8))
           (fields (split-string aprs-string "[,:!]"))
           (source (nth 0 fields))
           (dest (nth 1 fields))
           (info (nth 2 fields)))
      (with-current-buffer (get-buffer-create aprs-buffer)
        (goto-char (point-max))
        (insert (format "Source: %s\nDestination: %s\nInfo: %s\n\n"
                        source dest info))))))

;; Commands to start/stop the parser
(global-set-key (kbd "C-c a s") 'aprs-start-serial)
(global-set-key (kbd "C-c a q") 'aprs-stop-serial)

(provide 'aprs-kiss-parser)
;;; aprs-kiss-parser.el ends here
