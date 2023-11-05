;;; epipe.el --- Pipe stdout/stderr to emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023
;;
;; Author:  cbadger
;; Maintainer:  cbadger
;; Created: November 04, 2023
;; Modified: November 04, 2023
;; Version: 0.1.0
;; Homepage: https://github.com/fkr-0/epipe
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Pipe stdout/stderr to EMACS
;;
;;; Code:

(defvar epipe-buffer-name "*epipe*")
(defvar epipe-network-process-name "epipe")
(defvar epipe-proc-map (make-hash-table :test 'equal))
;; (defvar epipe-socket-path "/tmp/epipe.sock"
;;   "Path to the unix socket used for communication.")
(defun epipe--sock-path-to-buffer (sock-path)
  "Convert SOCK-PATH to buffer name. Return buffer instance."
  (get-buffer-create (string-join (list "*" "epipe" sock-path "*") )))

(defun epipe--proc-to-buffer (proc)
  "Convert PROC to buffer name. Return buffer instance."
  (get-buffer-create (string-join (list "*" "epipe" (process-contact proc :service) "*") )))

(defun epipe-receive (base64-output &optional buffer-name append)
  "Receive base64-encoded BASE64-OUTPUT and insert it.

If BUFFER-NAME is specified, insert the output into that buffer.
Otherwise, insert it into the buffer specified by
`epipe-buffer-name'. Overwrites by default but if APPEND is t."
  (when (not buffer-name)
    (setq buffer-name epipe-buffer-name))
  ;; (while (get-buffer buffer-name)
  ;;   (setq buffer-name (concat buffer-name "1")))
  (let ((buffer (get-buffer-create buffer-name))
         (output (base64-decode-string base64-output)))
    (with-current-buffer buffer
      (when (not append)
        (erase-buffer))
      (goto-char (point-max))  ; Move to the end of the buffer
      (insert output)          ; Insert the decoded output
      (unless (get-buffer-window buffer 'visible)
        (display-buffer buffer)))))  ; Display the buffer if not already visible

(defun epipe--add-proc (sock-path)
  "Add new epipe network process for SOCK-PATH."
  (let ((proc (make-network-process
                :name (string-join (list epipe-network-process-name sock-path) )
                :buffer (epipe--sock-path-to-buffer sock-path)
                :family 'local
                :service sock-path
                :filter  'epipe--update-filter ;; '(lambda (a b) (epipe--update-filter a b sock-path))
                :server t
                :sentinel 'epipe--update-sentinel)))
    (puthash sock-path proc epipe-proc-map)))

(defun epipe--stop-proc (sock-path)
  "Stop epipe network process for SOCK-PATH."
  (let ((proc (gethash sock-path epipe-proc-map)))
    (when (process-live-p proc)
      (delete-process proc)
      (remhash sock-path epipe-proc-map)
      (delete-file sock-path))))

(defun epipe-receive-socket (base64-output &optional buffer-name)
  "Receive base64-encoded BASE64-OUTPUT and insert it.

If BUFFER-NAME is specified, insert the output into that buffer. Otherwise,
insert it into the buffer specified by `epipe-buffer-name'."
  (when (not buffer-name)
    (setq buffer-name epipe-buffer-name))
  (let ((buffer (get-buffer-create buffer-name))
         (output (base64-decode-string base64-output)))
    (with-current-buffer buffer
      (goto-char (point-max))  ; Move to the end of the buffer)
      (insert output)          ; Insert the decoded output)
      (unless (get-buffer-window buffer 'visible)
        (display-buffer buffer)))))  ; Display the buffer if not already visible

(defun epipe--update-filter (proc string)
  "Filter function that receives data from PROC.

Inserts STRING into the buffer."
  ;; (message "Received: %s" string)
  ;; (message "Proc: %s" proc)
  (let ((buffer (epipe--proc-to-buffer proc)))
    ;; (message "Bufferr: %s" buffer)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (epipe-receive string (current-buffer) t)))))

(defun epipe--update-sentinel (proc msg)
  "Handle message MSG in process status of PROC.

If the process has finished or exited, kill the buffer.
This function is used as a sentinel for `epipe--update-process'."
  (message "Received: %s" msg)
  (message "Buffer: %s" proc)
  (when (string-match-p "\\(finished\\|exited\\)" msg)
    (kill-buffer (process-buffer proc))))


(provide 'epipe)
;;; epipe.el ends here
