(require 'json)
(setq tls-hostmismatch nil)
(setq url-debug nil)

(defun pexclient-execute (target data &optional auth-line)
  "Returns a buffer containing the http response from target or
nil if the url retrieval failed.  `auth-line' if present is an
alist of either (\"pin\" . pin) or (\"token\" . token) "
  (let ((url-request-method "POST")
	(url-request-data (json-encode-alist data))
	(url-request-extra-headers
         (list (cons "Content-type" "application/json")
               auth-line)))
    (let ((buf (url-retrieve-synchronously target)))
      (if url-debug
	  (switch-to-buffer buf)
	(insert url-request-data))
      buf)))

(defun pexclient-get (target auth-line)
  (let ((url-request-method "GET")
        (url-request-extra-headers
         (list (cons "Content-type" "application/json")
               auth-line)))
    (let ((buf (url-retrieve-synchronously target)))
      (if url-debug
	  (switch-to-buffer buf))
      buf))) 
  


(defun pexclient-authenticate (target conf &optional pin)
  "Return an authentication blob for `conf' running on node
`target' using `pin' to be granted access if given."
  
  (let ((target-url (concat "https://"
                            target
                            "/api/client/v2/conferences/"
                            conf
                            "/request_token")
                    )
        (auth-line (if pin
                       (cons "pin" pin))))
    (with-current-buffer (pexclient-execute target-url auth-line )
      (pexclient--delete-headers)
      (let ((token  (cdr (assoc 'token
                                (assoc 'result
                                       (json-read-from-string (buffer-string)))))))
        (list (cons "target" target)
              (cons "conf" conf)
              (cons "token" token)
              (cons "pin" pin))))))

(defun pexclient-participants (auth-blob)
  "Return the current list of participants for the
       conference which granted the credentials in `auth-blob'"
  (let ((target-url (concat "https://"
                            (pexclient--target auth-blob)
                            "/api/client/v2/conferences/"
                            (cdr (assoc "conf" auth-blob))
                            "/participants")))
    (pexclient--process-response (pexclient-get target-url
                                              (pexclient--token auth-blob)))))


(put 'invalid-auth 'error-conditions '(error invalid-auth))
(put 'invalid-auth 'error-message "Invalid authorisation")

(defun pexclient--process-response (buf)
  (with-current-buffer buf
    (pexclient--delete-headers)
    (message "%s" (buffer-string))
    (let ((resp (json-read-from-string (buffer-string))))
      (if (string= "failed" (cdr (assoc 'status resp)))
          (if (string= "Invalid token" (cdr (assoc 'result resp)))
              (signal 'invalid-auth nil)
            (error "Unknown failure in Pexip REST API"))
        (cdr (assoc 'result resp))))))


(defun pexclient--target (auth-blob)
  "Return the contents of the `target' field in auth-blob"
  (cdr (assoc "target" auth-blob)))

(defun pexclient--token (auth-blob)
  "Return a token cons cell"
  (assoc "token" auth-blob))



(defun pexclient--delete-headers ()
  "Delete HTTP headers from current buffer"
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point)))



     
