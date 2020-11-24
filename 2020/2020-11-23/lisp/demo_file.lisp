#!/usr/bin/clisp
;;;; #!/usr/bin/sbcl --script
;;;; demo_file.lisp
;;;; Geany Execute for CLISP: clisp "%f" for SBCL: sbcl --script "%f"
;;;; Edit -> Preferences -> Tools -> Terminal: mate-terminal -e "/bin/sh %c"
;;;; Ian Stewart. November 2020. Copyright CC0.

;; Write to file using with-open-file which avoids an open and a close.
;; file /tmp/demo_1.txt is assigned variable filename
(defvar filename)
(setq filename "/tmp/demo_1.txt")
(delete-file filename)

;; Perform a probe to see if the file exists
(if (probe-file filename)
	(format t "~%~a file exists. Data will be appended.~%" filename)
	(format t "~%~a file does not exist. It will be created.~%" filename))

;; Write to file with an append, but need create as file does not exists.
(format t "~%Writing file: ~a...~%~%" filename)
(with-open-file (stream filename 
		:direction :output 
		:if-exists :append
		:if-does-not-exist :create)
   (format stream "File: ~a. Seconds since epoch: ~:d" filename 
		(truncate (get-internal-real-time) 1000000))
   (terpri stream)
)

;; Read the file. Default is :direction :input
(format t "~%Reading file: ~a. Contents...~%~%" filename)
(with-open-file (stream filename :direction :input)
    (do ((l (read-line stream) (read-line stream nil 'eof)))
        ((eq l 'eof) "Reached end of file.")
     (format t "~&~A~%" l)))
     	
;;(quit)

;; Using functions.
(setf filename "/tmp/demo_2.txt")
(terpri)(princ "Write and Read files as function")(terpri)
;; The following function writes a string to a file. A keyword parameter 
;; is used to specify what to do if the file already exists (by default 
;; it causes an error, the values admissible are those of the 
;; with-open-file macro).

(format t "~%Write file ~a as a function call...~%" filename)
(defun write-file (string filename &key (action-if-exists :supersede))
   (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete 
                                        :overwrite :append :supersede))
   (with-open-file (outstream filename :direction :output :if-exists action-if-exists)
     (write-sequence string outstream)))

(setf content (format nil "File: ~a. Linux epoch: ~:d" filename 
	(truncate (get-internal-real-time) 1000000)))
(write-file content filename)


(format t "~%Read file ~a as a function call...~%" filename)
(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

; Call the function while passing the file path and name.
(print(read-file filename))


(format t "~%~%Press Return Key to Exit")(read-line)
#|
Notes and References:
http://gigamonkeys.com/book/files-and-file-io.html
https://lispcookbook.github.io/cl-cookbook/files.html
https://riptutorial.com/ebook/common-lisp
https://riptutorial.com/common-lisp/example/19473/reading-and-writing-entire-files-to-and-from-strings

You can write this to read a line from a file:

(with-open-file (stream "/some/file/name.txt")
  (format t "~a~%" (read-line stream)))

To create a new file, you can write something like this:

(with-open-file (stream "/some/file/name.txt" :direction :output)
  (format stream "Some text."))

:direction
    :input
    :output
    :io
    :probe

    :input - for input streams (default value)
    :output - for output streams
    :io - for bidirectional streams
    :probe - for just checking a files existence; the stream is opened and then closed.


:if exists
	:error
	:new-version
	:rename 
	:rename-and-delete
	:append 
	:supersede

	:error - it signals an error.
	:new-version - it creates a new file with the same name but larger version number.
	:rename - it renames the existing file.
	:rename-and-delete - it renames the existing file and then deletes it.
	:append - it appends to the existing file.
	:supersede - it supersedes the existing file.
	nil - it does not create a file or even a stream just returns nil to indicate failure.


:if-does-not-exist
	:error
	:create
	
	:error - it signals an error.
	:create - it creates an empty file with the specified name and then uses it.
	nil - it does not create a file or even a stream, but instead simply returns nil to indicate failure.

The :element-type specifies the type of the unit of transaction for the stream.

The :external-format argument specifies an implementation-recognized scheme for representing characters in files.

|#
