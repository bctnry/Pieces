#!/usr/bin/racket

#lang racket/base
(require net/cgi)
(require db)
(require racket/format)

(define (display-singl-comment name content)
  (displayln
   (string-append
    "<div class=\"comment_singl\">
     <b>" name "</b> says: "content
     "</div>"
     )))

(output-http-headers)  ; output the header

(displayln "A simple guestbook.")
(displayln "<hr />")

; connecting to the mysql server
(define server-address "localhost")
(define server-username "root")
(define server-password "password") ; your password here.
(define server-database "Test")
(define conn (mysql-connect #:user server-username
			    #:database server-database
			    #:server server-address
			    #:password server-password))
(if (connected? conn)
    (begin
      (let* ((result (query conn (string-append "SELECT * FROM RktGuestbook")))
	     (headers (rows-result-headers result))
	     (rows (rows-result-rows result)))
	(for ([i rows])
	     (let ((name (vector-ref i 0)) (content (vector-ref i 1)))
	       (display-singl-comment name content))))
      (disconnect conn))
    (displayln "failed to connect database."))


(define bindings (get-bindings))  ; get the bindings
(let ((name (assoc 'name bindings)) (content (assoc 'content bindings)))
  (when (and name content)
      (let* ((conn (mysql-connect #:user server-username
				  #:database server-database
				  #:server server-address
				  #:password server-password))
	     (stmt_p (prepare conn "INSERT INTO RktGuestbook VALUES (?,?)"))
             (stmt (bind-prepared-statement stmt_p (list (cdr name) (cdr content)))))
	(begin
	  (query conn stmt)
	  (display-singl-comment (cdr name) (cdr content))
	  (disconnect conn)))))
      

(displayln "<hr />")
(displayln "
  <form action=\"#\" method=\"POST\">
  <table>
    <tr><td>Your name:</td>
        <td><input type=\"text\" name=\"name\" /></td></tr>
    <tr><td>Content:</td>
        <td><textarea name=\"content\"></textarea></td></tr>
    <tr><td colspan=\"2\"><input type=\"submit\" /></td></tr>
  </form>
")
