#!/usr/bin/env -S guix shell guile-json -- guile -s
!#

(use-modules
  ((web uri) #:select (build-uri))
  ((web client) #:select (http-get))
  ((json parser) #:select (json->scm))
  ((srfi srfi-11) #:select (let-values))
  ((ice-9 pretty-print) #:select (pretty-print)))

(define *api-uri* (build-uri 'https #:host "service.greenhost.net"
                                    #:path "/api/v2"))
(define *api-key* "ebcbdd77073e33c068a9858c9b6fc43f2dcbe7401f4faf67")

(define* (api #:key (uri *api-uri*)
                    (key *api-key*)
                    (path "")
                    (method 'GET))
  (let-valies (((header body-port) (http-request uri #:streaming? #t
                                                     #:method method)))))
(define api-schema
  (let-values (((req-uri )
                (header body-port) (http-get req-uri #:streaming? #t)))
    (json->scm body-port)))

(pretty-print api-schema)
