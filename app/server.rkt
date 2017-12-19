#lang racket/base

(require racket/tcp)
(require racket/port)
(require racket/string)
(require racket/file)

(require xml)

(require "logger.rkt")

(provide serve)


; set the debug level to maximum
(DEBUG-LEVEL WARN)


;;;;; top-level constants
(define temp-port 3000)


(define BASE-FOLDER (make-parameter "public"))


(define REQ-COUNTER (make-parameter 0))



;;;;; functions
(define (read-until-empty-lines in-port)

  (define (inner in-port accum)
    (define input-datum (read-line in-port))
    (cond
      [(eof-object? input-datum) accum]
      [else (begin 
              (define line (string-replace input-datum "\r" ""))
              (if (string=? "" line)
                  accum
                  (inner in-port (cons line accum))))]))
    (reverse (inner in-port '())))
  


(define (accept-and-handle listener)
  (info "Running accept-and-handle")
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    
    (define-values (in out) (tcp-accept listener))
    (thread (λ ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  
  ; shut down all sub-threads after 10 seconds
  (thread (λ ()
            (sleep 10)
            (custodian-shutdown-all cust)))
  (void))



(define (create-header-hash header-list)
  (define header-hash (make-hash))
  (define top-split (string-split (car header-list) " "))
  (hash-set! header-hash "method"  (car top-split))
  (hash-set! header-hash "route"   (car (cdr top-split)))
  (hash-set! header-hash "httpver" (car (cdr (cdr top-split))))
  (for-each (λ (line)
              (define splitv (string-split line ":"))
              (hash-set! header-hash (car splitv) (car (cdr splitv))))
            (cdr header-list))
  (values header-hash))
  

; Route the request and write data to the out port
; Routes can result in many things that have to be accounted for:
; 1. Images or Assets
;    When someone requests an image, the path should be looked up in a local
;    image folder in the Racket server, something like "/public/images"
;    If it's in there, read the bytes and write the bytearray to the output stream
;    If not, yield a 404 message
; 2. A Page
;    Just like images, files should be stored in "/public". Files should be written
;    in Racket, such that /path/to/file mirrors /path/to/file.rkt
;    if no match exists, return a 404 error
(define (route header output)
  (unless (hash-has-key? header "route")
    (error "No route given in header"))

  (define desired-route (hash-ref header "route"))

  (cond
    [(string=? desired-route "/") (page-lookup "/index" output)]
    [else (page-lookup desired-route output)])

  (void))


(define (page-lookup desired-path output)
  (define file-path (string->path (string-append (BASE-FOLDER) desired-path ".rkt")))
  (if (file-exists? file-path)
      (serve-file file-path output)
      (do-404 output))
  (void))
                    

(define (serve-file file-path output)
  (display "HTTP/1.0 200 Okay\r\n" output)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" output)
  (display (xexpr->string (car (file->list file-path))) output))
  

  
(define (default-page output)
  (display "HTTP/1.0 200 Okay\r\n" output)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" output)
  (display "<html><body>Hello there</body></html>" output))


(define (do-404 output)
  (display "HTTP/1.0 404 Not Found" output)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" output)
  (display "<html><body>The requested content was not found</body></html>" output))


; The handle should create a header hash and dispatch it properly
; 
(define (handle in out)
  (info "Received in/out ports")
  (define stuff  (read-until-empty-lines in))

  (displayln "\nHEADER ---------------")
  (displayln (create-header-hash stuff))
  (displayln "----------------------\n")

  (info "Routing the request")
  (route (create-header-hash stuff) out))


(define (serve)
  (info "Starting up server")
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen temp-port 5 #t))

    (define (loop)
      (info "starting accept-and-handle")
      (accept-and-handle listener)
      (info "finished accept-and-handle")
      (loop))
    
    (define t (thread loop))
    (thread-wait t)
    (custodian-shutdown-all main-cust)))

; end

  
  
