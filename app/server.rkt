#lang typed/racket/base

(require racket/tcp)
(require racket/port)
(require racket/string)

(require "logger.rkt")

(provide serve)


(DEBUG-LEVEL WARN)

;;;;; top-level constants
(define temp-port 3000)


(define REQ-COUNTER (make-parameter 0))


;;;;; functions
(: read-until-empty-lines (-> Input-Port (Listof String)))
(define (read-until-empty-lines in-port)

  (: inner (-> Input-Port (Listof String) (Listof String)))
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
  


(: accept-and-handle (-> TCP-Listener Void))
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



(: create-header-hash (-> (Listof String)
                          (Mutable-HashTable String String)))
(define (create-header-hash header-list)
  (: header-hash (Mutable-HashTable String String))
  (define header-hash (make-hash))
  (hash-set! header-hash "top" (car header-list))
  (for-each (λ ([line : String])
              (define splitv (string-split line ":"))
              (hash-set! header-hash (car splitv) (car (cdr splitv))))
            (cdr header-list))
  (values header-hash))
  


(: handle (-> Input-Port Output-Port Void))
(define (handle in out)
  (info "Received in/out ports")
  (define stuff  (read-until-empty-lines in))
  (define header (string-join stuff "\n"))

  (displayln "\nHEADER ---------------")
  (displayln (create-header-hash stuff))
  (displayln "----------------------\n")

  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello there</body></html>" out))

(: serve (-> Void))
(define (serve)
  (info "Starting up server")
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen temp-port 5 #t))

    (: loop (-> Void))
    (define (loop)
      (info "starting accept-and-handle")
      (accept-and-handle listener)
      (info "finished accept-and-handle")
      (loop))
    
    (define t (thread loop))
    (thread-wait t)
    (custodian-shutdown-all main-cust)))

; end

  
  
