#lang typed/racket/base


(provide info
         debug
         warn
         INFO
         DEBUG
         WARN
         DEBUG-LEVEL
         )


(define INFO  10)
(define DEBUG 20)
(define WARN  30)
(define DEBUG-LEVEL (make-parameter 0))

(define FORMAT-STR "[\033[38;5;~am~a\033[0m] ~a")


(: create-log-wrapper (-> String Integer Integer (-> String Void)))
(define (create-log-wrapper name num-filter color)
  (λ (msg)
    (when (<= num-filter (DEBUG-LEVEL))
      (displayln (format FORMAT-STR color name msg)))))


(define info  (create-log-wrapper "info" 10 13))
(define debug (create-log-wrapper "debg" 20 15))
(define warn  (create-log-wrapper "warn" 30  8))


; end
