#lang racket/base

(require racket/cmdline)

(require "app/logger.rkt")
(require "app/server.rkt")


(module+ main
  (command-line
   #:program "server"

   #:once-each
   [("-p" "--production") "Set port to run on production port 80"
                          (SERVER-PORT 80)]

   [("-w" "--warn") "Set the log level to WARN"
                    (DEBUG-LEVEL WARN)]
   #:args ()
   (serve)))
