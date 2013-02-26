#lang racket/base
(require racket/runtime-path
         "../main.rkt")

(define-runtime-path root-path "wics")

(module+ main
  (go root-path 9006 
      "https://docs.google.com/forms/d/1lriBNLt9FyHEgtRiZ0TLn3CxzT2qeQToKZcQGztHl50/viewform"))
