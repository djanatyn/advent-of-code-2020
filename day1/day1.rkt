#lang racket

(require racket/string)
(require racket/port)

(define expenses
  (map string->number
   (string-split
    (port->string (open-input-file "input")))))
