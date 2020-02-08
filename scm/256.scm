(define e #\x1B)
(define (tostr i) (number->string i))

(define (colors i)
  (display
    (string-append (tostr i) "\u001B[4D\u001B[4C\u001B[48;5;"
                   (tostr i) "m\u001B[K\u001B[m\n"))
  (if (< i 255) (colors (+ 1 i))))

(colors 0)
