#lang racket

(define (grep-port patterns port path)
  in-lines
  (for ([line (in-lines port)])
    (for/first ([pattern patterns]
                #:when (regexp-match? pattern line))
      (printf "~a:~a\n" path line)))
  )


#| - Take list of patterns and files from command-line |#
#| - For each file |#
#|   - Try to open a port to the file |#
#|   - If I can't, print an error message, unless the user said not to |#
#|   - grep-port |#

(module+ test
  (require rackunit)

  (define test-strings
    `#hash(
      ["file-one.txt" . "one"]
      ["path/to/file-two.txt"
       . "one and more things"]
      ["/absolute/path/to/file-three.txt"
       . ,(string-join
           '("one"
             "two"
             "three"
             )
           "\n")
       ]
      ["file-four.txt"
       . ,(string-join
           '("one two three four"
             "one three four"
             "one four"
             )
           "\n")
       ]
      ["file-five.txt"
       . ,(string-join
           '("one two"
             "two three"
             "three four"
             )
           "\n")]
      ))

  (define (test-grep-port test-file-path patterns expected-output [failure-message ""])
    (call-with-input-string
     (hash-ref test-strings test-file-path)
     (λ (port)
       (check-equal?
        (with-output-to-string
          (λ ()
            (grep-port patterns port test-file-path)))
        expected-output
        failure-message
        ))))

  (test-grep-port
   "file-one.txt"
   (list #px"one")
   (format "~a:~a\n" "file-one.txt" "one")
   "failed to find match in a single word file")

  (test-grep-port
   "path/to/file-two.txt"
   (list #px"one")
   (format "~a:~a\n" "path/to/file-two.txt" "one and more things")
   "failed to find match in a file with one line")

  (test-grep-port
   "/absolute/path/to/file-three.txt"
   (list #px"one")
   (format "~a:~a\n" "/absolute/path/to/file-three.txt" "one")
   "failed to find match in a file with several lines")

  (test-grep-port
   "/absolute/path/to/file-three.txt"
   (list #px"two")
   (format "~a:~a\n" "/absolute/path/to/file-three.txt" "two")
   "failed to find match in a file with several lines")

  (test-grep-port
   "file-four.txt"
   (list #px"one")
   (string-join
    (map
     (λ (line)
       (format "~a:~a\n" "file-four.txt" line))
     '("one two three four"
       "one three four"
       "one four"
       ))
    "")
   "failed to match multiple lines"
   )

  (test-grep-port
   "file-five.txt"
   (list #px"one" #px"two")
   (string-join
    (map
     (λ (line)
       (format "~a:~a\n" "file-five.txt" line))
     '("one two"
       "two three")
     )
    "")
   "prints each matching line more than once"
   )
  )


