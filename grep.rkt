#lang racket

(define show-file-error-messages (make-parameter #true))

#| - Given a list of patterns and filepaths, prints lines from each file which
match at least one of the patterns. Prints error messages when it cannot access
a file, if instructed. |#
(define (grep patterns paths)
  (for ([path paths])
    (with-handlers
        ([exn:fail:filesystem:errno?
          (λ (exn)
            (when (show-file-error-messages)
              (print-error-message
               "~a: ~a\n"
               path
               (errno-to-message (car (exn:fail:filesystem:errno-errno exn))))))])
      (call-with-input-file
          path
        (λ (port) (grep-port patterns port path))))
    ))

#| - What does it do: Given a list of patterns and a port to scan, prints lines
from the port which match at least one of the patterns |#
(define (grep-port patterns port path)
  in-lines
  (for ([line (in-lines port)])
    (for/first ([pattern patterns]
                #:when (regexp-match? pattern line))
      (printf "~a:~a\n" path line)))
  )

(define (errno-to-message errno)
  (case errno
    [(2) ("No such file or directory")]
    [(13) ("Permission denied")]))

(define (print-error-message form . v)
  (eprintf "grep: ~a"
           (apply format form v)))

(module+ main
  (define patterns null)
  (command-line
   #:program "grep"
   #:multi
   (("-e" "--pattern")
    pattern
    "Use patterns for matching"
    (set! patterns (cons pattern patterns)))
   (("-s" "--no-messages")
    "suppress error messages"
    (show-file-error-messages #false))
   #:args paths
   (grep patterns paths))
  )

(module+ test
  (require rackunit))

; learning tests
(module+ test
  (check-exn
   exn?
   (thunk (open-input-file "unopenable-file.txt")))

  (check-exn
   exn:fail?
   (thunk (open-input-file "unopenable-file.txt")))

  (check-exn
   exn:fail:filesystem?
   (thunk (open-input-file "unopenable-file.txt")))

  (check-exn
   exn:fail:filesystem:errno?
   (thunk (open-input-file "file-doesnt-exist")))

  (check-equal?
   (with-handlers ([exn:fail:filesystem:errno?
                    (λ (exn) (car (exn:fail:filesystem:errno-errno exn)))])
     (open-input-file "file-doesnt-exist")
     )
   2
   )
  )

(module+ test
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

