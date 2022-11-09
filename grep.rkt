#lang racket/base

(require mock
         racket/cmdline
         racket/port)

(define show-file-error-messages (make-parameter #true))

; TODO: -f FILE, --file=FILE
; TODO: -i, --ignore-case
; TODO: --no-ignore-case
; TODO: -v, --invert-match
; TODO: -w, --word-regexp
; TODO: -x, --line-regexp
; TODO: Write usage patterns like in manual, whatever you can currently use eg `grep [options ...] PATTERNS ... FILES ...
(define (main)
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
   (grep patterns paths)))

#| - Given a list of patterns and filepaths, prints lines from each file which
match at least one of the patterns. Prints error messages when it cannot access
a file, if instructed. |#
(define/mock (grep patterns paths)
  #:mock call-with-input-file #:as cwif-mock
  #:mock print-error-message #:as pem-mock #:with-behavior void
  #:mock grep-port #:as gp-mock #:with-behavior void
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
  (for ([line (in-lines port)])
    (for/first ([pattern patterns]
                #:when (regexp-match? pattern line))
      (printf "~a:~a\n" path line)))
  )

(define (errno-to-message errno)
  (case errno
    [(2) "No such file or directory"]
    [(13) "Permission denied"]))

(define (print-error-message form . v)
  (eprintf "grep: ~a"
           (apply format form v)))

(module+ main
  (main))

(module+ test
  (require rackunit
           mock/rackunit
           racket/function
           racket/string
           racket/generator))

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

  (define (mock-call-with-input-file . vals)
    (define vals-generator (sequence->generator vals))
    (λ (path proc)
      (define val (vals-generator))
      (cond
        [(string? val) (call-with-input-string val proc)]
        [(exn? val) (raise val)])))

  (define (mock-exn:fail:filesystem:errno errno)
    (exn:fail:filesystem:errno "" (current-continuation-marks) errno))

  (with-mocks grep
    (with-mock-behavior ([cwif-mock
                          (mock-call-with-input-file
                           "hi"
                           (mock-exn:fail:filesystem:errno '(2 . posix))
                           (mock-exn:fail:filesystem:errno '(13 . posix)))])
      (grep (list "hic" "hi") (list "path-one.txt"))
      ))
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

  (with-mocks grep
    (with-mock-behavior
        ([cwif-mock
          (mock-call-with-input-file
           (mock-exn:fail:filesystem:errno '(2 . posix))
           (mock-exn:fail:filesystem:errno '(13 . posix))
           (mock-exn:fail:filesystem:errno '(2 . posix))
           (mock-exn:fail:filesystem:errno '(13 . posix)))])
      (grep (list "") (list "file-dne.txt"))
      (check-mock-num-calls pem-mock 1)
      (grep (list "") (list "file-dne.txt"))
      (check-mock-num-calls pem-mock 2)
      (parameterize ([show-file-error-messages #false])
        (grep (list "") (list "file-dne.txt"))
        (check-mock-num-calls pem-mock 2)
        (grep (list "") (list "file-dne.txt"))
        (check-mock-num-calls pem-mock 2))
      ))

  (with-mocks grep
    (with-mock-behavior
        ([cwif-mock (mock-call-with-input-file "")])
      (grep (list "") (list "file.txt"))
      (check-mock-num-calls gp-mock 1)))
  )

