#lang racket/base

(require mock
         racket/cmdline
         racket/bool
         racket/function
         racket/port)

(define show-file-error-messages (make-parameter #true))
(define ignore-case-in-search (make-parameter #false))
(define invert-matches (make-parameter #false))

; TODO: -w, --word-regexp
; TODO: -x, --line-regexp
; TODO(v2): Write usage patterns like in manual, whatever you can currently use eg `grep [options ...] PATTERNS ... FILES ...
; TODO(v2): How do I test out main?
(define/mock (main)
  #:mock grep #:as g-mock #:with-behavior void
  #:mock call-with-input-file #:as cwif-mock
  (define patterns null)
  (command-line
   #:program "grep"
   #:multi
   (("-e" "--pattern")
    pattern
    "Use patterns for matching"
    (set! patterns (cons pattern patterns)))
   (("-f" "--file")
    path
    "take PATTERNS from FILE"
    (cond
      [(directory-exists? path)
       (print-error-message
        "~a: ~a\n"
        path
        "Is a directory. Argument to --file must be a file.")
       (exit)]
      [else
       (with-handlers
           ([exn:fail:filesystem:errno?
             (λ (exn)
               (when (show-file-error-messages)
                 (print-error-message
                  "~a: ~a\n"
                  path
                  (errno-to-message (car (exn:fail:filesystem:errno-errno exn))))
                 (exit)))])
         (call-with-input-file
             path
           (λ (port) (set! patterns (append patterns (port->lines port))))))
       ]))
   (("-s" "--no-messages")
    "suppress error messages"
    (show-file-error-messages #false))
   (("-i" "--ignore-case")
    "ignore case distinctions in patterns and data"
    (ignore-case-in-search #true))
   (("--no-ignore-case")
    "do not ignore case distinctions (default)"
    (ignore-case-in-search #false))
   (("-v" "--invert-match")
    "select non-matching lines"
    (invert-matches #true))
   #:args paths
   (when (ignore-case-in-search)
     (set! patterns (map add-case-insensitive-mode patterns)))
   (grep patterns paths)))

(define (add-case-insensitive-mode pattern)
  (format "(?i:~a)" pattern))

#| - Given a list of patterns and filepaths, prints lines from each file which
match at least one of the patterns. Prints error messages when it cannot access
a file, if instructed. |#
(define/mock (grep patterns paths)
  #:mock call-with-input-file #:as cwif-mock
  #:mock print-error-message #:as pem-mock #:with-behavior void
  #:mock grep-port #:as gp-mock #:with-behavior void
  #:mock directory-exists? #:as de-mock #:with-behavior (const #false)
  (for ([path paths]
        #:unless
        (let ([result (directory-exists? path)])
          (when result (print-error-message "~a: ~a\n" path "Is a directory"))
          result))
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
                #:when (xor (regexp-match? pattern line) (invert-matches)))
      ; TODO: Extract this into a function and use it in test-grep-port
      (displayln (format-match path line)))))

(define (format-match path line)
  (format "~a:~a" path line))

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
           syntax/parse/define
           (for-syntax racket/base) ; for ~?
           racket/function
           racket/string
           racket/match
           racket/list
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

  ; how to check the arguments of a mock call, and use match on them
  (with-mocks main
    (parameterize ([current-command-line-arguments
                    '#("-f" "patterns" "file")])
      (with-mock-behavior ([cwif-mock
                            (mock-call-with-input-file
                             "one\ntwo\nthree")])
        (main)
        (check-mock-called-with? g-mock (arguments '("one" "two" "three") '("file")))
        (check-true
         (match (arguments-positional (mock-call-args (last (mock-calls g-mock))))
           #| [(struct arguments ((list "one" "two" "three") (list "file"))) #true] |#
           [(list (list-no-order "one" "three" "two") (list-no-order "file")) #true]
           [_ #false])
         ))))
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

  (define (test-grep-port
           test-file-path patterns
           #:failure-message [failure-message ""]
           . expected-output-lines)
    (call-with-input-string
     (hash-ref test-strings test-file-path)
     (λ (port)
       (check-equal?
        (with-output-to-string
          (λ ()
            (grep-port patterns port test-file-path)))
        (if (null? expected-output-lines)
            ""
            (string-join
             (map (curry format-match test-file-path) expected-output-lines)
             "\n"
             #:after-last "\n"))
        failure-message
        ))))

  (test-grep-port
   "file-one.txt"
   (list #px"one")
   "one"
   #:failure-message "failed to find match in a single word file")

  (test-grep-port
   "path/to/file-two.txt"
   (list #px"one")
   "one and more things"
   #:failure-message "failed to find match in a file with one line")

  (test-grep-port
   "/absolute/path/to/file-three.txt"
   (list #px"one")
   "one"
   #:failure-message "failed to find match in a file with several lines")

  (test-grep-port
   "/absolute/path/to/file-three.txt"
   (list #px"two")
   "two"
   #:failure-message "failed to find match in a file with several lines")

  (test-grep-port
   "file-four.txt"
   (list #px"one")
   "one two three four"
   "one three four"
   "one four"
   #:failure-message "failed to match multiple lines")

  (test-grep-port
   "file-five.txt"
   (list #px"one" #px"two")
   "one two"
   "two three"
   #:failure-message "prints each matching line more than once"
   )

  (parameterize ([invert-matches #true])
    (test-grep-port
     "file-one.txt"
     (list "one")
     #:failure-message "invert match doesn't work"
     )

    (test-grep-port
     "file-four.txt"
     (list "three")
     "one four"
     #:failure-message "invert match doesn't work")
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

  ; TODO: Improve failure error reporting. It should look more like `check-mock-called-with?`
  ; TODO: Check all of the calls, similar to check-mock-called-with?
  (define-syntax-parse-rule
    (check-mock-called-with-match? mock-name:id
                                   ((~literal arguments) clauses ...)
                                   (~optional message:expr))
    (check-true
     (match (arguments-positional (mock-call-args (last (mock-calls mock-name))))
       [(list clauses ... ) #true]
       [_ #false])
     (~? message)))

  ; TODO: test main
  ; TODO: test that patterns are read correctly
  (with-mocks main
    (parameterize ([current-command-line-arguments
                    '#("-e" "one" "-e" "two" "file")])
      (main)
      (check-mock-called-with-match?
       g-mock (arguments (list-no-order "one" "two") '("file"))))

    (parameterize ([current-command-line-arguments
                    '#("-f" "patterns" "file")])
      (with-mock-behavior ([cwif-mock
                            (mock-call-with-input-file
                             "one\ntwo\nthree")])
        (main)
        (check-mock-called-with-match?
         g-mock (arguments (list-no-order "one" "two" "three") (list-no-order "file")))
        )))

  ; testing when called with paths that are directories
  ; case one: called with one path which is a directory
  (with-mocks grep
    (with-mock-behavior ([de-mock (const #true)])
      (grep (list "") (list "."))
      (check-mock-num-calls pem-mock 1)))
  )

