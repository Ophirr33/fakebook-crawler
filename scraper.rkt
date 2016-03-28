#lang racket
(require racket/tcp)
(require racket/async-channel)

; sends a get request for the given filepath
(define (get file-path)
  (define-values (in out) (tcp-connect "fring.ccs.neu.edu" 80))
  (display (format "GET ~a HTTP/1.1\r\nHost: fring.ccs.neu.edu\r\nCookie: ~a; ~a\r\nConnection: Close\r\n\r\n"
                   file-path session csrf) out)
  (flush-output out)
  (port->string in))

; sends a post for the given filepath
(define (post file-path)
  (define-values (in out) (tcp-connect "fring.ccs.neu.edu" 80))
  (let ([creds (format "username=~a&password=~a&csrfmiddlewaretoken=~a"
                       username password (cadr (string-split csrf "=")))])
    (display (format "POST ~a HTTP/1.1\r\nHost: fring.ccs.neu.edu\r\nCookie: ~a; ~a\r\nContent-Length: ~a\r\nConnection: Close\r\n\r\n~a\r\n\r\n"
                     file-path session csrf (string-length creds) creds) out)
    (flush-output out)
    (port->string in)))

; sets the cookies
(define (set-cookie response)
  (define (grab-response x old)
    (if x (car x) old))
  (set! csrf (grab-response (regexp-match #rx"csrftoken=[^;]+" response) csrf))
  (set! session (grab-response (regexp-match #rx"sessionid=[^;]+" response) session)))

; Parse the response and return a pair of the code and 
(define (parse-response response)
  (when (not (regexp-match? #rx"^HTTP/1.1" response))
    (error "got a non HTTP/1.1 response"))
  (define code (string->number (cadr (string-split response))))
  (set-cookie response)
  (cons code
        (cond [(= 200 code)
               (last (string-split response "\r\n\r\n"))]
              [(or (= 301 code) (= 302 code))
               (car (string-split
                     (car (regexp-match #rx"http://.+/"
                                        (car (regexp-match #rx"Location: [^(\r\n)]+" response))))
                     "http://fring.ccs.neu.edu"))]
              [(or (= code 403) (= code 404) (= code 504) (= code 500)) #f]
              [else response])))

; Grabs all the hyperlinks out of the html, and filters them
; to only be fakebook links
(define (parse-links html)
  (filter (λ (x) (or (string=? "/" (substring x 0 1))
                     (string=? "fring" (substring x 0 5))
                     (string=? "http://fring" (substring x 0 12))))
          (map (λ (x) (substring x 9))
               (regexp-match* #rx"<a href=\"[^\"]+" html))))

; Grabs all the secret flags out of the html
(define (parse-flags html)
  (map (λ (x) (substring x 6)) (regexp-match* #rx"FLAG: [^<]+" html)))

; Prints the given list
(define (print-list l)
  (for ([x l])
    (println x)))

;; LOGGING IN
(define (login)
  (parse-response (get "/accounts/login/?next=/fakebook/"))
  (parse-response (post "/accounts/login/?next=/fakebook/")))

;; Book keeping
(define csrf #f)
(define session #f)
(define username "001719068")
(define password "3U4A57BB")
(define visited (set))
(define flags (set))
(define todo (make-async-channel))

(define main-thread
  (thread (λ () (async-channel-put todo (cdr (login)))
                (build-list 8 (λ (x) (thread thread-action)))
                (thread-action)
                (print-list flags)
                (kill-thread main-thread))))

;; main action performed by a single thread
(define (thread-action)
  (if (= (set-count flags) 5) flags
      (let ([url (async-channel-get todo)])
        (if (set-member? visited url) (thread-action)
            (begin (set! visited (set-add visited url))
                   (crawl url)
                   (thread-action))))))

;; Crawls a single webpage
(define (crawl url)
  (let* ([response (parse-response (get url))]
         [code (car  response)]
         [body (cdr response)])
    ;(displayln (format "url: ~a flags: ~a code: ~a" url flags code))
    (cond [(= 200 code)
           (set! flags (set-union (list->set (parse-flags body)) flags))
           (for ([l (parse-links body)]) (async-channel-put todo l))]
          [(or (= 301 code) (= 302 code))
           (set! todo (cons body todo))]
          [(and (>= 500 code) (< code 600))
           (crawl url)])))

#|
Ceri's flags:
 "21a0e8a49821c56b34672977d0f16ef9757f5d7d8121c74dc4df3495f63f3770"
 "1a764fe971761dba1a945a3399a01ef7cf76c2e606c0273f5d0fe56d27962187"
 "1624418366bb42353d3f096e211f2737d9899fd0771f398355503c186a4dda2a"
 "fe8a6195c7a88b392507fca0dd1165d6893b9f2d60eaecee3ccfb114984b4bcf"
 "81df8171e41c3aec26fcd0db81a12825ebdba9c1a2b30055309b499053369a84"
Ty's flags:
 "96f5610cf66129990f567c8e7cc5c5f6e4356b2dace449e816fea89c93b2d39b"
 "4c045db5e51c59401858974089e5e75f09fcedcc8f7628184e66dd33671dbabb"
 "2d08642c39879868f0bec957c3b642333e7b5371bee41ccbd37b93156160f5f6"
 "6869c3e2628a417d9fa24775e2171d5c24a5c5fe499c9d22b41455251109bd04"
 "9259a92dcd58eafd9c0d0000c5874c6e06e1fbfc0066d3a6284c83839a6b117d"
|#