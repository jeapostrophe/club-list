#lang racket/base
(require racket/runtime-path
         racket/contract
         racket/date
         racket/file
         racket/string
         unstable/contract
         web-server/servlet
         web-server/formlets
         web-server/servlet-env)

(define-runtime-path source-dir ".")

(define (footer)
  `(div ([id "footer"])
        "Powered by "
        (a ([href "http://racket-lang.org/"]) "Racket") ". "
        "Written by "
        (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy")
        "."
        (br)
        (span ([id "timestamp"])
              ,(date->string (seconds->date (current-seconds)) #t))))

(define (template title . bodies)
  (response/xexpr
   `(html
     (head (title ,title)
           (link ([rel "stylesheet"]
                  [type "text/css"]
                  [href "/style.css"])))
     (body
      (div ([class "content"])
           ,@bodies)
      ,(footer)))))

(define (single-text-box-page label submit-text template)
  (define the-formlet
    (formlet
     (table
      (tr (td ,label)
          (td ,{(to-string (required (text-input))) . => . ans})))
     ans))
  (define the-req
    (send/suspend
     (λ (k-url)
       (template
        `(div ([id "query"])
              (form ([action ,k-url] [method "post"])
                    ,@(formlet-display the-formlet)
                    (input ([type "submit"] [value ,submit-text]))))))))
  (formlet-process the-formlet the-req))

(define (page/login admin-pw req)
  (define email
    (single-text-box-page
     "Email:" "Log In"
     (λ (query)
       (template
        "Login"
        query))))

  (cond
    [(string=? admin-pw email)
     'admin]
    [(regexp-match #rx"^.+@.+\\..+$" email)
     email]
    [else
     (page/login admin-pw req)]))

(define (page/admin events-path users-path req)
  ;; XXX
  (template "Admin"))

(define (valid-event? events-path s)
  ;; XXX
  #f)

(define (event-name events-path s)
  ;; XXX
  "XXX")

(define (page/normal events-path users-path user survey-pw survey-url req)
  (define user-path (build-path users-path user))

  (define user-info-f
    (case-lambda
      [()
       (if (file-exists? user-path)
         (file->value user-path)
         (hash))]
      [(new-info)
       (write-to-file new-info user-path #:exists 'replace)]))

  (page/normal/survey user-info-f survey-pw survey-url req)

  (page/normal/event events-path user-info-f req))

(define (page/normal/survey user-info-f survey-pw survey-url req)
  (define i (user-info-f))
  (unless (equal? (hash-ref i 'survey-pw #f) survey-pw)
    (define ans
      (single-text-box-page
       "What is the registration password?" "Verify"
       (λ (query)
         (template
          "Registration"
          `(div ([id "register"])
                (p "Please fill out the " 
                   (a ([href ,survey-url]) "registration survey")
                   " to continue.")
                (p "For your convenience, it is embedded below:")

                (iframe ([src ,(format "~a?embedded=true" survey-url)]
                         [width "760"]
                         [height "500"]
                         [frameborder "0"]
                         [marginheight "0"]
                         [marginwidth "0"])
                        "Loading..."))
          query))))

    (user-info-f (hash-set (user-info-f) 'survey-pw ans))

    (page/normal/survey user-info-f survey-pw survey-url req)))

(define (page/normal/event events-path user-info-f req)
  (define ans
    (single-text-box-page
     "Which event did you attend?" "Report"
     (λ (query)
       (template
        "Event"
        query))))

  (cond
    [(valid-event? events-path ans)
     (user-info-f
      (hash-set (user-info-f) ans #t))

     (send/back
      (template
       "Event confirmation"
       `(div ([id "confirm"])
             "Thank you for attending " ,(event-name events-path ans) ".")))]
    [else
     (page/normal/event events-path user-info-f req)]))

(define ((page/start events-path users-path admin-pw survey-pw survey-url) req)
  (define user (page/login admin-pw req))
  (cond
    [(eq? 'admin user)
     (page/admin events-path users-path req)]
    [else
     (page/normal events-path users-path user survey-pw survey-url req)]))

(define (go root-path port survey-url)
  (define (read-pw p)
    (string-trim (file->string (build-path root-path p))))
  (define admin-pw (read-pw "admin"))
  (define survey-pw (read-pw "survey"))
  (define users-path (build-path root-path "users"))
  (make-directory* users-path)
  (define events-path (build-path root-path "events"))
  (make-directory* events-path)
  (serve/servlet
   (page/start events-path users-path admin-pw survey-pw survey-url)
   #:port port
   #:listen-ip #f
   #:command-line? #t
   #:quit? #f
   #:launch-browser? #f
   #:extra-files-paths (list (build-path source-dir "static"))
   #:servlet-regexp #rx""
   #:servlet-path "/"))

(provide/contract
 [go (-> path-string? port-number? string?
         void)])
