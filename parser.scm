(define (parse-xml file-path)
  (define file (open-input-file file-path))

  
  (define (create-objcet tag atributes . body)
    (list tag
          atributes
          body))

  (define (find-start-tag)
    (let ((c (read-char file)))
      (if (evq? c #\<)

