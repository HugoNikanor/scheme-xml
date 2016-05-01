(define (get-tag data)
  (car data))

(define (get-atributes data)
  (cadr data))

(define (get-body data)
  (cddr data))

(define (tag? data)
  (list? data))

(define (make-indent length indent-width)
  (list->string (make-list (* length indent-width) #\space)))

; name="val1 val2"
(define (format-atribute atribute)
  (if (null? atribute)
    '()
    (string-append (string (car atribute)) "="
                   (surround #\" (accumulate string-append "" (interspace " " (map string (cdr atribute))))))))


; see sample.scm for how the data is expressed in scheme
(define (make-xml data) 
  (define (inner data nest-level)
    (let ((indent-width 4))
      (if (not (tag? data))
        (begin (for-each display (list (make-indent nest-level indent-width) data)) (newline))
        (begin 
          (display (make-indent nest-level indent-width))
          (display "<")
          (display (get-tag data))
          (for-each display (cons " " (interspace #\space (map format-atribute (get-atributes data)))))
          (display ">")
          (newline)
          (if (not (null? (get-body data)))
            (map (lambda (x) (inner x (+ nest-level 1))) (get-body data)))
          (for-each display 
                    (list 
                      (make-indent nest-level indent-width)
                      "</"
                      (get-tag data)
                      ">\n"))))))
  (inner data 0))
