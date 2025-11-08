#lang racket

(define ha-segno?
  (lambda (str)
    (if (string=? str "")
        #f
        (let ([c (string-ref str 0)])
          (cond
            ((char=? c #\+) #t)
            ((char=? c #\-) #t)
            (else #f)
            )
          )
        )
    )
  )
;; ritorna il valore positivo o negativo del primo carattere di una stringa
;; se il primo carattere è un "-" restituisce -1, altrimenti +1
;; assumendo che la stringa non possa essere vuota e che contenga un valore numerico interpretabile
(define segno
  (lambda (str)
    (if (char=? (string-ref str 0) #\-)
        -1
        +1
        )
    )
  )

;; ritorna la posizione del carattere punto nella stringa
;; con indice normalizzato a 0
(define posizione-punto
  (lambda (str)
    (if (string=? str "")
        0
        (if (char=? (string-ref str 0) #\.)
            0
            (+ 1 (posizione-punto (substring str 1)))
            )
        )
    )
  )

;; ritorna una stringa senza il punto
(define string-no-punto
  (lambda (str)
    (let ([pos (posizione-punto str)])
      (string-append 
        (substring str 0 pos)
        (if (= pos (string-length str))
            ""
            (substring str (+ pos 1))
            )
        )
      )
    )
  )

;; ritorna il valore numerico del primo carattere di una stringa
(define bit-val
  (lambda (c)
    (cond
      ((char=? c #\0) 0)
      ((char=? c #\1) 1)
      (else (error "bit-val: carattere non valido" c))
      )
    )
  )

;; ritorna il valore decimale di unas stringa binaria
(define string-bin->number
  (lambda (str)
    (if (ha-segno? str)
        (string-bin->number (substring str 1))
        (if (string=? str "")
            0
            (+ 
              (* 
                (bit-val (string-ref str 0))
                (expt 2 (- (string-length str) 1)) 
                )
              (string-bin->number (substring str 1))
              )
            )
        )
    )
  )

;; ritorna il valore decimale per cui spostare la posizione del punto nel valore decimale
(define divisore
  (lambda (str)
    (let ([pos (posizione-punto str)] [len (string-length str)])
      (if (= pos len) ;; risolve il caso in cui non ci sia il punto e quidi expt = -1
          1
          (expt 2 (- len pos 1))
          )
      )
    )
  )

(define bin-rep->number
  (lambda (str)
    (let ([num (* (segno str)
                  (/ (string-bin->number (string-no-punto str)) (divisore str))
                  )])
      num
      )
    )
  )

(bin-rep->number "+101")
(bin-rep->number "1010.1010")
(bin-rep->number "-1010.1011")
(bin-rep->number "+1011.10")
