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

;; ritorna la posizione di un carattere nella stringa
;; con indice normalizzato a 0
(define string-index
  (lambda (str c)
    (let ([len (string-length str)])
      ;; named let (syntactic sugar)
      (let loop ([i 0])
        (if (= i len)
            #f
            (if (char=? (string-ref str i) c)
                i
                (loop (+ i 1))
                )
            )
        )
      )
    )
  )

;; specifico per il carattere "."
(define posizione-punto
  (lambda (str)
    (string-index str #\.)
    )
  )

;; ritorna una stringa senza il punto
(define string-no-punto
  (lambda (str)
    (let ([pos (posizione-punto str)])
      (if pos
          (string-append 
            (substring str 0 pos)
            (substring str (+ pos 1))
            )
          (substring str 0)
          )
      )
    )
  )

;; ritorna il valore numerico del primo carattere di una stringa
(define bit-val
  (lambda (alfabeto c)
    (let ([index (string-index alfabeto c)])
      (if index
          index
          (error "bit-val: carattere" c "non trovato nell'alfabeto" alfabeto)
          )
      )
    )
  )

;; ritorna il valore decimale di unas stringa binaria
(define string-base->number
  (lambda (alfabeto base str)
    (if (ha-segno? str)
        ;; salta il segno
        (string-base->number alfabeto base (substring str 1))
        (if (string=? str "")
            0
            (+ 
              (* 
                (bit-val alfabeto (string-ref str 0))
                (expt base (- (string-length str) 1)) 
                )
              (string-base->number alfabeto base (substring str 1))
              )
            )
        )
    )
  )

;; ritorna il valore decimale per cui spostare la posizione del punto nel valore decimale
(define divisore
  (lambda (base str)
    (let ([pos (posizione-punto str)] [len (string-length str)])
      (if pos ;; risolve il caso in cui non ci sia il punto e quidi expt = -1
          (expt base (- len pos 1))
          1
          )
      )
    )
  )

;; restituisce il valore decimale di una stringa in una determinata base b
;; ricavata da un alfabeto generico
(define rep->number
  (lambda (alfabeto str)
    ;; (validatore-alfabeto alfabeto) ;; controlla che l'alfabeto sia valido (non ci siano doppioni)
    (let ([base (string-length alfabeto)])
      (* (segno str)
         (/ (string-base->number alfabeto base (string-no-punto str))
            (divisore base str)
            )
         )
      )
    )
  )

;; restituisce il valore decimale di una stringa binaria
(define bin-rep->number
  (lambda (str)
    (rep->number "01" str)
    )
  )

;; tests
(display "tests binari....")(newline)
(bin-rep->number "+101")
(bin-rep->number "1010.1010")
(bin-rep->number "-1010.1011")
(bin-rep->number "+1011.10")

(display "tests base b....")(newline)
(rep->number "zu" "-uuzz")
(rep->number "0123" "+21.1")
(rep->number "01234" "-10.02")
(rep->number "oabcd" "-ao.ob")
(rep->number "0123456789ABCDEF" "0.A")
(rep->number "0123456789ABCDEF" "1CF.0")
