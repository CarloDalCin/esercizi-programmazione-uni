;; Determina se un sostantivo è plurale
(define (plurale? s)
  (let ((ultima (string-ref s (- (string-length s) 1))))
    (or (char=? ultima #\i) (char=? ultima #\e))))

;; Aggiunge l'articolo corretto
(define (aggiungi-articolo s)
  (let ((ultima (string-ref s (- (string-length s) 1))))
    (cond ((char=? ultima #\i) (string-append "i " s))   ; maschile plurale
          ((char=? ultima #\o) (string-append "il " s))  ; maschile singolare
          ((char=? ultima #\e) (string-append "le " s))  ; femminile plurale
          ((char=? ultima #\a) (string-append "la " s))  ; femminile singolare
          (else s))))

;; Coniuga il verbo in base alla radice e al tipo
(define (coniuga verbo soggetto)
  (let* ((radice (substring verbo 0 (- (string-length verbo) 3)))
         (desinenza (substring verbo (- (string-length verbo) 3)))
         (plurale (plurale? soggetto)))
    (cond
      ((string=? desinenza "are")
       (if plurale (string-append radice "ano") (string-append radice "a")))
      ((or (string=? desinenza "ere") (string=? desinenza "ire"))
       (if plurale (string-append radice "ono") (string-append radice "e")))
      (else verbo))))

;; Costruisce la frase completa
(define (frase sog verbo compogg)
  (string-append
    (aggiungi-articolo sog)
    " "
    (coniuga verbo sog)
    " "
    (aggiungi-articolo compogg)))

;; Test
(display (frase "gatto" "inseguire" "topi")) (newline)
(display (frase "mucca" "ruminare" "fieno")) (newline)
(display (frase "scolare" "leggere" "novella")) (newline)
(display (frase "bambini" "ascoltare" "favole")) (newline)
(display (frase "musicisti" "suonare" "piano")) (newline)
(display (frase "cuoco" "friggere" "patate")) (newline)
(display (frase "camerieri" "servire" "clienti")) (newline)
(display (frase "mamma" "vestire" "figlie")) (newline)

