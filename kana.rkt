#lang racket
(require racket/vector)
(require racket/string)
(provide get-presets get-customs get-hira get-kata get-roma) 


(define monographs '(v k s t n h m y r s w nn)) 
(define diacritics '(g z d b p))
(define mono-slides '(gk gs gt gn gh gm gr))
(define dia-slides '(gg gz gb gp))
(define all (append monographs diacritics mono-slides dia-slides))

(define presets (hash "Monographs" monographs
                      "Diacritics" diacritics
                      "Monograph Slides" mono-slides
                      "Diacritic Slides" dia-slides
                      "All" all))

;Gives the gui the random kana lambda based on the presets
(define (get-presets key)
  (random-kana (hash-ref presets key)))

;Gives the gui the random kana lambda based on custom user input
(define (get-customs input)
  (define str (open-input-string input))
  (random-kana
   (for/list ([i (in-port read str)])
     i)))

(define (get-hira x)
  (car x))
(define (get-kata x)
  (cadr x))
(define (get-roma x)
  (caddr x))

;Given a set of symbols it loads them if they exist in the kana.txt file
;returning a lambda closed by the vector of all kana within its set
;that lambda when called returns a random kana from its set
(define (random-kana set)
  (let* ([vec (load-set "kana.txt" set)]
         [len (vector-length vec)])
    (λ () (vector-ref vec (random len)))))

;Loads set from txt file. should be reasonably fast due to lazy streams
;If I have time in the future I would like to make this more efficient but as
;I am on a bit of a time crunch this should be more than fine.
(define (load-set path lst)
  (define port (open-input-file path))
  (define (filter-pred x)
    (member (read (open-input-string x)) lst))
  (for/fold ([acc #()])
            ([lns (sequence-filter filter-pred (in-lines port))]
             #:break (eof-object? lns))
    (define str (open-input-string lns))
    (read str) ;discards the group identifier as its no longer needed
    (vector-append acc
     (for/vector ([i (in-port read str)])
       i))))