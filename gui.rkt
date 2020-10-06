#lang racket
(require racket/gui)
(require pict)
(require "kana.rkt")

;The list the choice field uses
(define choice-kana-selection  '("Monographs"
                                    "Diacritics"
                                    "Monograph Slides"
                                    "Diacritic Slides"
                                    "All"
                                    "Custom"))
    
;Class that displays kana and where all the logic is located
;I want to mention that kana-display is internally modeled as a finite state machine
(define kana-display%
  (class canvas%
    (inherit get-dc get-width get-height refresh-now)
    (define kana-set #f) ; the set of kana being tested
    (define status 'null) ; binding for if the user got something right or wrong
    (define type 'kana)      ; binding for the problem type, Romanji or kana
    (define kana #f)  ;The kana itself, contains hira, kata and roma
    (define question-types '(hira)) 
    (define question #f) ;set to a funtion that when given a kana returns what is marked question
    (define answer   #f)
    (define roma-type-options '()) ;list to store possible answers for multiple choice questions
    
    (super-new)

   ;Changes active kana based on kana-set and sets the question type randomly from
   ;the pool of question types provided by the user.
    (define/private (next-kana!)
      (define i (list-ref question-types
                          (random (length question-types))))
      (set! kana (kana-set))
      (cond [(eq? i 'hira)
             (set! answer get-roma)
             (set! question get-hira)
             (set! type 'kana)]
            [(eq? i 'kata)
             (set! answer get-roma)
             (set! question get-kata)
             (set! type 'kana)]
            [(eq? i 'roma)
             ;If kata and hira are not checked default behavior is to quiz on hira
             (define options (remove 'roma question-types))
             (define len (length options))
             (cond [(> len 0)
                    (define chosen (list-ref options (random len)))
                    (set! question get-roma)
                    (set! answer
                          (cond [(eq? chosen 'kata)
                                 get-kata]
                                [(eq? chosen 'hira)
                                 get-hira]))]
                   [else (set! answer get-hira)
                         (set! question get-roma)])
             (set! type 'roma)
             (set! roma-type-options
                   (shuffle (list kana (kana-set) (kana-set) (kana-set))))]))
                 
    ; Callbacks ------------------------------------------------------------------------------------
    (define/public (make-answer-callback)
      (λ (c e)
        (when (eq? (send e get-event-type) 'text-field-enter)
          (define (correct!)
            (set! status 'right)
            (refresh-now)
            (sleep 0.7)
            (next-kana!)
            (set! status 'null))
          
          (define v (send c get-value))
          (send c set-value "")
          (cond [(eq? type 'kana)
                 (if (eq? (string->symbol v) (answer kana))
                     (correct!)
                     (set! status 'wrong))
                 (refresh-now)]
                [(eq? type 'roma)
                 (define num-string (string->number v))
                 (if (and num-string
                          (eq? (answer (list-ref roma-type-options
                                                 (- (string->number v) 1)))
                               (answer kana)))
                     (correct!)
                     (set! status 'wrong))
                 (refresh-now)]))))

    ;When box is checked, add symbol to the question type list otherwise remove
    (define/private (make-check-box-callback sym)
      (λ (c e)
        (if (send c get-value)
            (set! question-types
                  (cons sym question-types))
            (set! question-types
                  (remove sym question-types)))))
    
    (define/public (make-hira-check-box-callback)
      (make-check-box-callback 'hira))
                        
    
    (define/public (make-kata-check-box-callback)
      (make-check-box-callback 'kata))

    (define/public (make-roma-check-box-callback)
      (make-check-box-callback 'roma))

    ; User choosing which set of kana to study
    (define/public (make-kana-selection-callback)
      (λ (c e)
        (when (or (eq? (send e get-event-type) 'text-field-enter)
                  (eq? (send e get-event-type) 'text-field))
          (define x (send c get-value))
          ;As long as the selection is valid load answer
          ;If custom is chosen it reads input from the user
          ;Otherwise loads the preset then change kana and refresh canvas
          (when (member x choice-kana-selection)
            (if (string=? x "Custom")
                (set! kana-set
                      (get-customs
                       (get-text-from-user
                        "Custom"
                        (format
                         "Please type your choices in space seperated list~%for glides append the letter g to the front")
                        #f
                        "v k sk")))
                (set! kana-set (get-presets x))))
            (next-kana!)
            (refresh-now))))
    
    
    (define/override (on-paint)
      (define dc (get-dc))
      (define (format-options)
        (apply (λ (a b c d) (format "1. ~a | 2. ~a | 3. ~a | 4. ~a" a b c d))
               (map answer roma-type-options)))

      (define question-pict
        (if (not kana-set)
            (text "Welcome to Kana Learner!" 'default 20)
            (text (symbol->string (question kana)) 'default 45)))
             
      (define feedback-pict
        (cond [(eq? status 'null)
               (blank 0 30)]
              [(eq? status 'right)
               (colorize (text "Correct!" 'default 25) "green")]
              [(eq? status 'wrong)
               (colorize
                (text (format "Wrong! Answer is ~a, try again!"
                             (answer kana))
                     'default 25)
                "red")]))
     
      (define options-pict
        (cond [(eq? type 'kana)
               (blank)]
              [(eq? type 'roma)
               (text (format-options) 'default 30)]))

      (define pict
        (vc-append (blank 10)
                   question-pict
                   (blank 40)
                   feedback-pict
                   (blank 10)
                   options-pict))
      
      (draw-pict pict dc (- (/ (get-width) 2)
                            (/ (pict-width pict) 2))
                 10)
      )))



(define frame (new frame%
                   [label "Kana Learner"]
                   [width 400]
                   [height 300]))

(define main-panel (new vertical-panel% [parent frame]))
;In order to store callbacks in kana-display and have the gui
;look the way I wanted it to, I had to re arrange the children
;this command just ensures that it is only calculated once.
(send main-panel begin-container-sequence)
(define kana-display (new kana-display%[parent main-panel]))
(define horizontal-pane (new horizontal-pane%
                              [parent main-panel]
                              [stretchable-height #f]
                              [alignment '(center center)]))
(define kana-choice (new combo-field%
                         [label "choice"]
                         [choices choice-kana-selection]
                         [callback (send kana-display make-kana-selection-callback)]
                         [parent horizontal-pane]))
(define hira-check (new check-box%
                        [label "Hiragana"]
                        [parent horizontal-pane]
                        [value #t]
                        [callback (send kana-display make-hira-check-box-callback)]))
(define kata-check (new check-box%
                        [label "Katakana"]
                        [parent horizontal-pane]
                        [callback (send kana-display make-kata-check-box-callback)]))
(define roma-check (new check-box%
                        [label "Romaji"]
                        [parent horizontal-pane]
                        [callback (send kana-display make-roma-check-box-callback)]))
(send main-panel change-children reverse)
(define terminal (new text-field%
                      [parent main-panel]
                      [label "?:"]
                      [callback (send kana-display make-answer-callback)]))
(send main-panel end-container-sequence)
(send frame show #t)
                