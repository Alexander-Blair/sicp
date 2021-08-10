#lang sicp

(define (require p)
  (if (not p) (amb)))

(define nouns 
  '(noun student professor cat class boots fur))

(define verbs 
  '(verb studies lectures eats sleeps))

(define adjectives
  '(fat stupid smelly disgusting))

(define articles '(article the a))
   
(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (list->amb seq)
  (amb . seq))

(define (parse-word word-list)
  (list (car word-list) (list->amb (cdr word-list))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb 
     verb-phrase
     (maybe-extend 
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (define (maybe-append-adjective article)
    (amb article
         (list 'article-with-adjective
               article
               (parse-word adjectives))))
  (list 'simple-noun-phrase
        (maybe-append-adjective (parse-word articles))
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb
     noun-phrase
     (maybe-extend
      (list 'noun-phrase
            noun-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define prepositions 
  '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))