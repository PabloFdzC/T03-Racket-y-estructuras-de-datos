#lang racket
(define (node id name value) (list (list id name value) '()))

(define (insert-node tree parent id name value)
  (cond
    [(equal? (caar tree) parent) (list (car tree) (append (cadr tree) (list (node id name value))))]
    [(= (length (cdr tree)) 0) tree]
    [else (list (car tree)
                (for/list ([elem (cadr tree)])
                  (insert-node elem parent id name value)
                  )
                )]
    ))
