#lang racket/base

(require rackunit)
(require "util.rkt")

(define x
  (cons (quote chicago)
        (cons (quote pizza)
              (quote ()))))
x

(set! x (quote gone))
x

;(set! y (quote yet))
;y

;(define x (quote again))
;x

(set! x (quote skins))
x

(define gourmet
  (lambda (food)
    (cons food
          (cons x (quote ())))))

(gourmet (quote onion))

(cons x (quote ()))

(gourmet (quote onion))

(set! x (quote rings))

(gourmet (quote onion))

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x (quote ())))))

(gourmand (quote potato))
x

(gourmand (quote rice))
x

(define diner
  (lambda (food)
    (cons (quote milkshake)
          (cons food (quote ())))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons (quote milkshake)
          (cons food (quote ())))))

(dinerR (quote onion))
x

(dinerR (quote pekanpie))
x

(gourmand (quote onion))
x

(define omnivore
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x (quote ()))))))

(omnivore (quote bouillabaisse))
x

(define gobbler
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x (quote ()))))))

(gobbler (quote gumbo))
x

(define nibbler
  (lambda (food)
    (let ((x (quote donut)))
      (set! x food)
      (cons food
            (cons x (quote ()))))))

(nibbler (quote cheerio))
x

(define omnivore2
  (let ((x (quote minestrone)))
    (lambda (food)
      (cons food
            (cons x (quote ())))
      (set! x food))))

(omnivore2 (quote bouillabaisse))
(omnivore2 (quote bouillabaisse2))
(omnivore2 (quote bouillabaisse3))

(define nibbler2
  (lambda (food)
    (let ((x (quote donut)))
      (cons food
            (cons x (quote ())))
      (set! x food))))

(nibbler2 (quote cheerio))
(nibbler2 (quote cheerio2))
(nibbler2 (quote cheerio3))

(define food (quote none))
(define glutton
  (lambda (x)
    (set! food x)
    (cons (quote more)
          (cons x
                (cons (quote more)
                      (cons x (quote ())))))))

(glutton (quote garlic))
food
x

(define chez-nous
  (lambda ()
    (set! food x)
    (set! x food)))

(chez-nous)
food
x

(glutton (quote garlic))
(gourmand (quote potato))
food
x

(define chez-nous2
  (lambda ()
    (let ((a food))
    (set! food x)
    (set! x a))))

(chez-nous2)
food
x
