
#lang racket

(require pict
         file/gif
         mrlib/gif)
(module+ test
(require rackunit))

(define (build-grid size proc)
  (build-vector size (λ (i) (build-vector size (λ (j) (proc i j))))))

(define (build-random-grid size)
  (build-grid (λ (i j) (random))))

(define (make-grid size [v 0])
  (build-vector size (λ (i) (make-vector size v))))

(define (grid-set! grid xCell yCell value)
  (vector-set! (vector-ref grid xCell) yCell value))

(define (get-cell-value size field xCell yCell)
  (define size-1 (sub1 size))
  (if (and (<= 0 xCell size-1) (<= 0 yCell size-1))
      (vector-ref (vector-ref field xCell) yCell)
      0))

(define (get-window-range coord window-half-size)
  (range (- coord window-half-size) (+ coord window-half-size 1)))
(module+ test
  (check-equal? (get-window-range 4 1) (range 3 6))
  (check-equal? (length (get-window-range 4 1)) 3))


(define (get-cell-sum size field xCell yCell window-half-size f)
  (foldl
   (λ (i acci)
     (+ acci
        (foldl
         (λ (j accj)
           (if (and (equal? i xCell) (equal? j yCell))
               accj
               (+ accj (f (get-cell-value size field i j) i j xCell yCell))))
         0
         (get-window-range yCell window-half-size))))
   0
   (get-window-range xCell window-half-size)))

(module+ test
  (let* ((size 3)
         (field (make-grid size))
         (f (λ (v i j xCell yCell) v)))
    (check-equal? (get-cell-value size field 0 0) 0)
    (check-equal? (get-cell-value size field 2 2) 0)
    (grid-set! field 0 0 1)
    (check-equal? (get-cell-value size field 0 0) 1)
    (check-equal? (get-cell-sum size field 1 1 1 f) 1)
    (grid-set! field 2 2 1)
    (check-equal? (get-cell-value size field 2 2) 1)
    (check-equal? (get-cell-sum size field 1 1 1 f) 2)))

(define (next-cell-value size field xCell yCell window-half-size f)
  (define sum (get-cell-sum size field xCell yCell window-half-size f))
  (if (or (equal? sum 3)
          (and (equal? (get-cell-value size field xCell yCell) 1)
               (equal? sum 2)))
      1
      0))

(define (next-generation size field window-half-size f)
  (define result (make-grid size))
  (for-each
   (λ (i)
     (for-each
      (λ (j)
        (grid-set! result i j (next-cell-value size field i j  window-half-size f)))
      (range size)))
   (range size))
  result)


(module+ test
  (define size 3)
  (define g0 (make-grid size))
  (define f (λ (v i j xCell yCell) v))
  (grid-set! g0 0 1 1)
  (grid-set! g0 1 1 1)
  (grid-set! g0 2 1 1)
  (define g1 (next-generation size g0 1 f))
  (check-equal? (get-cell-value size g1 0 0) 0)
  (check-equal? (get-cell-value size g1 0 1) 0)
  (check-equal? (get-cell-value size g1 0 2) 0)
  (check-equal? (get-cell-value size g1 1 0) 1)
  (check-equal? (get-cell-value size g1 1 1) 1)
  (check-equal? (get-cell-value size g1 1 2) 1)
  (check-equal? (get-cell-value size g1 2 0) 0)
  (check-equal? (get-cell-value size g1 2 1) 0)
  (check-equal? (get-cell-value size g1 2 2) 0)
  )

;(define (grid->image size grid)
;  (build-image size size
;               (λ (xx yy)
;                 (get-cell-value size grid xx yy))))

(define (grid->gif-bytes size grid)
  (apply bytes-append
  (map
   (λ (i)
     (define line-bytes (make-bytes (* 3 size)))
     (for-each
      (λ (j)
        (define color-value (255 * (get-cell-value size grid i j)))
        (for-each 
         (λ (iColor)
           (bytes-set! line-bytes (+ (* 3 j) iColor) color-value))
         (range 3)))
      (range size))
     line-bytes)
   (range size))))


(define (grid->gif size start-grid nb-gen filename window-half-size f)
  (call-with-output-file filename
    (lambda (out)
      (gif-start out size size 0 #f)
      (define g start-grid)
      (for-each
       (λ (i-gen)
         (gif-add-image out 0 0 size size #f (grid->gif-bytes g))
         (gif-add-control out 'any #f 10 #f)
         (set! g (next-generation size g window-half-size f)))
       (range nb-gen))
      (gif-end out))
    #:exists 'replace))

; Créer une série d'images simples
(define (create-frame i)
  (text (number->string i) null  240))

; Créer une liste de frames (ici, des chiffres de 0 à 9)
(define frames (map create-frame (range 10)))

; Convertir les frames en bitmaps
(define bitmaps
  (map pict->bitmap frames))

; Enregistrer les frames en tant que GIF animé
(define (save-gif filename bitmaps)
      (write-animated-gif bitmaps 100 filename #:disposal 'restore-bg))

