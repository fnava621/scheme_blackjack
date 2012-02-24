(define deck-numbers
  '(A 2 3 4 5 6 7 8 9 10 J Q K))

(define suit-types
  '(club spade heart diamond))

;; > (show-deck (list 1 2 3) (ist 4 5 6))
;; > (list (cons 1 4) (cons 1 5) (cons 1 6) (cons 2 4) (cons 2 5) (cons 2 6) (cons 3 4) (cons 3 5) cons 3 6))
(define (show-deck first-list second-list)
 (define (iter list1 list2)
    (define (iter-2 list-1 list-2)
      (if (null? list-2) (iter (cdr list1) list2)
          (append (list (cons (car list-1) (car list-2))) (iter-2 list-1 (cdr list-2)))))
    (if (null? list1) nil
        (iter-2 list1 list2)))
 (iter first-list second-list))

;; creates an entire deck. A list of 52 card types.
(define whole-deck
  (show-deck deck-numbers suit-types))

;; A recursive shuffle a list procedure. Shuffles the whole deck in this program. 
(define (shuffle-list! list)
  (define (loop in out n)
    (if (= n 0) (cons (car in) (shuffle-list! (append (cdr in) out)))
                (loop (cdr in) (cons (car in) out) (- n 1))))
  (if (null? list)
      '()
      (loop list '() (random (length list)))))


(define (play-blackjack)
  (let ((deck (shuffle-list! whole-deck)))
    (let ((dealer (cons (car deck) (cons (cadr deck) '())))
          (player (cons (caddr deck) (cons (cadddr deck) '()))))
      (display "Dealer shows")
      (newline)
      (display (car dealer))
      (newline)
      (display "You have")
      (newline)
      (display player)
      (newline)
      (set! deck (cddddr deck))
      (define (iter)
        (newline)
        (display "DO YOU WANT TO HIT? yes or no?")
        (newline)
        (let ((answer (read)))
          (cond ((eq? answer 'yes) 
                 (begin (set! player (cons (car deck) player))
                                     (display player)
                                     (cond ((and (> (count-total player) 21) (newline) "BUST! YOU LOSE!"))
                                           ((< (count-total player) 21) (begin (newline)
                                                                               (set! deck (cdr deck))
                                                                               (iter)))
                                           (else (iter)))))
                ((eq? answer 'no)
                 (cond ((and (equal? (count-total player) 21) (= 2 (length player)))
                             "BLACKJACK!!! WINNER WINNER CHICKEN DINNER")
                       ((= (count-total player) 21) (dealer-finishes-game))
                       (else (dealer-finishes-game))))
                (else "fail"))))
      (define (dealer-finishes-game)
        (newline)
        (display "Dealer shows")
        (newline)
        (display dealer)
        (newline)
        (cond ((> (count-total dealer) 21) "Dealer Busts! YOU WIN!")
              ((and (= 21 (count-total dealer)) (= 21 (count-total player))) "PUSH!")
              ((or (= 21 (count-total dealer)) 
                  (> (count-total dealer) (count-total player))) "DEALER WINS. YOU LOSE!!! :( ")
              (else (< (count-total dealer) (count-total player)) (begin (set! dealer (cons (car deck) dealer))
                                                             (set! deck (cdr deck))
                                                             (newline)
                                                             (display "Dealer Hits")
                                                             (dealer-finishes-game)))))                                                                                       
      (iter))))
              

;; You can only get four aces in one game. 
;; if you have more than one ace only one can be 11 without busting.

(define (count-total list)
  (let ((track-aces (ace-count list)) ;Uses ace count procedure to figure out how many aces does a list have. 
        (count 0) 
        (alt-count 0))
    (define (adjust-counts-for-aces)
      (if (>= track-aces 1)
          (cond ((= track-aces 1) (begin (set! count (+ 11 count))
                                         (set! alt-count (+ 1 alt-count))
                                         (tally)))
                ((= track-aces 2) (begin (set! count (+ 12 count))
                                         (set! alt-count (+ 2 alt-count))
                                         (tally)))
                ((= track-aces 3) (begin (set! count (+ 13 count))
                                         (set! alt-count (+ 3 alt-count))
                                         (tally)))
                ((= track-aces 4) (begin (set! count (+ 14 count))
                                         (set! alt-count (+ 4 alt-count))
                                         (tally)))
                (else "too many aces failure!"))
          count))
    (define (tally)
      (if (>= 21 count) count
           alt-count))
    (define (iter list-1)
      (cond ((null? list-1) (adjust-counts-for-aces))
            ((eq? (caar list-1) 'A) (iter (cdr list-1)))   
            ((or (eq? (caar list-1) 'K)
                 (eq? (caar list-1) 'Q)
                 (eq? (caar list-1) 'J)) (begin (set! count (+ count 10))
                                                (set! alt-count (+ alt-count 10))
                                                (iter (cdr list-1))))
            (else (begin (set! count (+ (caar list-1) count))
                         (set! alt-count (+ (caar list-1) alt-count))
                         (iter (cdr list-1))))))
    (iter list)))
 
    
;; Counts the number of times 'A shows up from a deck. 
  (define (ace-count whole-deck)
    (define (iter list result)
      (cond ((null? list) result)
            ((equal? (caar list) 'A) (iter (cdr list) (+ 1 result)))
            (else (iter (cdr list) result))))
    (iter whole-deck 0))
  
;; returns length of a list
(define (length list)
  (define (iter list result)
    (if (null? list) result
        (iter (cdr list) (+ 1 result))))
  (iter list 0))











