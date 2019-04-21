#lang typed/racket

(require typed/test-engine/racket-tests)

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")
(require (only-in typed/racket/gui/base put-file get-file))

; data definitions

(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))

(define-struct PhysicalLoc
  ([x-offset-from-left : Integer]
   [y-offset-from-top  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))

(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))

(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))

; logical->physical: given a LogicalLoc, translate it to the physical location
; the integer argument is the dimension (locations per side) of the board
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical logloc dimension boardspec)
  (cond
    [(or (negative? dimension)
         (negative? (LogicalLoc-row logloc))
         (negative? (LogicalLoc-col logloc))
         (> (LogicalLoc-row logloc) dimension)
         (> (LogicalLoc-col logloc) dimension))
     (error "This Logical Location is not on the board.")]
    [else
     (PhysicalLoc (+ (BoardSpec-margin-pixels boardspec)
                     (* (LogicalLoc-col logloc)
                        (BoardSpec-cell-size-pixels boardspec)))
                  (+ (BoardSpec-margin-pixels boardspec)
                     (* (- (sub1 dimension) (LogicalLoc-row logloc))
                        (BoardSpec-cell-size-pixels boardspec))))]))

(check-error (logical->physical
              (LogicalLoc 100 100)
              19
              (BoardSpec 'tan 10 12 3))
             "This Logical Location is not on the board.")
(check-expect (logical->physical
               (LogicalLoc 0 0)
               4
               (BoardSpec 'tan 10 12 3))
              (PhysicalLoc 12 42))
(check-expect (logical->physical
               (LogicalLoc 1 0)
               4
               (BoardSpec 'tan 10 12 3))
              (PhysicalLoc 22 42))
(check-expect (logical->physical
               (LogicalLoc 0 1)
               4
               (BoardSpec 'tan 10 12 3))
              (PhysicalLoc 12 32))
(check-expect (logical->physical
               (LogicalLoc 2 2)
               4
               (BoardSpec 'tan 10 12 3))
              (PhysicalLoc 32 22))
(check-expect (logical->physical
               (LogicalLoc 0 0)
               19
               (BoardSpec 'tan 15 25 6))
              (PhysicalLoc 25 295))

; physicalDist: calculates the physical distance between two points
(: physicalDist : Integer Integer Integer Integer -> Real)
(define (physicalDist x0 y0 x1 y1)
  (local {(define dx (- x0 x1))
          (define dy (- y0 y1))}
    (sqrt (+ (* dx dx) (* dy dy)))))

(check-expect (physicalDist 0 0 10 0) 10)
(check-expect (physicalDist 3 0 0 4) 5)
(check-within (physicalDist 13 18 4 12) 10.816 0.001)

; gdline-round: takes the margin size, cell size, and Real and rounds
; the real to the nearest grid line (depending on margin + cell size).
; The next few functions were taken from the reference code, as I believed that
; this method to approaching the function was way more efficient than what
; I was previously using, despite the fact that my old methods had worked.
(: gdline-round : Integer Integer Real -> Integer)
(define (gdline-round margin gap-size x)
  (if (>= x 0)
      (+ margin (* gap-size (exact-round (/ (- x margin) gap-size))))
      margin))

; as mentioned in the reference code,
; this function works for all decimals except for a decimal that ends in 0.5
(check-expect (gdline-round 5 1 4.9) 5)
(check-expect (gdline-round 5 1 5.1) 5)
(check-expect (gdline-round 5 1 5.4) 5)
(check-expect (gdline-round 5 1 5.6) 6)
(check-expect (gdline-round 5 1 5.9) 6)
(check-expect (gdline-round 5 1 9.9) 10)
(check-expect (gdline-round 5 5 4.9) 5)
(check-expect (gdline-round 5 5 5.9) 5)
(check-expect (gdline-round 5 5 9.9) 10)
(check-expect (gdline-round 5 5 -99999) 5)

; onBoard?: checks if a logical location is on the board, given dimension
; taken from reference code
(: onBoard? : Integer LogicalLoc -> Boolean)
(define (onBoard? dim lloc)
  (match lloc
    [(LogicalLoc x y) (and (<= 0 x (sub1 dim)) (<= 0 y (sub1 dim)))]))

(check-expect (onBoard? 4 (LogicalLoc 5 0)) false)
(check-expect (onBoard? 19 (LogicalLoc 12 15)) true)

; convert physical location to logical, if within stone radius of a stone location
; taken from reference code
(: physical->logical : PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical physloc dimension bspec)
  (match physloc
    [(PhysicalLoc x y)
     (match bspec
       [(BoardSpec _ cell margin stone)
        (local
          {(define nearx (gdline-round margin cell x))
           (define neary (gdline-round margin cell y))
           (define lloc-candidate
             (LogicalLoc
              (quotient (- nearx margin) cell)
              (quotient (- (* (sub1 dimension) cell) (- neary margin)) cell)))}
          (if (and (onBoard? dimension lloc-candidate)
                   (<= (physicalDist x y nearx neary) stone))
              (Some lloc-candidate)
              'None))])]))

(check-expect (physical->logical (PhysicalLoc 21 41) 4 (BoardSpec 'tan 10 12 3))
              (Some (LogicalLoc 1 0)))
(check-expect (physical->logical (PhysicalLoc 50 50) 4 (BoardSpec 'tan 10 12 3))
              'None)
(check-expect
 (physical->logical (PhysicalLoc 100 150) 19 (BoardSpec 'blue 20 24 8))
 (Some (LogicalLoc 4 12)))

; char->string converts a character into a string
(: char->string : Char -> String)
(define (char->string char)
  (string char))

(check-expect (char->string #\c) "c")
(check-expect (char->string #\P) "P")

; defines the Go alphabet, missing "I" for labeling on the go board
(: GoLetterLabels (Listof Char))
(define GoLetterLabels
  (string->list "ABCDEFGHJKLMNOPQRSTUVWXYZ"))

; convert the column index to a string label
; taken from reference code
; 0 => "A", ..., 24 => "Z", 25 => "AA", ...
(: column->string : Integer -> String)
(define (column->string n)
  (make-string (add1 (quotient n 25))
               (list-ref GoLetterLabels (remainder n 25))))

(check-expect (column->string 0) "A")
(check-expect (column->string 24) "Z")
(check-expect (column->string 25) "AA")
(check-expect (column->string 26) "BB")

; produce a string label for a logical location
; ex: (logical->string (LogicalLoc 0 0)) => "A1"
; taken from reference code
(: logical->string : LogicalLoc -> String)
(define (logical->string lloc)
  (match lloc
    [(LogicalLoc col row)
     (string-append (column->string col) (number->string (add1 row)))]))

(check-expect (logical->string (LogicalLoc 0 0))  "A1")
(check-expect (logical->string (LogicalLoc 1 0))  "B1")
(check-expect (logical->string (LogicalLoc 0 1))  "A2")
(check-expect (logical->string (LogicalLoc 25 0)) "AA1")
(check-expect (logical->string (LogicalLoc 0 3)) "A4")
(check-expect (logical->string (LogicalLoc 1 3)) "B4")
(check-expect (logical->string (LogicalLoc 3 3)) "D4")
(check-expect (logical->string (LogicalLoc 11 17)) "M18")


; findn: returns the nth item in the list
; if there aren't enough items, returns 'NotFound
(: findn : All (a) Integer (Listof a) -> (U 'NotFound a))
(define (findn n xs)
  (cond
    [(empty? xs) 'NotFound]
    [(= n 1) (first xs)]
    [else (findn (- n 1) (rest xs))]))

(check-expect (findn 1 '(1 2 3 4)) 1)
(check-expect (findn 2 '(1 2 3 4)) 2)
(check-expect (findn 3 '(1 2 3 4 5 6)) 3)
(check-expect (findn 4 '(1 2 3 4 5 6)) 4)
(check-expect (findn 10 '(1 2 3 4 5 6)) 'NotFound)

; empty-board: creates an empty board with the given dimension
(: empty-board : Integer -> Board)
(define (empty-board dim)
  (build-vector dim
                (lambda ([i : Integer])
                  (make-vector dim (cast 'None (Optional Stone))))))

; defines a test Go game for testing purposes
(: testGo Go)
(define testGo
  (Go (vector (vector 'None 'None (Some 'white) (Some 'black))
              (vector (Some 'black) (Some 'white) 'None 'None)
              (vector 'None (Some 'white) (Some 'black) 'None)
              (vector (Some 'black) 'None (Some 'white) 'None))
      'black
      (list (vector (vector 'None 'None (Some 'white) (Some 'black))
                    (vector (Some 'black) (Some 'white) 'None 'None)
                    (vector 'None (Some 'white) (Some 'black) 'None)
                    (vector (Some 'black) 'None 'None 'None))
            (vector (vector 'None 'None (Some 'white) 'None)
                    (vector (Some 'black) (Some 'white) 'None 'None)
                    (vector 'None (Some 'white) (Some 'black) 'None)
                    (vector (Some 'black) 'None 'None 'None))
            (vector (vector 'None 'None (Some 'white) 'None)
                    (vector (Some 'black) 'None 'None 'None)
                    (vector 'None (Some 'white) (Some 'black) 'None)
                    (vector (Some 'black) 'None 'None 'None))
            (vector (vector 'None 'None (Some 'white) 'None)
                    (vector (Some 'black) 'None 'None 'None)
                    (vector 'None (Some 'white) 'None 'None)
                    (vector (Some 'black) 'None 'None 'None)))
      (Some (LogicalLoc 0 3))
      '()
      '()
      0))

; gameBeginning: creates the beginning of game with integer dimension
(: gameBeginning : Integer -> Go)
(define (gameBeginning dimension)
  (local
    {(: vec1 : (Vectorof (Optional Stone)))
     (define vec1 (make-vector dimension 'None))
     (: vec2 : (Vectorof (Vectorof (Optional Stone))))
     (define vec2 (make-vector dimension vec1))
     (: lp : Integer -> Board)
     (define (lp i)
       (cond
         [(= i dimension) vec2]
         [else
          (begin
            (local
              {(: v1 : (Vectorof (Optional Stone)))
               (define v1 (make-vector dimension 'None))}
              (vector-set! vec2 i v1)
              (lp (+ i 1))))]))}
    (Go (lp 0) 'black '() 'None '() '() 0)))

(check-expect (gameBeginning 2) (Go
                                 (vector (vector 'None 'None)
                                         (vector 'None 'None))
                                 'black
                                 '()
                                 'None
                                 '()
                                 '()
                                 0))
(check-expect (gameBeginning 5) (Go
                                 (vector (vector 'None 'None 'None 'None 'None)
                                         (vector 'None 'None 'None 'None 'None)
                                         (vector 'None 'None 'None 'None 'None)
                                         (vector 'None 'None 'None 'None 'None)
                                         (vector 'None 'None 'None 'None 'None))
                                 'black
                                 '()
                                 'None
                                 '()
                                 '()
                                 0))

; appearsLL?: returns true if the given LogicalLoc appears in a list of LogicalLoc
(: appearsLL? : LogicalLoc (Listof LogicalLoc) -> Boolean)
(define (appearsLL? LL list)
  (match list
    ['() false]
    [(cons head tail)
     (if (and (= (LogicalLoc-col head) (LogicalLoc-col LL))
              (= (LogicalLoc-row head) (LogicalLoc-row LL)))
         true
         (appearsLL? LL tail))]))

(check-expect (appearsLL? (LogicalLoc 0 0)
                          (list (LogicalLoc 0 1) (LogicalLoc 1 0) (LogicalLoc 0 0)))
              #t)

(check-expect (appearsLL? (LogicalLoc 0 4)
                          (list (LogicalLoc 0 1) (LogicalLoc 1 0) (LogicalLoc 0 0)))
              #f)


; board-ref: return the stone at the specified location on the board,
; or indicate it is unoccupied
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref gameState logloc)
  (match logloc
    [(LogicalLoc x y)
     (vector-ref (vector-ref (Go-board gameState) y) x)]))

(check-expect (board-ref testGo (LogicalLoc 0 0)) 'None)
(check-expect (board-ref testGo (LogicalLoc 1 1)) (Some 'white))
(check-expect (board-ref testGo (LogicalLoc 2 2)) (Some 'black))
(check-expect (board-ref testGo (LogicalLoc 3 3)) 'None)


; bd-ref: same thing as board-ref but takes a board rather than a Go
(: bd-ref  : Board LogicalLoc -> (Optional Stone))
(define (bd-ref b logloc)
  (match logloc
    [(LogicalLoc x y)
     (vector-ref (vector-ref b y) x)]))

(check-expect (bd-ref (Go-board testGo) (LogicalLoc 0 0)) 'None)
(check-expect (bd-ref (Go-board testGo) (LogicalLoc 1 1)) (Some 'white))
(check-expect (bd-ref (Go-board testGo) (LogicalLoc 2 2)) (Some 'black))
(check-expect (bd-ref (Go-board testGo) (LogicalLoc 3 3)) 'None)
               
; flip: flip to the next player
; the next functions were taken from the reference code because I believed
; they are more efficient than the previous implementation I had for put-stone-at
(: flip : Stone -> Stone)
(define (flip s)
  (match s
    ['white 'black]
    ['black 'white]))

(check-expect (flip 'white) 'black)
(check-expect (flip 'black) 'white)


; board-set!: modifies a board to put a given stone at the location
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! gameState logloc stone)
  (match logloc
    [(LogicalLoc x y)
     (vector-set!
      (vector-ref (Go-board gameState) y)
      x
      (match stone
        ['None 'None]
        [(Some c) (Some c)]))]))

(board-set! testGo (LogicalLoc 2 3) (Some 'black))
(board-set! testGo (LogicalLoc 1 2) (Some 'white))
(board-set! testGo (LogicalLoc 0 3) 'None)
(check-expect (board-ref testGo (LogicalLoc 2 3)) (Some 'black))
(check-expect (board-ref testGo (LogicalLoc 1 2)) (Some 'white))
(check-expect (board-ref testGo (LogicalLoc 0 3)) 'None)

; define a test board for board-set! function
(: testboard3 : Go)
(define testboard3 (gameBeginning 4))
(board-set! testboard3 (LogicalLoc 3 3) (Some 'black))
(check-expect (board-ref testboard3 (LogicalLoc 3 3)) (Some 'black))
(check-expect (board-ref testboard3 (LogicalLoc 3 0)) 'None)

; bd-set!: modifies a board to put a given stone at the location
; but with a board rather than Go
(: bd-set! : Board LogicalLoc (Optional Stone) -> Void)
(define (bd-set! bd logloc stone)
  (match logloc
    [(LogicalLoc x y)
     (vector-set!
      (vector-ref bd y)
      x
      (match stone
        ['None 'None]
        [(Some c) (Some c)]))]))

(bd-set! (Go-board testGo) (LogicalLoc 0 1) (Some 'black))
(bd-set! (Go-board testGo) (LogicalLoc 1 0) (Some 'white))
(bd-set! (Go-board testGo) (LogicalLoc 2 0) 'None)
(check-expect (board-ref testGo (LogicalLoc 0 1)) (Some 'black))
(check-expect (board-ref testGo (LogicalLoc 1 0)) (Some 'white))
(check-expect (board-ref testGo (LogicalLoc 2 0)) 'None)

; vector-copy1: copies a vector of 1 dimension (row) of the board
; to create a new vector
(: vector-copy1 : (Vectorof (Optional Stone)) -> (Vectorof (Optional Stone)))
(define (vector-copy1 v)
  (if (= (vector-length v) 0)
      v
      (local
        {(define len (vector-length v))
         (: new-vec : (Vectorof (Optional Stone)))
         (define new-vec (make-vector len 'None))
         (: lp : Integer -> (Vectorof (Optional Stone)))
         (define (lp i)
           (cond
             [(= i len) new-vec]
             [else
              (begin
                (vector-set! new-vec i (vector-ref v i))
                (lp (+ i 1)))]))}
        (lp 0))))

(check-expect (vector-copy1 (vector 'None 'None (Some 'white) (Some 'black)))
              (vector 'None 'None (Some 'white) (Some 'black)))
(check-expect (vector-copy1
               (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black)))
              (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black)))

; Below is a demonstration of the vector copy mechanics of vector-copy1
; vector-set! of v1 will ONLY affect v1, and not v2.
(: v1 : (Vectorof (Optional Stone)))
(define v1
  (vector 'None (Some 'white) 'None 'None))
(display "v1= ")
(display v1)
(display "\n")

(: v2 : (Vectorof (Optional Stone)))
(define v2
  (vector-copy1 v1))
(display "v2= ")
(display v2)
(display "\n")

(vector-set! v1 0 (Some 'black))

(display "changed v1= ")
(display v1)
(display "\n")
(display "unchanged v2= ")
(display v2)
(display "\n")

; vector-copy2: makes a copy of a Board through a second dimension vector copy
(: vector-copy2 : (Vectorof (Vectorof (Optional Stone)))
   -> (Vectorof (Vectorof (Optional Stone))))
(define (vector-copy2 v)
  (if (= (vector-length v) 0)
      v
      (local
        {(define len (vector-length v))
         (: new-vec2 : (Vectorof (Optional Stone)))
         (define new-vec2 (make-vector len 'None))
         (: new-vec : (Vectorof (Vectorof (Optional Stone))))
         (define new-vec (make-vector len new-vec2))
         (: lp : Integer -> (Vectorof (Vectorof (Optional Stone))))
         (define (lp i)
           (cond
             [(= i len) new-vec]
             [else
              (begin
                (vector-set! new-vec i (vector-copy1 (vector-ref v i)))
                (lp (+ i 1)))]))}
        (lp 0))))

; board-copy: makes a full copy of the given Board, where the values in b2
; can be changed without affecting b1
(: board-copy : Board -> Board)
(define (board-copy b1)
  (vector-copy2 b1))

(check-expect (board-copy (Go-board testGo)) (Go-board testGo))
(check-expect (board-copy (vector (vector 'None (Some 'white) 'None 'None)
                                  (vector 'None 'None (Some 'black) 'None)
                                  (vector (Some 'white) 'None 'None 'None)
                                  (vector 'None 'None 'None (Some 'black))))
              (vector (vector 'None (Some 'white) 'None 'None)
                      (vector 'None 'None (Some 'black) 'None)
                      (vector (Some 'white) 'None 'None 'None)
                      (vector 'None 'None 'None (Some 'black))))

; Below are the tests for the board-copy function.
; board1 is copied into board2. board1 is then changed,
; but board2 remains unchanged.
(: board1 Board)
(define board1
  (empty-board 4))
(display "board1= ")
(display board1)
(display "\n")

(: board2 Board)
(define board2
  (board-copy board1))
(display "board2= ")
(display board2)
(display "\n")

(bd-set! board1 (LogicalLoc 0 0) (Some 'black))

(display "changed board1= ")
(display board1)
(display "\n")
(display "unchanged board2= ")
(display board2)
(display "\n")

#|
; put-stone-at:
; Return (Some go+), where go+ includes the new stone, if possible.
; Return 'None if location is already occupied.
; Raise an error if it's not the turn to place that stone.
(: put-stone-at : LogicalLoc Stone Go -> (Optional Go))
(define (put-stone-at lloc s g)
  (match g
    [(Go board nextplayer history)
     (if (not (symbol=? s nextplayer))
         (error "It is not your turn.")
         (match (board-ref g lloc)
           [(Some _) 'None]
           ['None
            (match s
              ['black
               (begin
                 (append (list (board-copy board)) history)
                 (board-set! g lloc (Some 'black))
                 (Some (Go board (flip nextplayer) history)))]
              ['white
               (begin
                 (append (list (board-copy board)) history)
                 (board-set! g lloc (Some 'white))
                 (Some (Go board (flip nextplayer) history)))])]))]))
|# 

; board=?1: returns true if every element in a 1 dimensional vector is the same
(: board=?1 : (Vectorof (Optional Stone)) (Vectorof (Optional Stone)) -> Boolean)
(define (board=?1 vec1 vec2)
  (local
    {(define len1 (vector-length vec1))
     (define len2 (vector-length vec2))
     (: lp : Integer -> Boolean)
     (define (lp i)
       (local
         {(: content1 : (Optional Stone))
          (define content1 (vector-ref vec1 i))
          (: content2 : (Optional Stone))
          (define content2 (vector-ref vec2 i))}
         (cond
           [(not (= len1 len2)) false]
           [(and (= len1 0) (= len2 0)) true]
           [else
            (match* (content1 content2)
              [('None 'None) (cond
                               [(= i (sub1 len1)) true]
                               [else (lp (+ i 1))])]
              [('None (Some _)) false]
              [((Some _) 'None) false]
              [((Some c1) (Some c2))
               (cond
                 [(and (= i (sub1 len1)) (symbol=? c1 c2)) true]
                 [(symbol=? c1 c2) (lp (+ i 1))]
                 [else false])])])))}
    (lp 0)))

(check-expect (board=?1 (vector 'None 'None (Some 'white) (Some 'black))
                        (vector 'None 'None (Some 'white) (Some 'black))) true)
(check-expect (board=?1 (vector 'None 'None (Some 'white) (Some 'black))
                        (vector 'None (Some 'white) 'None (Some 'black))) false)

; board=?2: returns true if every element in the Board is the same
(: board=?2 : Board Board -> Boolean)
(define (board=?2 b1 b2)
  (local
    {(define len1 (vector-length b1))
     (define len2 (vector-length b2))
     (: lp : Integer -> Boolean)
     (define (lp i)
       (local
         {(: r1 : (Vectorof (Optional Stone)))
          (define r1 (vector-ref b1 i))
          (: r2 : (Vectorof (Optional Stone)))
          (define r2 (vector-ref b2 i))}
         (cond
           [(not (= len1 len2)) false]
           [(and (= len1 0) (= len2 0)) true]
           [else
            (cond
              [(and (board=?1 r1 r2) (= i (sub1 len1))) true]
              [(board=?1 r1 r2) (lp (+ i 1))]
              [else false])])))}
    (lp 0)))

(check-expect (board=?2 (Go-board (gameBeginning 19))
                        (Go-board (gameBeginning 19))) true)
(check-expect (board=?2 (Go-board testGo) (Go-board testGo)) true)
(check-expect (board=?2 (Go-board testboard3) (Go-board testboard3)) true)
(check-expect (board=?2 (Go-board testGo) (Go-board testboard3)) false)

; valid-board-spec?: given a BoardSpec, determines if it is valid
; all positive, stone radius is less than half of the cell size
; and margin exceeds the stone radius
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? bspec)
  (local
    {(: cs Integer)
     (define cs (BoardSpec-cell-size-pixels bspec))
     (: mp Integer)
     (define mp (BoardSpec-margin-pixels bspec))
     (: srp Integer)
     (define srp (BoardSpec-stone-radius-pixels bspec))}
    (cond
      [(or (negative? cs) (negative? mp) (negative? srp))
       false]
      [(>= srp (/ cs 2)) false]
      [(<= mp srp) false]
      [else true])))

(check-expect (valid-board-spec? (BoardSpec 'tan 10 11 5)) false)
(check-expect (valid-board-spec? (BoardSpec 'tan 25 10 10)) false)
(check-expect (valid-board-spec? (BoardSpec 'tan 22 50 11)) false)
(check-expect (valid-board-spec? (BoardSpec 'tan 10 12 3)) true)

; int->text-size: makes a byte from cell or margin size, or uses 255 if too big
(: int->text-size : Integer -> Byte)
(define (int->text-size n)
  (cond
    [((make-predicate Byte) n) n]
    [else 255]))

(check-expect (int->text-size 50) 50)
(check-expect (int->text-size 300) 255)

; takeN: take n items from front of list
; if there aren't enough items, take as many as possible
(: takeN : All (a) Integer (Listof a) -> (Listof a))
(define (takeN n xs)
  (cond
    [(empty? xs) '()]
    [(zero? n) '()]
    [else (cons (first xs)
                (takeN (- n 1) (rest xs)))]))

(check-expect (takeN 3 '(1 2 3 4 5)) '(1 2 3))
(check-expect (takeN 6 '(1 2 3)) '(1 2 3))

; dropn: drop n items from front of list
(: dropn : All (a) Integer (Listof a) -> (Listof a))
(define (dropn n xs)
  (cond
    [(empty? xs) '()]
    [(zero? n) xs]
    [else (dropn (- n 1) (rest xs))]))

(check-expect (dropn 2 '(1 2 3 4 5 6)) '(3 4 5 6))
(check-expect (dropn 6 '(1 2)) '())

; list-squares: cut list of squares into specified size
(: list-squares : All (a) Integer (Listof a) -> (Listof (Listof a)))
(define (list-squares size xs)
  (match xs
    ['() '()]
    [_ (cons (takeN size xs)
             (list-squares size (dropn size xs)))]))

(check-expect (list-squares 3 '(1 2 3 4 5 6 7 8 9 10 11 12))
              '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(check-expect (list-squares 2 '(1 2 3 4 5 6 7 8 9 10 11 12 13))
              '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12) (13)))

; draw-squares: creates a square for each cell needed on board
(: draw-squares : Integer BoardSpec -> (Listof Image))
(define (draw-squares dimension bspec)
  (local
    {(: draw-a-box : Integer -> Image)
     (define (draw-a-box n)
       (square (BoardSpec-cell-size-pixels bspec) 'outline 'black))}
    (map draw-a-box (build-list (* dimension dimension) add1))))

; eyeball tests for draw-squares
; should be a list of 9 small squares
(draw-squares 3 (BoardSpec 'tan 10 12 3))
(check-expect (length (draw-squares 10 (BoardSpec 'tan 10 12 3))) 100)

; draw-lines: draw lines for board by putting boxes beside/above each other
; taken from reference code
(: draw-lines : Integer BoardSpec -> Image)
(define (draw-lines dimension bspec)
  (foldr (lambda ([i1 : Image] [i2 : Image]) (above/align "left" i1 i2))
         empty-image
         (map (lambda ([ds : (Listof Image)]) (foldr beside empty-image ds))
              (list-squares (sub1 dimension)
                            (draw-squares (sub1 dimension) bspec)))))

; eyeball tests for draw-lines function
; should be grid of 3x3
(draw-lines 4 (BoardSpec 'tan 10 12 3))
; should be grid of 18x18
(draw-lines 19 (BoardSpec 'tan 10 12 3))

; makeBoard: creates the Go board image, based on BoardSpec components
(: makeBoard : Go BoardSpec -> Image)
(define (makeBoard game bspec)
  (local
    {(define len (vector-length (Go-board game)))}
    (overlay
     (draw-lines len bspec)
     (square (+ (* (- len 1) (BoardSpec-cell-size-pixels bspec))
                (* (BoardSpec-margin-pixels bspec) 2))
             "solid"
             (BoardSpec-background-color bspec)))))

; eyeball test for makeBoard
; should be a board with 19 intersections
(makeBoard (gameBeginning 19) (BoardSpec 'tan 20 24 6))


; draw column labels to go along the bottom of the board
(: draw-labelsX : World -> Image)
(define (draw-labelsX w)
  (match w
    [(World (BoardSpec _ cell margin _) (Go board _ _ _ _ _ _) _ _ _ _)
     (foldr (lambda ([column-label : String] [img : Image])
              (beside (overlay (text column-label
                                     (int->text-size (- cell 2)) 'black)
                               (square cell 'outline 'white))
                      img))
            empty-image
            (build-list (vector-length board) column->string))]))

; eyeball test for draw-labelsX
(draw-labelsX (World (BoardSpec 'tan 10 12 4)
                     (gameBeginning 19) "" 100 110 (Some (LogicalLoc 1 2))))
(draw-labelsX (World (BoardSpec 'tan 20 24 9)
                     (gameBeginning 8) "" 100 110 (Some (LogicalLoc 1 2))))
(draw-labelsX (World (BoardSpec 'tan 15 20 7)
                     (gameBeginning 4) "" 100 110 (Some (LogicalLoc 1 2))))

; draw-labelsY : draws the labels for the grid to the side of the board
(: draw-labelsY : Integer BoardSpec (Listof Integer) Image -> Image)
(define (draw-labelsY ycoord bspec labels bground)
  (match labels
    ['() bground]
    [(cons h t)
     (place-image
      (text (number->string h) (int->text-size
                                (quotient (BoardSpec-cell-size-pixels bspec) 2))
            'black)
      (/ (BoardSpec-margin-pixels bspec) 2) ycoord
      (draw-labelsY (- ycoord (BoardSpec-cell-size-pixels bspec))
                    bspec
                    t
                    bground))]))

; eyeball test for draw-labelsY function
; should be a list of 1 to 19 listed vertically on a rectangle
(draw-labelsY 606 (BoardSpec 'tan 30 36 9) (build-list 19 add1)
              (rectangle 36 642 'solid 'white))

; passGo: flip the next player, and leave all else about game the same
; the next few helper functions for draw-world will be taken from the
; reference code for increased clarity
(: passGo : Go -> Go)
(define (passGo g)
  (match g
    [(Go board next history ltp ltop ltsc cp)
     (Go board (flip next) history 'None '() '() (+ 1 cp))]))

(check-expect (passGo testGo)
              (Go (Go-board testGo) 'white (Go-history testGo) 'None
                  '() '() 1))

; next-player: read the next player out of the go struct
(: next-player : World -> Stone)
(define (next-player w)
  (match w
    [(World _ (Go _ next _ _ _ _ _) _ _ _ _) next]))

(check-expect
 (next-player (World (BoardSpec 'tan 10 12 4)
                     testGo "message" 100 200 (Some (LogicalLoc 2 2)))) 'black)

; passWorld: this applies pass to the go struct within the world and
; changes the status message
(: passWorld : World -> World)
(define (passWorld w)
  (World (World-spec w)
         (passGo (World-game w))
         (string-append (symbol->string (next-player w)) " passed.")
         (World-black-tenths w)
         (World-white-tenths w)
         (World-hover w)))

(check-expect (passWorld (World (BoardSpec 'tan 10 12 4)
                                testGo
                                "black's turn to move."
                                100
                                200
                                (Some (LogicalLoc 3 1))))
              (World (BoardSpec 'tan 10 12 4)
                     (Go (Go-board testGo) 'white (Go-history testGo)
                         'None '() '() 1)
                     "black passed."
                     100
                     200
                     (Some (LogicalLoc 3 1))))

; board->string1: 1 dimensional board to string
(: board->string1 : (Vectorof (Optional Stone)) -> String)
(define (board->string1 row)
  (local
    {(define len (vector-length row))
     (: lp : Integer -> String)
     (define (lp i)
       (cond
         [(= i len) ""]
         [else
          (match (vector-ref row i)
            ['None (string-append "_" (lp (+ i 1)))]
            [(Some c)
             (if (symbol=? c 'black) (string-append "*" (lp (+ i 1)))
                 (string-append "o" (lp (+ i 1))))])]))}
     (lp 0)))

; board->string2: 2 dimentional board to string
(: board->string2 : Board -> String)
(define (board->string2 board)
  (local
    {(define len (vector-length board))
     (: lp : Integer -> String)
     (define (lp i)
       (cond
         [(= i len) ""]
         [else
          (string-append (board->string1 (vector-ref board i))
                         (if (= i (sub1 len)) "" "|")
                         (lp (+ i 1)))]))}
    (lp 0)))

(check-expect (board->string2
               (vector (vector (Some 'white) (Some 'white) (Some 'black) 'None)
                       (vector (Some 'white) (Some 'white) (Some 'black) 'None)
                       (vector (Some 'black) (Some 'white) (Some 'black) 'None)
                       (vector 'None (Some 'black) (Some 'black) 'None)))
              "oo*_|oo*_|*o*_|_**_")

; history->string: turns a board history into a string
(: history->string : (Listof Board) -> String)
(define (history->string boards)
  (match boards
    ['() ""]
    [(cons h '()) (board->string2 h)]
    [(cons h t)
     (string-append (board->string2 h)
                    "!"
                    (history->string t))]))

(check-expect
 (history->string (Go-history testGo))
 "__o*|*o__|_o*_|*___!__o_|*o__|_o*_|*___!__o_|*___|_o*_|*___!__o_|*___|_o__|*___")

; go->string: turns a go struct into a string
(: go->string : Go -> String)
(define (go->string game)
  (match game
    [(Go board next history ltp ltoc ltsc cp)
     (string-append
      (match next ['black "*"] ['white "o"])
      "~"
      (board->string2 board)
      "~"
      (history->string history)
      "~"
      (number->string cp))]))

(check-expect
 (go->string testGo)
 (string-append "*~_o_*|*o__|_o*_|__*_~__o*|*o__|_o*_|*___!__o_|*o__|_o*_|"
                "*___!__o_|*___|_o*_|*___!__o_|*___|_o__|*___~0"))

; world->string: transforms a world into a string
(: world->string : World -> String)
(define (world->string w)
  (match w
    [(World bspec game stat btime wtime hoverloc)
     (string-append
      (number->string btime)
      "@"
      (number->string wtime)
      "@"
      (go->string game))]))

(check-expect
 (world->string
 (World
  (BoardSpec 'tan 30 40 14)
  testGo
  "Test Status Message"
  98765
  56789
  (Some (LogicalLoc 0 0))))
 (string-append
  "98765@56789@*~_o_*|*o__|_o*_|__*_~__o*|*o__|_o*_|*___!__o_|*o__|_o"
  "*_|*___!__o_|*___|_o*_|*___!__o_|*___|_o__|*___~0"))

; string->row: converts a string into one row of the board
(: string->row : String -> (Vectorof (Optional Stone)))
(define (string->row s)
  (local
    {(: charlist : (Listof Char))
     (define charlist (string->list s))
     (: vec : (Vectorof (Optional Stone)))
     (define vec (make-vector (length charlist) 'None))
     (define len (vector-length vec))
     (: lp : Integer (Listof Char) -> (Vectorof (Optional Stone)))
     (define (lp i l)
       (match l
         ['() vec]
         [(cons h t)
          (if (= i len) vec
              (begin
                (vector-set! vec i
                             (match h
                               [#\o (Some 'white)]
                               [#\* (Some 'black)]
                               [#\_ 'None]))
                (lp (+ i 1) t)))]))}
    (lp 0 charlist)))

(check-expect (string->row "o__") (vector (Some 'white) 'None 'None))
  
; string->board: takes the board portion of the string and makes it a board
(: string->board : String -> Board)
(define (string->board s)
  (local
    {(: strlist : (Listof String))
     (define strlist (string-split s "|"))
     (: vec : (Vectorof (Vectorof (Optional Stone))))
     (define vec (empty-board (length strlist)))
     (define len (vector-length vec))
     (: lp : Integer (Listof String) -> Board)
     (define (lp i l)
       (match l
         ['() vec]
         [(cons h t)
          (if (= i len) vec
              (begin
                (vector-set! vec i (string->row h))
                (lp (+ i 1) t)))]))}
    (lp 0 strlist)))

(check-expect
 (string->board "oo*_|oo*_|*o*_|_**_")
 (vector
 (vector (Some 'white) (Some 'white) (Some 'black) 'None)
 (vector (Some 'white) (Some 'white) (Some 'black) 'None)
 (vector (Some 'black) (Some 'white) (Some 'black) 'None)
 (vector 'None (Some 'black) (Some 'black) 'None)))

; string->history: given a string, read it and create the history of the game
(: string->history : String -> (Listof Board))
(define (string->history s)
  (local
    {(: strlist : (Listof String))
     (define strlist (string-split s "!"))
     (: lp : (Listof String) -> (Listof Board))
     (define (lp l)
       (match l
         ['() '()]
         [(cons h t)
          (cons (string->board h) (lp t))]))}
    (lp strlist)))

(check-expect
 (string->history
  "__o*|*o__|_o*_|*___!__o_|*o__|_o*_|*___!__o_|*___|_o*_|*___!__o_|*___|_o__|*___")
 (Go-history testGo))

;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

; string->go: takes a string and makes a Go Struct
(: string->go : String -> Go)
(define (string->go s)
  (local
    {(: strlist : (Listof String))
     (define strlist (string-split s "~"))}
    (Go
     (string->board (second strlist))
     (match (first strlist)
       ["*" 'black]
       ["o" 'white])
     (string->history (third strlist))
     'None
     '()
     '()
     (string->integer (fourth strlist)))))

(check-expect
 (string->go (string-append
              "*~_o_*|*o__|_o*_|__*_~__o*|*o__|_o*_|*___!__o_|*o__|_o*_|"
              "*___!__o_|*___|_o*_|*___!__o_|*___|_o__|*___~0"))
 (Go
  (vector
   (vector 'None (Some 'white) 'None (Some 'black))
   (vector (Some 'black) (Some 'white) 'None 'None)
   (vector 'None (Some 'white) (Some 'black) 'None)
   (vector 'None 'None (Some 'black) 'None))
  'black
  (list
   (vector
    (vector 'None 'None (Some 'white) (Some 'black))
    (vector (Some 'black) (Some 'white) 'None 'None)
    (vector 'None (Some 'white) (Some 'black) 'None)
    (vector (Some 'black) 'None 'None 'None))
   (vector
    (vector 'None 'None (Some 'white) 'None)
    (vector (Some 'black) (Some 'white) 'None 'None)
    (vector 'None (Some 'white) (Some 'black) 'None)
    (vector (Some 'black) 'None 'None 'None))
   (vector
    (vector 'None 'None (Some 'white) 'None)
    (vector (Some 'black) 'None 'None 'None)
    (vector 'None (Some 'white) (Some 'black) 'None)
    (vector (Some 'black) 'None 'None 'None))
   (vector
    (vector 'None 'None (Some 'white) 'None)
    (vector (Some 'black) 'None 'None 'None)
    (vector 'None (Some 'white) 'None 'None)
    (vector (Some 'black) 'None 'None 'None)))
  'None
  '()
  '()
  0))

(: string->world : BoardSpec String -> World)
(define (string->world bspec s)
  (local
    {(: strlist : (Listof String))
     (define strlist (string-split s "@"))}
    (World
     bspec
     (string->go (third strlist))
     "Welcome back to Go!"
     (string->integer (first strlist))
     (string->integer (second strlist))
     'None)))

(check-expect
 (string->world
  (BoardSpec 'tan 30 40 14)
  (string-append
   "98765@56789@*~_o_*|*o__|_o*_|__*_~__o*|*o__|_o*_|*___!__o_|*o__|_o"
   "*_|*___!__o_|*___|_o*_|*___!__o_|*___|_o__|*___~0"))
 (World
  (BoardSpec 'tan 30 40 14)
  (Go
   (vector
    (vector 'None (Some 'white) 'None (Some 'black))
    (vector (Some 'black) (Some 'white) 'None 'None)
    (vector 'None (Some 'white) (Some 'black) 'None)
    (vector 'None 'None (Some 'black) 'None))
   'black
   (list
    (vector
     (vector 'None 'None (Some 'white) (Some 'black))
     (vector (Some 'black) (Some 'white) 'None 'None)
     (vector 'None (Some 'white) (Some 'black) 'None)
     (vector (Some 'black) 'None 'None 'None))
    (vector
     (vector 'None 'None (Some 'white) 'None)
     (vector (Some 'black) (Some 'white) 'None 'None)
     (vector 'None (Some 'white) (Some 'black) 'None)
     (vector (Some 'black) 'None 'None 'None))
    (vector
     (vector 'None 'None (Some 'white) 'None)
     (vector (Some 'black) 'None 'None 'None)
     (vector 'None (Some 'white) (Some 'black) 'None)
     (vector (Some 'black) 'None 'None 'None))
    (vector
     (vector 'None 'None (Some 'white) 'None)
     (vector (Some 'black) 'None 'None 'None)
     (vector 'None (Some 'white) 'None 'None)
     (vector (Some 'black) 'None 'None 'None)))
   'None
   '()
   '()
   0)
  "Welcome back to Go!"
  98765
  56789
  'None))

#|
     ; function to make strings into vectors
     (: string->vector : String -> (Vectorof Char))
     (define (string->vector str)
       (list->vector (string->list str)))
     ; makes the string in each vector index into another vector with each character
     (: add-vec : (Vectorof String) -> (Vectorof (Vectorof Char)))
     (define (add-vec v)
       (local
         {(define len (vector-length v))
          (: lp : Integer -> (Vectorof (Vectorof Char)))
          (define (lp i)
            (cond
              [(= i (sub1 len)) (string->vector (vector-ref v i))]
              [else (begin
                      (string->vector (vector-ref v i))
                      (lp (+ i 1)))]))}
         (lp 0)))
     ; bvec is the board vector, with a string instead of an Optional Stone
     (: bvec : (Vectorof (Vectorof Char)))
     (define bvec (add-vec blist))
     ; add-stone1
     (: add-stone1 : (Vectorof Char) -> Void)
     (define (add-stone1 vec)
       (local
         {(define len (vector-length vec))
          (: lp : Integer -> Void)
          (define (lp i)
            (cond
              [(= i len) (void)]
              [else (begin
                      (vector-set! vec
                                   i
                                   (match (vector-ref vec i)
                                     [#\o (Some 'white)]
                                     [#\* (Some 'black)]
                                     [#\_ 'None]))
                      (lp (+ i 1)))]))}
         (lp 0)))
     (: add-stone2 : (Vectorof (Vectorof String)) -> Void)
     (define (add-stone2 v)
       (local
         {(define len (vector-length v))
          (: lp : Integer -> Void)
          (define (lp i)
            (cond
              [(= i len) (void)]
              [else
               (add-stone1 (vector-ref v i))]))}
         (lp 0)))}
    (add-stone2 bvec)))
|#

; produce the message panel that will appear at the bottom of the display
(: message-panel : World -> Image)
(define (message-panel w)
  (match w
    [(World (BoardSpec bg cell margin _) (Go board next _ _ _ _ cp) stat _ _ _)
     (local
       {(define len (vector-length board))
        (define panel : Image
          (rectangle (+ (* cell (sub1 len)) (* 2 margin)) (* margin 2) 'solid bg))}
       (overlay (text
                 (if (= cp 2) (outcome->string (outcome (World-game w))) stat)
                 (int->text-size
                  (exact-floor (/ margin 2))) 'black) panel))]))

(message-panel (World (BoardSpec 'tan 30 20 9)
                      (gameBeginning 19)
                      "Welcome to Go!"
                      20
                      50
                      'None))

; liberties?: determines if a LogicalLoc has any liberties given a Board
(: liberties? : Board LogicalLoc -> Boolean)
(define (liberties? board lloc)
  (match (bd-ref board lloc)
    ['None true]
    [(Some _)
     (match lloc
       [(LogicalLoc x y)
        (local
          {(define len (sub1 (vector-length board)))
           (: upneighbor : (Optional Stone))
           (define upneighbor (bd-ref board
                                      (LogicalLoc x (if (= y len) len (add1 y)))))
           (: downneighbor : (Optional Stone))
           (define downneighbor (bd-ref board
                                        (LogicalLoc x (if (= y 0) 0 (sub1 y)))))
           (: leftneighbor : (Optional Stone))
           (define leftneighbor (bd-ref board
                                        (LogicalLoc (if (= x 0) 0 (sub1 x)) y)))
           (: rightneighbor : (Optional Stone))
           (define rightneighbor (bd-ref
                                  board
                                  (LogicalLoc (if (= x len) len (add1 x)) y)))}
          (cond
            [(and (= x 0) (= y 0))
             (match* (upneighbor rightneighbor)
               [((Some _) (Some _)) false]
               [(_ _) true])]
            [(and (= x 0) (= y len))
             (match* (downneighbor rightneighbor)
               [((Some _) (Some _)) false]
               [(_ _) true])]
            [(and (= x len) (= y 0))
             (match* (upneighbor leftneighbor)
               [((Some _) (Some _)) false]
               [(_ _) true])]
            [(and (= x len) (= y len))
             (match* (downneighbor leftneighbor)
               [((Some _) (Some _)) false]
               [(_ _) true])]
            [(= x 0)
             (match* (upneighbor downneighbor rightneighbor)
               [((Some _) (Some _) (Some _)) false]
               [(_ _ _) true])]
            [(= x len)
             (match* (upneighbor downneighbor leftneighbor)
               [((Some _) (Some _) (Some _)) false]
               [(_ _ _) true])]
            [(= y 0)
             (match* (upneighbor rightneighbor leftneighbor)
               [((Some _) (Some _) (Some _)) false]
               [(_ _ _) true])]
            [(= y len)
             (match* (downneighbor rightneighbor leftneighbor)
               [((Some _) (Some _) (Some _)) false]
               [(_ _ _) true])]
            [else
             (match* (upneighbor downneighbor leftneighbor rightneighbor)
               [((Some _) (Some _) (Some _) (Some _)) false]
               [(_ _ _ _) true])]))])]))

(check-expect (liberties? (vector (vector (Some 'black) (Some 'white) 'None 'None)
                                  (vector (Some 'black) (Some 'white) 'None 'None)
                                  (vector 'None 'None 'None 'None))
                          (LogicalLoc 0 0)) false)
(check-expect (liberties? (vector (vector (Some 'black) (Some 'white) 'None 'None)
                                  (vector 'None (Some 'white) 'None 'None)
                                  (vector 'None 'None 'None 'None))
                          (LogicalLoc 0 0)) true)
(check-expect (liberties? (vector
                           (vector (Some 'black) (Some 'white) 'None 'None)
                           (vector (Some 'black) (Some 'white) (Some 'black) 'None)
                           (vector 'None (Some 'white) 'None 'None))
                          (LogicalLoc 1 1)) false)
(check-expect (liberties? (vector
                           (vector (Some 'black) 'None 'None 'None)
                           (vector (Some 'black) (Some 'white) (Some 'black) 'None)
                           (vector 'None (Some 'white) 'None 'None))
                          (LogicalLoc 1 1)) true)
(check-expect (liberties? (vector
                           (vector (Some 'black) 'None 'None 'None)
                           (vector (Some 'black) (Some 'white) (Some 'black) 'None)
                           (vector 'None (Some 'white) 'None 'None))
                          (LogicalLoc 0 2)) true)

; adj-loc: returns the adjacent locations to a LogicalLoc
(: adj-loc : Board LogicalLoc -> (Listof LogicalLoc))
(define (adj-loc board lloc)
  (match lloc
    [(LogicalLoc x y)
     (local
       {(define len (sub1 (vector-length board)))
        (: upneighbor : LogicalLoc)
        (define upneighbor (LogicalLoc x (if (= y len) len (add1 y))))
        (: downneighbor : LogicalLoc)
        (define downneighbor (LogicalLoc x (if (= y 0) 0 (sub1 y))))
        (: leftneighbor : LogicalLoc)
        (define leftneighbor (LogicalLoc (if (= x 0) 0 (sub1 x)) y))
        (: rightneighbor : LogicalLoc)
        (define rightneighbor (LogicalLoc (if (= x len) len (add1 x)) y))}
       (cond
         [(and (= x 0) (= y 0))
          (list upneighbor rightneighbor)]
         [(and (= x 0) (= y len))
          (list downneighbor rightneighbor)]
         [(and (= x len) (= y 0))
          (list upneighbor leftneighbor)]
         [(and (= x len) (= y len))
          (list downneighbor leftneighbor)]
         [(= x 0)
          (list upneighbor downneighbor rightneighbor)]
         [(= x len)
          (list upneighbor downneighbor leftneighbor)]
         [(= y 0)
          (list upneighbor rightneighbor leftneighbor)]
         [(= y len)
          (list downneighbor rightneighbor leftneighbor)]
         [else
          (list upneighbor downneighbor rightneighbor leftneighbor)]))]))

(check-expect (adj-loc (vector (vector (Some 'black) (Some 'white) 'None 'None)
                               (vector (Some 'black) (Some 'white) 'None 'None)
                               (vector 'None 'None 'None 'None))
                       (LogicalLoc 1 1))
              (list (LogicalLoc 1 2) (LogicalLoc 1 0)
                    (LogicalLoc 2 1) (LogicalLoc 0 1)))
(check-expect (adj-loc (vector (vector (Some 'black) (Some 'white) 'None)
                               (vector (Some 'black) (Some 'white) 'None)
                               (vector 'None 'None 'None 'None))
                       (LogicalLoc 0 0))
              (list (LogicalLoc 0 1) (LogicalLoc 1 0)))
(check-expect (adj-loc (vector (vector (Some 'black) (Some 'white) 'None)
                               (vector (Some 'black) (Some 'white) 'None)
                               (vector 'None 'None 'None))
                       (LogicalLoc 2 2))
              (list (LogicalLoc 2 1) (LogicalLoc 1 2)))
(check-expect (adj-loc (vector (vector (Some 'black) (Some 'white) 'None)
                               (vector (Some 'black) (Some 'white) 'None)
                               (vector 'None 'None 'None))
                       (LogicalLoc 2 1))
              (list (LogicalLoc 2 2) (LogicalLoc 2 0)
                    (LogicalLoc 1 1)))

; same-color?: returns true if 2 logical loc are the same color stone
(: same-color? : Board LogicalLoc LogicalLoc -> Boolean)
(define (same-color? board l1 l2)
  (local
    {(: stone1 : (Optional Stone))
     (define stone1 (bd-ref board l1))
     (: stone2 : (Optional Stone))
     (define stone2 (bd-ref board l2))}
    (match* (stone1 stone2)
      [('None 'None) false]
      [('None (Some _)) false]
      [((Some _) 'None) false]
      [((Some c1) (Some c2)) (symbol=? c1 c2)])))

(check-expect (same-color?
               (vector (vector (Some 'black) (Some 'white) 'None 'None)
                       (vector (Some 'black) (Some 'white) 'None 'None)
                       (vector 'None 'None 'None 'None))
               (LogicalLoc 0 0)
               (LogicalLoc 0 1)) true)
(check-expect (same-color?
               (vector (vector (Some 'black) (Some 'white) 'None 'None)
                       (vector (Some 'black) (Some 'white) 'None 'None)
                       (vector 'None 'None 'None 'None))
               (LogicalLoc 1 0)
               (LogicalLoc 0 1)) false)


; unmarked-neighbors: returns the unmarked neighbors of the same color
; of a given LogicalLoc
(: unmarked-neighbors : Board LogicalLoc (Listof LogicalLoc)
   -> (Listof LogicalLoc))
(define (unmarked-neighbors board lloc marked)
  (local
    {; remove-marked: removes a marked logicalloc from the list of neighbors
     (: remove-marked : LogicalLoc (Listof LogicalLoc)
        -> (Listof LogicalLoc))
     (define (remove-marked markedloc tobefiltered)
       (match tobefiltered
         ['() '()]
         [(cons h t)
          (match* (markedloc h)
            [((LogicalLoc x1 y1) (LogicalLoc x2 y2))
             (if (and (= x1 x2) (= y1 y2))
                 (remove-marked markedloc t)
                 (cons h (remove-marked markedloc t)))])]))
     ; remove-color: removes locs with other color or none from the list of neigh
     ; this list of logicalloc in this function is the adj-loc list
     (: remove-color : LogicalLoc (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (remove-color logicalloc neighbors)
       (match neighbors
         ['() '()]
         [(cons h t)
          (if (same-color? board logicalloc h)
              (cons h (remove-color logicalloc t))
              (remove-color logicalloc t))]))
     ; removes all the marked locations from list of neighbors
     (: remove-all : (Listof LogicalLoc) (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (remove-all markedlocs tobefiltered)
       (match markedlocs
         ['() tobefiltered]
         [(cons head tail)
          (remove-marked head (remove-all tail tobefiltered))]))}
    (remove-all marked (remove-color lloc (adj-loc board lloc)))))

(check-expect (unmarked-neighbors
               (vector (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector 'None (Some 'white) 'None 'None)
                       (vector 'None 'None 'None 'None))
               (LogicalLoc 1 1)
               (list (LogicalLoc 1 0) (LogicalLoc 1 2)))
              (list (LogicalLoc 0 1)))
(check-expect (unmarked-neighbors
               (vector (vector (Some 'black) (Some 'white) 'None 'None)
                       (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector 'None (Some 'black) 'None 'None)
                       (vector 'None 'None 'None 'None))
               (LogicalLoc 1 1)
               (list (LogicalLoc 1 0)))
              (list (LogicalLoc 0 1)))
(check-expect (unmarked-neighbors
               (vector (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector 'None (Some 'white) 'None 'None)
                       (vector 'None 'None 'None 'None))
               (LogicalLoc 0 0)
               (list (LogicalLoc 1 0) (LogicalLoc 0 1)))
              '())
(check-expect (unmarked-neighbors
               (vector (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector 'None (Some 'white) 'None 'None)
                       (vector 'None 'None 'None 'None))
               (LogicalLoc 0 1)
               (list (LogicalLoc 1 2) (LogicalLoc 1 1)))
              (list (LogicalLoc 0 0)))
(check-expect (unmarked-neighbors
               (vector (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector (Some 'white) (Some 'white) 'None 'None)
                       (vector 'None (Some 'white) 'None 'None)
                       (vector 'None 'None 'None 'None))
               (LogicalLoc 1 0)
               (list (LogicalLoc 1 1)))
              (list (LogicalLoc 0 0)))

; identify-chain: identifies chains of stones during the capture phase
(: identify-chain : Board (Listof LogicalLoc) (Listof LogicalLoc)
   -> (Optional (Listof LogicalLoc)))
(define (identify-chain board to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons head tail)
     (if (liberties? board head)
         'None
         (identify-chain
          board
          (append tail (unmarked-neighbors board head marked))
          (append marked (unmarked-neighbors board head marked))))]))

(check-expect (identify-chain
               (vector (vector (Some 'white) (Some 'white) (Some 'black) 'None)
                       (vector (Some 'white) (Some 'white) (Some 'black) 'None)
                       (vector (Some 'black) (Some 'white) (Some 'black) 'None)
                       (vector 'None (Some 'black) (Some 'black) 'None))
               (list (LogicalLoc 1 2))
               (list (LogicalLoc 1 2)))
              (Some (list (LogicalLoc 1 2)
                          (LogicalLoc 1 1)
                          (LogicalLoc 1 0)
                          (LogicalLoc 0 1)
                          (LogicalLoc 0 0))))
(check-expect (identify-chain
               (vector (vector (Some 'white) (Some 'white) (Some 'black) 'None)
                       (vector (Some 'white) (Some 'white) (Some 'black) 'None)
                       (vector 'None (Some 'white) (Some 'black) 'None)
                       (vector 'None (Some 'black) (Some 'black) 'None))
               (list (LogicalLoc 1 2))
               (list (LogicalLoc 1 2)))
              'None)
(check-expect
 (identify-chain
  (vector
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None 'None 'None 'None))
  (list (LogicalLoc 1 6))
  (list (LogicalLoc 1 6)))
 'None)
(check-expect
 (identify-chain
  (vector
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None 'None 'None 'None))
  (list (LogicalLoc 0 7))
  (list (LogicalLoc 0 7)))
 'None)
(check-expect
 (identify-chain
  (vector
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None 'None 'None 'None))
  (list (LogicalLoc 0 6))
  (list (LogicalLoc 0 6)))
 'None)
(check-expect
 (identify-chain
  (vector
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None 'None 'None 'None))
  (list (LogicalLoc 1 5))
  (list (LogicalLoc 1 5)))
 'None)
(unmarked-neighbors
 (vector
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
  (vector 'None 'None 'None 'None 'None 'None 'None 'None))
 (LogicalLoc 0 5)
 '())

; unmarked-territory: returns the unmarked neighbors of also 'None
; of a given LogicalLoc
(: unmarked-territory :  Board LogicalLoc (Listof LogicalLoc)
   -> (Listof LogicalLoc))
(define (unmarked-territory board lloc marked)
  (local
    {; remove-marked: removes a marked logicalloc from the list of neighbors
     (: remove-marked : LogicalLoc (Listof LogicalLoc)
        -> (Listof LogicalLoc))
     (define (remove-marked markedloc tobefiltered)
       (match tobefiltered
         ['() '()]
         [(cons h t)
          (match* (markedloc h)
            [((LogicalLoc x1 y1) (LogicalLoc x2 y2))
             (if (and (= x1 x2) (= y1 y2))
                 (remove-marked markedloc t)
                 (cons h (remove-marked markedloc t)))])]))
     ; removes all logicalloc that has a stone from the list of places to explore
     (: remove-stones : (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (remove-stones neighbors)
       (match neighbors
         ['() '()]
         [(cons h t)
          (match (bd-ref board h)
            ['None (cons h (remove-stones t))]
            [(Some _) (remove-stones t)])]))
     ; removes all the marked locations from list of neighbors
     (: remove-all : (Listof LogicalLoc) (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (remove-all markedlocs tobefiltered)
       (match markedlocs
         ['() tobefiltered]
         [(cons head tail)
          (remove-marked head (remove-all tail tobefiltered))]))}
    (remove-all marked (remove-stones (adj-loc board lloc)))))

(check-expect
 (unmarked-territory
  (vector
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None))
  (LogicalLoc 3 0) (list (LogicalLoc 3 0) (LogicalLoc 3 1) (LogicalLoc 4 0)))
 '())

; neighbors?: determines if any of the adj-loc is a stone of other color
(: neighbors? : Stone Board LogicalLoc -> Boolean)
(define (neighbors? stone board lloc)
  (local
    {(: lp : (Listof LogicalLoc) -> Boolean)
     (define (lp llocs)
       (match llocs
         ['() false]
         [(cons head tail)
          (match (bd-ref board head)
            ['None (lp tail)]
            [(Some c) (if (symbol=? c stone)
                          (lp tail)
                          true)])]))}
    (lp (adj-loc board lloc))))

; identify territory: identifies chains of 'None
(: identify-territory : Stone Board (Listof LogicalLoc) (Listof LogicalLoc)
   -> (Optional (Listof LogicalLoc)))
(define (identify-territory stone board to-explore marked)
  (match to-explore
    ['() (Some marked)]
    [(cons head tail)
     (if (neighbors? stone board head)
         'None
         (identify-territory
          stone board
          (append tail (unmarked-territory board head marked))
          (append marked (unmarked-territory board head marked))))]))

(identify-territory
 'black
 (Go-board (gameBeginning 4)) (list (LogicalLoc 0 0)) (list (LogicalLoc 0 0)))
(identify-territory
 'black
 (vector
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None))
 (list (LogicalLoc 3 0)) (list (LogicalLoc 3 0)))

; none-counter: makes a list of the LogicalLoc with 'None
(: none-counter : Board -> (Listof LogicalLoc))
(define (none-counter board)
  (local
    {(: len : Integer)
     (define len (vector-length board))
     (: count : Integer Integer (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (count x y acc)
       (cond
         [(and (= x (sub1 len)) (= y (sub1 len)))
          (match (bd-ref board (LogicalLoc x y))
            ['None (cons (LogicalLoc x y) acc)]
            [(Some _) acc])]
         [(= y (sub1 len))
          (match (bd-ref board (LogicalLoc x y))
            ['None (count (+ x 1) 0 (cons (LogicalLoc x y) acc))]
            [(Some _) (count (+ x 1) 0 acc)])]
         [else
          (match (bd-ref board (LogicalLoc x y))
            ['None (count x (+ y 1) (cons (LogicalLoc x y) acc))]
            [(Some _) (count x (+ y 1) acc)])]))}
    (count 0 0 '())))

(check-expect
(length (none-counter
 (vector
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None)))) 42)

; run identify territory for every single 'None on the board given a stone
(: it-board : Stone Board (Listof LogicalLoc) ->
   (Listof (Optional (Listof LogicalLoc))))
(define (it-board stone board llocs)
  (match llocs
    ['() '()]
    [(cons h t)
     (cons (identify-territory stone board (list h) (list h))
           (it-board stone board t))]))

; territory-points: gives one point for each LogicalLoc in a chain
(: territory-points : Stone Board -> Integer)
(define (territory-points stone board)
  (local
    {(: len : Integer)
     (define len (vector-length board))
     ; listof logicalloc equality function
     (: loflloc=? : (Listof LogicalLoc) (Listof LogicalLoc) -> Boolean)
     (define (loflloc=? l1 l2)
       (match l1
         ['() true]
         [(cons h t)
          (if (and (appearsLL? h l2) (loflloc=? t l2)) true false)]))
     ; remove 'None from a listof chains
     (: remove-none : (Listof (Optional (Listof LogicalLoc))) ->
        (Listof (Listof LogicalLoc)))
     (define (remove-none l)
       (match l
         ['() '()]
         [(cons h t)
          (match h
            ['None (remove-none t)]
            [(Some _) (cons (Some-value h) (remove-none t))])]))
     ; remove duplicate chains from the listof chains
     (: remove-duplicates : (Listof (Listof LogicalLoc)) ->
        (Listof (Listof LogicalLoc)))
     (define (remove-duplicates lofchains)
       (match lofchains
         ['() '()]
         [(cons hd tl)
          (cond
            [(empty? tl) (cons hd '())]
            [else
             (if (loflloc=? hd (first tl))
                 (remove-duplicates tl)
                 (cons hd (remove-duplicates tl)))])]))
          ; appends the all the llocs in the chains together into a big list
     (: appender : (Listof (Listof LogicalLoc)) -> (Listof LogicalLoc))
     (define (appender l)
       (match l
         ['() '()]
         [(cons h t)
          (append h (appender t))]))}
    (length (appender (remove-duplicates
                       (remove-none (it-board stone board
                                              (none-counter board))))))))

(check-expect 
(territory-points
 'black
(vector
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None)))
42)
 
; stone-counter1: given a row in a board,
; count how many stones of a given color in the row
(: stone-counter1 : Stone (Vectorof (Optional Stone)) -> Integer)
(define (stone-counter1 stone row)
  (local
    {(: len : Integer)
     (define len (vector-length row))
     (: count : Integer Integer -> Integer)
     (define (count i acc)
       (cond
         [(= i len) acc]
         [else
          (match (vector-ref row i)
            ['None (count (+ i 1) acc)]
            [(Some c) (if (symbol=? c stone)
                          (count (+ i 1) (+ acc 1))
                          (count (+ i 1) acc))])]))}
     (count 0 0)))

(check-expect (stone-counter1 'black
                              (vector (Some 'black) (Some 'black) (Some 'white) 'None))
              2)
(check-expect (stone-counter1 'white
                              (vector 'None 'None 'None 'None)) 0)

; stone-counter2: given Board, count all stones of a certain color
(: stone-counter2 : Stone Board -> Integer)
(define (stone-counter2 stone board)
  (local
    {(: len : Integer)
     (define len (vector-length board))
     (: count2 : Integer Integer -> Integer)
     (define (count2 i acc)
       (cond
         [(= i len) acc]
         [else
          (count2 (+ i 1) (+ acc (stone-counter1 stone (vector-ref board i))))]))}
    (count2 0 0)))

(check-expect
(stone-counter2
 'black
 (vector
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
  (vector (Some 'black) 'None 'None 'None 'None 'None 'None 'None)))
9)

; outcome: gives the outcome to the completed game
(: outcome : Go -> Outcome)
(define (outcome game)
  (match game
    [(Go board next history ltp ltoc ltsc cp)
     (local
       {(define black
          (+ (stone-counter2 'black board) (territory-points 'black board)))
        (define white
          (+ (stone-counter2 'white board) (territory-points 'white board)))}
     (Outcome
      black
      white
      (cond
        [(> black white) 'black]
        [(< black white) 'white]
        [else 'draw])))]))

(check-expect (outcome testGo) (Outcome 4 3 'black))

; outcome->string: makes the outcome of the game into a string status message
(: outcome->string : Outcome -> String)
(define (outcome->string outcome)
  (string-append
   "Black score: "
   (number->string (Outcome-black outcome))
   "\nWhite score: "
   (number->string (Outcome-white outcome))
   "\nWinner: "
   (symbol->string (Outcome-winner outcome))))

(check-expect (outcome->string
               (Outcome 10 12 'white))
              "Black score: 10\nWhite score: 12\nWinner: white")


; capture: defines a capture, which is identifying the chain,
; setting all captured locations to 'None
(: capture : Board LogicalLoc -> Void)
(define (capture board lloc)
  (local
    {(: remove-same-color : LogicalLoc (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (remove-same-color logicalloc neighbors)
       (match neighbors
         ['() '()]
         [(cons h t)
          (if (same-color? board logicalloc h)
              (remove-same-color logicalloc t)
              (cons h (remove-same-color logicalloc t)))]))
     ; p2explore: list of places to explore take the adj-loc of
     (: p2explore : (Listof LogicalLoc))
     (define p2explore (remove-same-color lloc (adj-loc board lloc)))
     ; taking the adj-locations with same color removed
     (: makelist-of-chains : (Listof LogicalLoc) ->
        (Listof (Optional (Listof LogicalLoc))))
     (define (makelist-of-chains l)
       (match l
         ['() '()]
         [(cons head tail)
          (cons (identify-chain board (list head) (list head))
                (makelist-of-chains tail))]))
     ; list-of-chains is a list of chains of the OPPOSITE color
     (: list-of-chains :  (Listof (Optional (Listof LogicalLoc))))
     (define list-of-chains (makelist-of-chains p2explore))
     ; remover: replaces all spots in chain with 'None
     (: remover : (Optional (Listof LogicalLoc)) -> Void)
     (define (remover chain)
       (match chain
         ['None (void)]
         [(Some '()) (void)]
         [(Some (cons head tail))
          (begin
            (bd-set! board head 'None)
            (remover (Some tail)))]))
     ; full-capture: runs remover for each chain in list-of-chains
     (: full-capture : (Listof (Optional (Listof LogicalLoc))) -> Void)
     (define (full-capture chainlist)
       (match chainlist
         ['() (void)]
         [(cons head tail)
          (begin
            (remover head)
            (full-capture tail))]))}
    (full-capture list-of-chains)))

; checks for capture function. Should remove all white stones from the board that
; are in the chain
(: boardwithwhitechain : Board)
(define boardwithwhitechain
  (vector
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'white) (Some 'black) 'None 'None 'None 'None 'None)
   (vector (Some 'white) (Some 'black) (Some 'black) 'None 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None 'None 'None 'None)))
(capture boardwithwhitechain (LogicalLoc 1 6))
(display boardwithwhitechain)
(display "\n")
(: boardwithwhitechain2 : Board)
(define boardwithwhitechain2
  (vector
   (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
   (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None)))
(capture boardwithwhitechain2 (LogicalLoc 3 2))
(display boardwithwhitechain2)
(display "\n")
(display "\n")
(display "\n")

; list-lloc-captured: makes a list of all the llocs captured by a move
(: list-lloc-captured : Board LogicalLoc -> (Listof LogicalLoc))
(define (list-lloc-captured board lloc)
  (local
    {(: remove-same-color : LogicalLoc (Listof LogicalLoc) -> (Listof LogicalLoc))
     (define (remove-same-color logicalloc neighbors)
       (match neighbors
         ['() '()]
         [(cons h t)
          (if (same-color? board logicalloc h)
              (remove-same-color logicalloc t)
              (cons h (remove-same-color logicalloc t)))]))
     ; p2explore: list of places to explore take the adj-loc of
     (: p2explore : (Listof LogicalLoc))
     (define p2explore (remove-same-color lloc (adj-loc board lloc)))
     ; taking the adj-locations with same color removed
     (: makelist-of-chains : (Listof LogicalLoc) ->
        (Listof (Optional (Listof LogicalLoc))))
     (define (makelist-of-chains l)
       (match l
         ['() '()]
         [(cons head tail)
          (cons (identify-chain board (list head) (list head))
                (makelist-of-chains tail))]))
     ; list-of-chains is a list of chains of the OPPOSITE color
     (: list-of-chains :  (Listof (Optional (Listof LogicalLoc))))
     (define list-of-chains (makelist-of-chains p2explore))
     ; removes the 'None from the list
     (: lp : (Listof (Optional (Listof LogicalLoc))) ->
        (Listof (Listof LogicalLoc)))
     (define (lp lofchains)
       (match lofchains
         ['() '()]
         [(cons head tail)
          (match head
            ['None (lp tail)]
            [(Some _) (cons (Some-value head) (lp tail))])]))
     ; listof logicalloc equality function
     (: loflloc=? : (Listof LogicalLoc) (Listof LogicalLoc) -> Boolean)
     (define (loflloc=? l1 l2)
       (match l1
         ['() true]
         [(cons h t)
          (if (and (appearsLL? h l2) (loflloc=? t l2)) true false)]))  
     ; remove duplicate chains from the listof chains
     (: remove-duplicates : (Listof (Listof LogicalLoc)) ->
        (Listof (Listof LogicalLoc)))
     (define (remove-duplicates lofchains)
       (match lofchains
         ['() '()]
         [(cons hd tl)
          (cond
            [(empty? tl) (cons hd '())]
            [else
             (if (loflloc=? hd (first tl))
                 (remove-duplicates tl)
                 (cons hd (remove-duplicates tl)))])]))
     ; appends the all the llocs in the chains together into a big list
     (: appender : (Listof (Listof LogicalLoc)) -> (Listof LogicalLoc))
     (define (appender l)
       (match l
         ['() '()]
         [(cons h t)
          (append h (appender t))]))}
    (appender (remove-duplicates (lp list-of-chains)))))

(: boardwithwhitechain3 : Board)
(define boardwithwhitechain3
  (vector
   (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
   (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
   (vector (Some 'black) (Some 'white) (Some 'white) (Some 'black))
   (vector 'None (Some 'black) (Some 'black) 'None)))

(list-lloc-captured boardwithwhitechain3 (LogicalLoc 3 2))

; self-capture: defines a self-capture.
; should remove the self-capture initiated chain
(: self-capture : Board LogicalLoc -> Void)
(define (self-capture board lloc)
  (local
    {(: lp : Board (Listof LogicalLoc) -> Void)
     (define (lp bd locs)
       (match locs
         ['() (void)]
         [(cons h t)
          (begin
            (bd-set! bd h 'None)
            (lp bd t))]))}
    (lp board (match (identify-chain board (list lloc) (list lloc))
                ['None '()]
                [(Some c) c]))))

; eyeball test for self-capture function
(: mymy : Board)
(define mymy
  (vector
   (vector (Some 'white) (Some 'black) 'None 'None 'None)
   (vector (Some 'black) 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None 'None)))

(self-capture mymy (LogicalLoc 0 0))
(display mymy)

; list-lloc-scaptured: list of logicalloc that were self-captured
(: list-lloc-scaptured : Board LogicalLoc -> (Listof LogicalLoc))
(define (list-lloc-scaptured board lloc)
  (match (identify-chain board (list lloc) (list lloc))
    ['None '()]
    [(Some chain) chain]))

; format-sec: given tenths of a second, build string that shows seconds
; ex: (format-sec 17) -> "1.7"
(: format-sec (Integer -> String))
(define (format-sec t)
  (string-append (number->string (quotient t 10))
                 "."
                 (number->string (remainder t 10))))

(check-expect (format-sec 17) "1.7")
(check-expect (format-sec 934729) "93472.9")

; format-min: given tenths of a second build string that shows minutes
(: format-min (Integer -> String))
(define (format-min t)
  (string-append (number->string (quotient (quotient t 10) 60))
                 ":"
                 (format-sec (remainder t 600))))

(check-expect (format-min 1200) "2:0.0")
(check-expect (format-min 1532) "2:33.2")

; draw-timers: draws the timers beneath the message panel
(: draw-timers : World -> Image)
(define (draw-timers w)
  (match w
    [(World (BoardSpec bg cell margin stone)
            (Go board next history _ _ _ _)
            stat
            b-time
            w-time
            hoverloc)
     (overlay
      (above
       (text
        (string-append "Black: " (format-min b-time))
        (int->text-size (quotient margin 2))
        'black)
       (text
        (string-append "White: " (format-min w-time))
        (int->text-size (quotient margin 2))
        'white)
       (text
        (string-append "Total Game Time: "
                       (format-min (+ b-time w-time)))
        (int->text-size (quotient margin 2))
        'black))
      (rectangle (+ (* cell (sub1 (vector-length board))) (* 2 margin))
                    (* margin 4)
                    'solid
                    bg))]))

(draw-timers (World (BoardSpec 'moccasin 30 40 9)
                    (gameBeginning 19)
                    "test message"
                    13445
                    12379
                    (Some (LogicalLoc 1 1))))

; apply-move: copies the current board, saves it to history, then places the stone
; this function takes a Go and LogicalLoc and returns a Go
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move game lloc)
  (local
    {(: copy : Board)
     (define copy (board-copy (Go-board game)))
     (: new-history : (Listof Board))
     (define new-history (append (list copy) (Go-history game)))}
    (match game
      [(Go board next history ltp ltoc ltsc cp)
       (match (board-ref game lloc)
         ['None
          (begin
            (bd-set! board lloc (Some next))
            (local
              {(: oc : (Listof LogicalLoc))
               (define oc (list-lloc-captured board lloc))
               (: sc : (Listof LogicalLoc))
               (define sc (list-lloc-scaptured board lloc))}
              (capture board lloc)
              (self-capture board lloc)
              (Go board (flip next) new-history (Some lloc) oc
                  sc 0)))]
         [(Some _) game])])))

(check-expect (apply-move (gameBeginning 3) (LogicalLoc 0 0))
              (Go (vector (vector (Some 'black) 'None 'None)
                          (vector 'None 'None 'None)
                          (vector 'None 'None 'None))
                  'white
                  (list (vector (vector 'None 'None 'None)
                                (vector 'None 'None 'None)
                                (vector 'None 'None 'None)))
                  (Some (LogicalLoc 0 0))
                  '()
                  '()
                  0))

(check-expect (apply-move
               (Go
                (vector
                 (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
                 (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
                 (vector 'None (Some 'white) (Some 'white) (Some 'black))
                 (vector 'None (Some 'black) (Some 'black) 'None))
                'black
                '()
                (Some (LogicalLoc 0 1))
                '()
                '()
                0) (LogicalLoc 0 2))
              (Go
               (vector
                '#(None None None None)
                '#(None None None None)
                (vector (Some 'black) 'None 'None (Some 'black))
                (vector 'None (Some 'black) (Some 'black) 'None))
               'white
               (list
                (vector
                 (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
                 (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
                 (vector 'None (Some 'white) (Some 'white) (Some 'black))
                 (vector 'None (Some 'black) (Some 'black) 'None)))
               (Some (LogicalLoc 0 2))
               (list
                (LogicalLoc 1 2)
                (LogicalLoc 1 1)
                (LogicalLoc 2 2)
                (LogicalLoc 1 0)
                (LogicalLoc 2 1)
                (LogicalLoc 0 1)
                (LogicalLoc 2 0)
                (LogicalLoc 0 0)
                (LogicalLoc 3 1)
                (LogicalLoc 3 0))
               '()
               0))

; two-passes?: indicates if the past two moves have been passes
(: two-passes? : Go -> Boolean)
(define (two-passes? game)
  (match game
    [(Go board next history ltp ltoc ltsc cp)
     (if (>= cp 2) true false)]))

(check-expect (two-passes? (gameBeginning 19)) false)
(check-expect (two-passes? testGo) false)
(check-expect (two-passes?
               (Go (Go-board testGo)
                   (Go-next-to-play testGo)
                   (Go-history testGo)
                   (Go-last-turn-place testGo)
                   (Go-last-turn-opp-captures testGo)
                   (Go-last-turn-self-captures testGo)
                   2)) true)
                   

; legal-move: if the result of apply-move results in the same board as previous
; point, then returns false
(: legal-move : Go LogicalLoc -> Boolean)
(define (legal-move game lloc)
  (local
    {(: boardinQ : Board)
     (define boardinQ (board-copy (Go-board game)))
     (: lp : Board (Listof Board) -> Boolean)
     (define (lp b hist)
       (match hist
         ['() true]
         [(cons h t)
          (begin
            (bd-set! boardinQ lloc (Some (Go-next-to-play game))) 
            (if (board=?2 b h)
                false
                (lp b t)))]))}
    (match game
      [(Go board next history ltp ltoc ltsc cp)
       (cond
         [(= cp 2) false]
         [else 
          (lp boardinQ (Go-history game))])])))

(check-expect (legal-move
               (Go (vector (vector (Some 'black) 'None 'None)
                           (vector 'None 'None 'None)
                           (vector 'None 'None 'None))
                   'white
                   (list (vector (vector 'None 'None 'None)
                                 (vector 'None 'None 'None)
                                 (vector 'None 'None 'None)))
                   (Some (LogicalLoc 0 0))
                   '()
                   '()
                   0)
               (LogicalLoc 1 1)) true)
(check-expect (legal-move
               (Go (vector (vector (Some 'black) 'None 'None)
                           (vector 'None 'None 'None)
                           (vector 'None 'None 'None))
                   'white
                   (list (vector (vector (Some 'black) 'None 'None)
                                 (vector 'None (Some 'white) 'None)
                                 (vector 'None 'None 'None)))
                   (Some (LogicalLoc 0 0))
                   '()
                   '()
                   0)
               (LogicalLoc 1 1)) false)

; draw the current state of the game, including labels and status message
(: draw : World -> Image)
(define (draw w)
  (match w
    [(World (BoardSpec bg cell margin stone)
            (Go board next history ltp ltoc ltsc cp)
            stat
            b-timer
            w-timer
            hoverloc)
     (local
       {(define len (vector-length board))
        (define msg-panel (message-panel w))
        (define empty-board
          (makeBoard (Go board next history ltp ltoc ltsc cp)
                     (BoardSpec bg cell margin stone)))
        (define timer-panel (draw-timers w))
        (: side-length Integer)
        (define side-length
          (+ (* margin 2)
             (* (sub1 len)
                (int->text-size cell))))
        (: draw-stone : Stone -> Image)
        (define (draw-stone s)
          (circle stone 'solid s))
        (: draw-on-board1 : Stone Integer (Vectorof (Optional Stone)) Image -> Image)
        (define (draw-on-board1 stone ycoord b1 bground)
          (local
            {(define len (vector-length b1))
             (: lp : Integer Image -> Image)
             (define (lp i bground)
               (cond
                 [(= i len) bground]
                 [else
                  (match (logical->physical (LogicalLoc i ycoord) len (World-spec w))
                    [(PhysicalLoc x y)
                     (match (vector-ref b1 i)
                       ['None (lp (+ i 1) bground)]
                       [(Some c)
                        (if
                         (symbol=? c 'white)
                         (place-image (draw-stone 'white) x y (lp (+ i 1) bground))
                         (place-image
                          (draw-stone 'black) x y (lp (+ i 1) bground)))])])]))}
            (lp 0 bground)))
        (: draw-on-board2 : Stone Board Image -> Image)
        (define (draw-on-board2 stone board bground)
          (local
            {(define len (vector-length board))
             (: lp : Integer Image -> Image)
             (define (lp i bground)
               (cond
                 [(= i len) bground]
                 [else
                  (draw-on-board1
                   stone i (vector-ref board i) (lp (+ i 1) bground))]))}
            (lp 0 bground)))
        ; draw ghost stone if the move is valid
        (: draw-ghost-stone : Stone (Optional LogicalLoc) Image -> Image)
        (define (draw-ghost-stone s lloc bg)
          (match lloc
            ['None bg]
            [(Some (LogicalLoc x y))
             (if (legal-move (World-game w) (LogicalLoc x y))
             (match (logical->physical (LogicalLoc x y) len (World-spec w))
               [(PhysicalLoc xx yy)
                (place-image (circle stone 128 s) xx yy bg)])
             bg)]))
        ; draw the last placed stone as a white or black square
        (: draw-lastplaced : Stone (Optional LogicalLoc) Image -> Image)
        (define (draw-lastplaced s lloc bg)
          (match lloc
            ['None bg]
            [(Some (LogicalLoc x y))
             (match (logical->physical (LogicalLoc x y) len (World-spec w))
               [(PhysicalLoc xx yy)
                (place-image (square (* stone 2) 'solid s) xx yy bg)])]))
        ; draw stars where stones were just captured
        (: draw-captured1 : Stone LogicalLoc Image -> Image)
        (define (draw-captured1 s lloc bg)
          (match lloc
            [(LogicalLoc x y)
             (match (logical->physical (LogicalLoc x y) len (World-spec w))
               [(PhysicalLoc xx yy)
                (place-image (star (* stone 2) 'solid s) xx yy bg)])]))
        (: draw-captured2 : Stone (Listof LogicalLoc) Image -> Image)
        (define (draw-captured2 s locs bg)
          (match locs
            ['() bg]
            [(cons h t)
             (draw-captured1 s h (draw-captured2 s t bg))]))
         ; draw ellipses where stones were just self captured
        (: dimen : Integer)
        (define dimen (* stone 2))
        (: ellipsething : Image)
        (define ellipsething
          (underlay (ellipse (exact-floor (/ dimen 6)) dimen 'solid next)
            (ellipse (exact-floor (/ dimen 3))
                     (exact-floor (/ dimen (/ 5 6))) 'solid next)
            (ellipse (exact-floor (/ dimen 2))
                     (exact-floor (/ dimen (/ 2 3))) 'solid next)
            (ellipse (exact-floor (/ dimen (/ 2 3)))
                     (exact-floor (/ dimen 2)) 'solid next)
            (ellipse (exact-floor (/ dimen (/ 5 6)))
                     (exact-floor (/ dimen 3)) 'solid next)
            (ellipse dimen (exact-floor (/ dimen 6)) 'solid next)))
        (: draw-scaptured1 : Stone LogicalLoc Image -> Image)
        (define (draw-scaptured1 s lloc bg)
          (match lloc
            [(LogicalLoc x y)
             (match (logical->physical (LogicalLoc x y) len (World-spec w))
               [(PhysicalLoc xx yy)
                (place-image ellipsething xx yy bg)])]))
        (: draw-scaptured2 : Stone (Listof LogicalLoc) Image -> Image)
        (define (draw-scaptured2 s locs bg)
          (match locs
            ['() bg]
            [(cons h t)
             (draw-scaptured1 s h (draw-scaptured2 s t bg))]))
         }
       (beside/align
        "top"
        (above
         (draw-ghost-stone
          next
          hoverloc
          (draw-lastplaced
           (flip next)
           ltp
           (draw-scaptured2
            next
            ltsc
            (draw-captured2
             (flip next)
             ltoc
             (draw-on-board2 'black board empty-board)))))
         (draw-labelsX w)
         msg-panel
         timer-panel)
        (draw-labelsY
         (- side-length margin)
         (BoardSpec bg cell margin stone)
         (build-list len add1)
         (rectangle margin side-length 'solid 'white))))]))


; eyeball tests: draw
(draw (World (BoardSpec 'tan 14 18 6) (gameBeginning 19) "Welcome to Go!"
             1240 1199 'None))
(draw (World (BoardSpec 'tan 20 24 9) (gameBeginning 19) "Welcome to Go!"
             507 872 'None))
(draw (World (BoardSpec 'tan 10 12 4) (gameBeginning 4) "Welcome to Go!"
             3913 3809 'None))
(draw (World (BoardSpec 'tan 30 15 14) testGo "Welcome to Go!"
             9018 3119 (Some (LogicalLoc 0 0))))
(draw (World (BoardSpec 'tan 30 15 14) testboard3 " adf"
             1240 1199 'None))
(draw (World (BoardSpec 'tan 30 15 14)
             (Go boardwithwhitechain 'white '() 'None '() '() 0)
             "it works!"
             1740 1199 'None))

; add-to-history: applies apply-move to the world and updates the status message
(: add-to-history : LogicalLoc World -> World)
(define (add-to-history lloc w)
  (match w
    [(World (BoardSpec bg cell margin stone)
            (Go board next history ltp ltop ltsc cp) stat b white hover)
     (match (board-ref (World-game w) lloc)
       ['None
        (World (BoardSpec bg cell margin stone) (apply-move (World-game w) lloc)
               (string-append (symbol->string next)
                              " moved to "
                              (logical->string lloc)
                              ".")
               b
               white
               hover)]
       [(Some _) (World
                  (BoardSpec bg cell margin stone)
                  (Go board next history ltp ltop ltsc cp)
                  "The selected location is already occupied."
                  b
                  white
                  hover)])]))

; eyeball test for add-to-history function
(add-to-history (LogicalLoc 1 1)
                (World (BoardSpec 'tan 30 40 14) testboard3 "welcome to go!"
                       100 200 'None))
(draw (add-to-history (LogicalLoc 2 2)
                      (World (BoardSpec 'tan 30 40 14) testboard3 "nice"
                             100 200 'None)))
(display
 (Go-history (World-game
              (add-to-history (LogicalLoc 2 2)
                              (World (BoardSpec 'tan 30 40 14) testboard3 "nice"
                                     100 200 'None)))))

; react-to-tick: increments the timers based on who's turn it is to play
(: react-to-tick : World -> World)
(define (react-to-tick w)
  (match w
    [(World bspec (Go board next history ltp ltop ltsc cp) stat black white hoverloc)
     (if (= cp 2)
         w
         (match next
           ['black
            (World
             bspec
             (World-game w) stat (+ black 1) white hoverloc)]
           ['white
            (World
             bspec
             (World-game w) stat black (+ white 1) hoverloc)]))]
    [_ w]))

; handle-click: puts stone at clicked location, or does nothing
; changes the status message as well.
; Place stone on click, if location is unoccupied.
(: handle-click : World Integer Integer Mouse-Event -> World)
(define (handle-click w x y e)
  (match w
    [(World spec
            (Go board next history ltp  ltpop ltsc cp)
            stat black white hoverloc)
     (match e
       ["button-down"
        (match (physical->logical (PhysicalLoc x y)
                                  (vector-length (Go-board (World-game w))) spec)
          ['None w]
          [(Some lloc)
           (if (legal-move (World-game w) lloc)
               (add-to-history lloc w)
               (World spec (World-game w) "This move is not legal."
                      black white hoverloc))])]
       ["move"
        (match (physical->logical (PhysicalLoc x y)
                                  (vector-length (Go-board (World-game w))) spec)
          ['None w]
          [(Some lloc)
           (if (legal-move (World-game w) lloc)
               (World spec (World-game w) stat black white (Some lloc))
               w)])]
       [_ w])]))

;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))

;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (cond
      [(valid-board-spec? bs)
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world bs (port->string (open-input-file path))))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))]
      [else (error "BoardSpec is not valid.")])))


; handle-key: passes the turn when the key is pressed
; Pass on "p" or "P".
(: handle-key : World String -> World)
(define (handle-key w key)
  (match key
    [(or "P" "p")
     (if (= (Go-consecutive-passes (World-game w)) 2)
         (World (World-spec w) (World-game w) "You cannot pass, the game is over."
                (World-black-tenths w) (World-white-tenths w) (World-hover w))
         (passWorld w))]
    [(or "S" "s")
     (begin
       (save-game! w)
       w)]
    [(or "L" "l")
     (load-game (World-spec w))]
    [_ w]))

(check-expect (handle-key (World (BoardSpec 'tan 10 12 4)
                                 (gameBeginning 8)
                                 "Welcome to Go!"
                                 100
                                 200
                                 'None) "p")
              (World (BoardSpec 'tan 10 12 4)
                     (Go (Go-board (gameBeginning 8))
                         'white (Go-history (gameBeginning 8)) 'None '() '() 1)
                     "black passed."
                     100
                     200
                     'None))


; play: opens a window with the go board/ runs the program
(: play : Integer BoardSpec -> World)
(define (play dimension bspec)
  (cond
    [(< dimension 2) (error "Board is too small.")]
    [(not (valid-board-spec? bspec)) (error "This BoardSpec is not valid.")]
    [else
     (big-bang
         (World bspec (gameBeginning dimension) "Welcome to Go!" 0 0 'None) : World
       [to-draw draw]
       [on-mouse handle-click]
       [on-key handle-key]
       [on-tick react-to-tick 1/10])]))

(check-error (play 1 (BoardSpec 'tan 2 6 1)) "Board is too small.")
(check-error (play 10 (BoardSpec 'tan 10 12 5)) "This BoardSpec is not valid.")
; Interactive version of the Go Board
(play 19 (BoardSpec 'moccasin 30 36 14))


(test)