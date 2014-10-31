#lang racket
;Anthony Mace
;This program allows the user to play connect 4
;against AI

(define AMMGame '())

(define (AMMStartGame)
  (begin
   (set! AMMGame '(●   (□ □ □ □ □ □ □)
                       (□ □ □ □ □ □ □)
                       (□ □ □ □ □ □ □)
                       (□ □ □ □ □ □ □)
                       (□ □ □ □ □ □ □)
                       (□ □ □ □ □ □ □)
                  )
    )
   #t
  )
)

;displays game board
(define (AMMShowGame)
  (begin
    (display (car (cdr (cdr (cdr (cdr (cdr (cdr AMMGame)))))))) (newline)
    (display (car (cdr (cdr (cdr (cdr (cdr AMMGame))))))) (newline)
    (display (car (cdr (cdr (cdr (cdr AMMGame)))))) (newline)
    (display (car (cdr (cdr (cdr AMMGame))))) (newline)
    (display (car (cdr (cdr AMMGame)))) (newline)
    (display (car (cdr AMMGame))) (newline) 
    #t
  )
)

;Player move
(define (AMMMarkMove col)
  (begin    
    (set! AMMGame (cons (AMMChangeTurn)
          (AMMSetCell (cdr AMMGame) (AMMGetRow (cdr AMMGame) 1 col) col (car AMMGame)))
    )
    (if (AMMWinP col)
        (if (eq? (car AMMGame) '●)
            (display (cons '○ "has won!"))
            (display (cons '● "has won!"))
        )
        '()
    )
    (newline)
   col
   )
)

;If my AI has first move, go in the middle
(define (AMMFirstTurn)
  (AMMGoMiddle (car (cdr AMMGame)))
)

;Going in the middle
(define (AMMGoMiddle firstRow)
  (if (null? firstRow)
      #t
      (if (eq? (car firstRow) '□)
          (AMMGoMiddle (cdr firstRow))
          #f
       )
  )
)

;AI move
(define (AMMMakeMove)
  (if (AMMFirstTurn)
      (AMMMarkMove 4)
      (AMMCheckWinningColumns 1)
  )
)
  
;Random, legal move
(define (AMMMakeRandomMove matrix col)
   (if (AMMLegalMoveP col)
      (AMMMarkMove col)
      (AMMMakeRandomMove matrix (AMMGetRandomColumn))
   )
)

;AI implementation for willWin and Blocking
(define (AMMCheckWinningColumns col)
  (if (= col 8)
      (AMMMakeRandomMove (cdr AMMGame) (AMMGetRandomColumn))
      (if (AMMWillWinP col)
          (AMMMarkMove col)
          (if (AMMBlock col)
              (AMMMarkMove col)
              (AMMCheckWinningColumns (+ col 1))
           )
      )
  )
)
    
;Returns random number 1-7 for colum
(define (AMMGetRandomColumn)
  (+ (random 7) 1)
)

;Makes sure move is legal
(define (AMMLegalMoveP col)
  (AMMCheckMove (cdr AMMGame) 1 col)
)

;Helper for legalMove
(define (AMMCheckMove matrix row col)
  (if (= row 7)
      #f
      (if (eq? (AMMGetCell matrix row col) '□)
          #t
          (AMMCheckMove (cdr matrix) (+ row 1) col)
       )
   )
)

;Checks to see if there is a win
(define (AMMWillWinP col)
  (if (or 
       (AMMCheckWillWinVert col)
       (AMMCheckWillWinHoriz col)
       (AMMCheckWillWinDiag col)
      )
      #t
      #f
  )
)

;Will win down
(define (AMMCheckWillWinVert col)
  (if (= 3 (AMMCheckWillWinDownwards (- (AMMGetRow (cdr AMMGame) 1 col) 1) col))
      #t
      #f
  )
)

;Helper for willWinVert
(define (AMMCheckWillWinDownwards row col)
  (if (eq? (AMMGetCell (cdr AMMGame) row col) '())
      0
      (if (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
          (+ 1 (AMMCheckWillWinDownwards (- row 1) col))
          0
       )
   )
)

;WillWinHorizontal
(define (AMMCheckWillWinHoriz col)
  (if (<= 3 (+
             (AMMCheckWillWinRight (AMMGetRow (cdr AMMGame) 1 col) col)
             (AMMCheckWillWinLeft (AMMGetRow (cdr AMMGame) 1 col) col)
            )
       )
       #t
       #f
  )
)

(define (AMMCheckWillWinRight row col)
  (if (eq? (AMMGetCell (cdr AMMGame) row col) '())
      0
      (if (eq? (AMMGetCell (cdr AMMGame) row (+ col 1)) (car AMMGame))
          (+ 1 (AMMCheckWillWinRight row (+ col 1)))
          0
      )
  )
)

(define (AMMCheckWillWinLeft row col)
  (if (eq? (AMMGetCell (cdr AMMGame) row col) '())
      0
      (if (eq? (AMMGetCell (cdr AMMGame) row (- col 1)) (car AMMGame))
          (+ 1 (AMMCheckWillWinLeft row (- col 1)))
          0
      )
  )
)

(define (AMMCheckWillWinDiag col)
  (if (or (<= 3 (+ 
                 (AMMCheckWillWinUpRight (AMMGetRow (cdr AMMGame) 1 col) col)
                 (AMMCheckWillWinDownLeft (AMMGetRow (cdr AMMGame) 1 col) col)
                )
           )
          (<= 3 (+
                 (AMMCheckWillWinUpLeft (AMMGetRow (cdr AMMGame) 1 col) col)
                 (AMMCheckWillWinDownRight (AMMGetRow (cdr AMMGame) 1 col) col)
                )
          )
      )
      #t
      #f
   )
)

(define (AMMCheckWillWinUpRight row col)
  (if (eq? (AMMGetCell (cdr AMMGame) row col) '())
      0
      (if (eq? (AMMGetCell (cdr AMMGame) (+ row 1) (+ col 1)) (car AMMGame))
          (+ 1 (AMMCheckWillWinUpRight (+ row 1) (+ col 1)))
          0
      )
  )
)

(define (AMMCheckWillWinDownLeft row col)
  (if (eq? (AMMGetCell (cdr AMMGame) row col) '())
      0
      (if (eq? (AMMGetCell (cdr AMMGame) (- row 1) (- col 1)) (car AMMGame))
          (+ 1 (AMMCheckWillWinDownLeft (- row 1) (- col 1)))
          0
      )
  )
)

(define (AMMCheckWillWinUpLeft row col)
  (if (eq? (AMMGetCell (cdr AMMGame) row col) '())
      0
      (if (eq? (AMMGetCell (cdr AMMGame) (+ row 1) (- col 1)) (car AMMGame))
          (+ 1 (AMMCheckWillWinUpLeft (+ row 1) (- col 1)))
          0
      )
  )
)

(define (AMMCheckWillWinDownRight row col)
  (if (eq? (AMMGetCell (cdr AMMGame) row col) '())
      0
      (if (eq? (AMMGetCell (cdr AMMGame) (- row 1) (+ col 1)) (car AMMGame))
          (+ 1 (AMMCheckWillWinDownRight (- row 1) (+ col 1)))
          0
      )
  )
)

(define (AMMWinP col)
  (if (or 
       (AMMCheckVertically col)
       (AMMCheckHorizontally col)
       (AMMCheckDiagonally col)
      )
      #t
      #f
  )
)

(define (AMMCheckVertically col)
  (if (= 4 (AMMCheckDownwards (- (AMMGetRow (cdr AMMGame) 1 col) 1) col))
      #t
      #f
   )
)

(define (AMMCheckDownwards row col)
  (if (eq? (AMMGetCell (cdr AMMGame) row col) '())
      0
      (if (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
          0
          (+ 1 (AMMCheckDownwards (- row 1) col))
       )
   )
)

(define (AMMCheckHorizontally col)
  (if (<= 5 (+
            (AMMCheckRight (- (AMMGetRow (cdr AMMGame) 1 col) 1) col)
            (AMMCheckLeft (- (AMMGetRow (cdr AMMGame) 1 col) 1) col)
           )
       )
      #t
      #f
  )
)

(define (AMMCheckRight row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row col) '())
       (eq? (AMMGetCell (cdr AMMGame) row col) '□)
     )
     0
     (+ 1 (AMMCheckRight row (+ col 1)))
  )
)

(define (AMMCheckLeft row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row col) '())
       (eq? (AMMGetCell (cdr AMMGame) row col) '□)
     )
     0
     (+ 1 (AMMCheckLeft row (- col 1)))
  )
)

(define (AMMCheckDiagonally col)
  (if (or (<= 5 (+
             (AMMCheckUpAndRight (- (AMMGetRow (cdr AMMGame) 1 col) 1) col)
             (AMMCheckDownAndLeft (- (AMMGetRow (cdr AMMGame) 1 col) 1) col)
                )
          )
          (<= 5 (+
             (AMMCheckUpAndLeft (- (AMMGetRow (cdr AMMGame) 1 col) 1) col)
             (AMMCheckDownAndRight (- (AMMGetRow (cdr AMMGame) 1 col) 1) col)
               )
          )   
      )
      #t
      #f
  )
)

(define (AMMCheckUpAndRight row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row col) '())
       (eq? (AMMGetCell (cdr AMMGame) row col) '□)
     )
     0
     (+ 1 (AMMCheckUpAndRight (+ row 1) (+ col 1)))
  )
)

(define (AMMCheckDownAndLeft row col)
   (if (or 
       (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row col) '())
       (eq? (AMMGetCell (cdr AMMGame) row col) '□)
     )
     0
     (+ 1 (AMMCheckDownAndLeft (- row 1) (- col 1)))
  )
)

(define (AMMCheckUpAndLeft row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row col) '())
       (eq? (AMMGetCell (cdr AMMGame) row col) '□)
     )
     0
     (+ 1 (AMMCheckUpAndLeft (+ row 1) (- col 1)))
  )
)

(define (AMMCheckDownAndRight row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row col) '())
       (eq? (AMMGetCell (cdr AMMGame) row col) '□)
     )
     0
     (+ 1 (AMMCheckDownAndRight (- row 1) (+ col 1)))
  )
)

(define (AMMBlock col)
  (if (or 
       (AMMBlockVert col)
       (AMMBlockHoriz col)
       (AMMBlockDiag col)
      )
      #t
      #f
  )
)

(define (AMMBlockVert col)
  (if (= 3 (AMMBlockDown (- (AMMGetRow (cdr AMMGame) 1 col) 1) col))
      #t
      #f
  )
)

(define (AMMBlockDown row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) row col) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row col) '())
       (eq? (AMMGetCell (cdr AMMGame) row col) '□)
      )
      0
      (+ 1 (AMMBlockDown (- row 1) col))
  )
)
       
(define (AMMBlockHoriz col)
  (if (<= 3 (+
             (AMMBlockRight (AMMGetRow (cdr AMMGame) 1 col) col)
             (AMMBlockLeft (AMMGetRow (cdr AMMGame) 1 col) col)
            )
      )
      #t
      #f
  )
)

(define (AMMBlockRight row col)
  (if (or
       (eq? (AMMGetCell (cdr AMMGame) row (+ col 1)) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row (+ col 1)) '())
       (eq? (AMMGetCell (cdr AMMGame) row (+ col 1)) '□)
      )
      0
      (+ 1 (AMMBlockRight row (+ col 1)))
  )
)

(define (AMMBlockLeft row col)
  (if (or
       (eq? (AMMGetCell (cdr AMMGame) row (- col 1)) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) row (- col 1)) '())
       (eq? (AMMGetCell (cdr AMMGame) row (- col 1)) '□)
      )
      0
      (+ 1 (AMMBlockLeft row (- col 1)))
  )
)
  
(define (AMMBlockDiag col)
  (if (or (<= 3
              (+
               (AMMBlockUpRight (AMMGetRow (cdr AMMGame) 1 col) col)
               (AMMBlockDownLeft (AMMGetRow (cdr AMMGame) 1 col) col)
              )
          )
          (<= 3
              (+
               (AMMBlockUpLeft (AMMGetRow (cdr AMMGame) 1 col) col)
               (AMMBlockDownRight (AMMGetRow (cdr AMMGame) 1 col) col)
              )
           )
       )
       #t
       #f
    )
)

(define (AMMBlockUpRight row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) (+ row 1) (+ col 1)) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) (+ row 1) (+ col 1)) '())
       (eq? (AMMGetCell (cdr AMMGame) (+ row 1) (+ col 1)) '□)
     )
     0
     (+ 1 (AMMBlockUpRight (+ row 1) (+ col 1)))
  )
)

(define (AMMBlockDownLeft row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) (- row 1) (- col 1)) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) (- row 1) (- col 1)) '())
       (eq? (AMMGetCell (cdr AMMGame) (- row 1) (- col 1)) '□)
     )
     0
     (+ 1 (AMMBlockDownLeft (- row 1) (- col 1)))
  )
)

(define (AMMBlockUpLeft row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) (+ row 1) (- col 1)) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) (+ row 1) (- col 1)) '())
       (eq? (AMMGetCell (cdr AMMGame) (+ row 1) (- col 1)) '□)
     )
     0
     (+ 1 (AMMBlockUpLeft (+ row 1) (- col 1)))
  )
)

(define (AMMBlockDownRight row col)
  (if (or 
       (eq? (AMMGetCell (cdr AMMGame) (- row 1) (+ col 1)) (car AMMGame))
       (eq? (AMMGetCell (cdr AMMGame) (- row 1) (+ col 1)) '())
       (eq? (AMMGetCell (cdr AMMGame) (- row 1) (+ col 1)) '□)
     )
     0
     (+ 1 (AMMBlockDownRight (- row 1) (+ col 1)))
  )
)

(define (AMMGetRow matrix row col)
  (if (or (null? matrix) (= row 7))
      row
      (if (eq? (AMMGetCell matrix 1 col) '□)
          row
          (AMMGetRow (cdr matrix) (+ row 1) col)
      )
  )
)

(define (AMMChangeTurn)
  (if (eq? (car AMMGame) '●)
      '○
      '●
   )
)

(define (AMMGetCell matrix row column)
  (if (null? matrix)
      '()
      (if (= row 1)
          (AMMGetItem (car matrix) column)
          (AMMGetCell (cdr matrix) (- row 1) column)
      )
   )
)

(define (AMMGetItem myRow column)
  (if (null? myRow)
      '()
      (if (= column 1)
          (car myRow)
          (AMMGetItem (cdr myRow) (- column 1))
      )
   )
)

(define (AMMSetCell matrix row column item)
  (if (null? matrix)
      '()
      (if (= row 1)
          (cons (AMMSetItem (car matrix) column item) (cdr matrix))
          (cons 
           (car matrix)
           (AMMSetCell (cdr matrix) (- row 1) column item)
          )
      )
   )
)

(define (AMMSetItem myRow column item)
  (if (null? myRow)
      '()
      (if (= column 1)
          (cons item (cdr myRow))
          (cons 
           (car myRow) 
           (AMMSetItem (cdr myRow) (- column 1) item)
          )
      )
   )
)