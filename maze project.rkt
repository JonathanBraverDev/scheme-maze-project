(define testBoard '((_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _))) ;an empty 5*5 board
(define testBoardOK '((_ X _ _ _)(_ _ X _ X)(X X _ _ _)(_ _ _ X _)(_ X X X X))) ;a passible 5*5 maze
(define testBoardNO '((X X _ X _)(X _ X _ X)(X X X _ X)(_ X _ X _)(_ X _ X _))) ;an unpassible 5*5 maze
(define sectorTest '((A B C D E)(F G H I J) (K L M N O)(P Q R S T)(U V W X Y))) ;list of letters for PrintSector tests (will be deleted soon (I'm done with the function that needs it))

;new board creation
(define (BoardSize length width) ;creates an empty board for the maze to be generated into
  (cond
    ((= width 0) '())
    (else (cons (line length) (BoardSize length (sub1 width))))))

(define (line width) ;creates one line of  _ in a given length
  (cond
    ((= width 0) '())
    (else (cons '_ (line (sub1 width))))))

;printing section
(define (printBoard B) ;prints the whole maze (or the full empty board..... it just prints whatever board you give it, OK? now shash!)
   (cond
     ((empty? (rest B)) (print (first B)))
     (else (print (first B))
           (newline)
           (printBoard (rest B)))))

(define (PrintSector B Xpos Ypos) ;prints a 5*5 square, the given location is the center 
  (print (printSectorLine B Xpos (- Ypos 2) 0))
  (newline)
  (print (printSectorLine B Xpos (- Ypos 1) 0))
  (newline)
  (print (printSectorLine B Xpos Ypos 0))
  (newline)
  (print (printSectorLine B Xpos (+ Ypos 1) 0))
  (newline)
  (print (printSectorLine B Xpos (+ Ypos 2) 0)))

(define (printSectorLine B Xpos Ypos counter) ;PrintSector helper,
  (cond
    ((= 5 counter) '())
    (else (cons (findTile B (+ Xpos (+ -2 counter)) Ypos) (printSectorLine B Xpos Ypos (add1 counter))))))

(define (findTile B Xpos Ypos) ;returnes a tile in agiven location
  (list-ref (list-ref B Ypos) Xpos))

;random section
(define (MazeRandomaizer B) ;fulls the maze with random walls (the maze isnt alwasy passible but I'll fix that later)
  (cond
    ((empty? B) (lineRandomaizer B))
    (else (cons (lineRandomaizer (first B)) (MazeRandomaizer (rest B))))))

(define (lineRandomaizer L) ;MazeRandomaizer helper, runs the RandomTileGenerator for each tile in one line
    (cond
    ((empty? L) '())
    (else (cons (RandomTileGenerator (first L)) (lineRandomaizer (rest L))))))

(define (RandomTileGenerator L) ;returns a X (wall) or _ (path) tile based on a random number
  (cond
    ((= (random 2) 1) 'X)
    (else '_)))

;auto bord creation and printing
(define B1 (MazeRandomaizer (BoardSize 10 10)))
(printBoard B1)
