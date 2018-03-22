(define testBoard '((_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)))
(define testBoardOK '((_ X _ _ _)(_ _ X _ X)(X X _ _ _)(_ _ _ X _)(_ X X X X)))
(define testBoardNO '((X X _ X _)(X _ X _ X)(X X X _ X)(_ X _ X _)(_ X _ X _)))
(define sectorTest '((A B C D E)(F G H I J) (K L M N O)(P Q R S T)(U V W X Y)))

(define (BoardSize length width)
  (cond
    ((= width 0) '())
    (else (cons (line length) (BoardSize length (sub1 width))))))

(define (line width)
  (cond
    ((= width 0) '())
    (else (cons '_ (line (sub1 width))))))

(define (printBoard B)
   (cond
     ((empty? (rest B)) (print (first B)))
     (else (print (first B))
           (newline)
           (printBoard (rest B)))))

(define (PrintSector B Xpos Ypos) ;prints a 5*5 square, the given location is the center 
  (print (cons (findTile B (- Xpos 2) (- Ypos 2)) (cons (findTile B (- Xpos 1) (- Ypos 2)) (cons (findTile B Xpos (- Ypos 2)) (cons (findTile B (+ Xpos 1) (- Ypos 2)) (cons (findTile B (+ Xpos 2) (- Ypos 2)) '()))))))
  (newline)
  (print (cons (findTile B (- Xpos 2) (- Ypos 1)) (cons (findTile B (- Xpos 1) (- Ypos 1)) (cons (findTile B Xpos (- Ypos 1)) (cons (findTile B (+ Xpos 1) (- Ypos 1)) (cons (findTile B (+ Xpos 2) (- Ypos 1)) '()))))))
  (newline)
  (print (cons (findTile B (- Xpos 2) Ypos) (cons (findTile B (- Xpos 1) Ypos) (cons (findTile B Xpos Ypos) (cons (findTile B (+ Xpos 1) Ypos) (cons (findTile B (+ Xpos 2) Ypos) '()))))))
  (newline)
  (print (cons (findTile B (- Xpos 2) (+ Ypos 1)) (cons (findTile B (- Xpos 1) (+ Ypos 1)) (cons (findTile B Xpos (+ Ypos 1)) (cons (findTile B (+ Xpos 1) (+ Ypos 1)) (cons (findTile B (+ Xpos 2) (+ Ypos 1)) '()))))))
  (newline)
  (print (cons (findTile B (- Xpos 2) (+ Ypos 2)) (cons (findTile B (- Xpos 1) (+ Ypos 2)) (cons (findTile B Xpos (+ Ypos 2)) (cons (findTile B (+ Xpos 1) (+ Ypos 2)) (cons (findTile B (+ Xpos 2) (+ Ypos 2)) '())))))))

(define (findTile B Xpos Ypos)
  (list-ref (list-ref B Ypos) Xpos))

(define (MazeRandomaizer B)
  (cond
    ((empty? B) (lineRandomaizer B))
    (else (cons (lineRandomaizer (first B)) (MazeRandomaizer (rest B))))))

(define (lineRandomaizer L)
    (cond
    ((empty? L) '())
    (else (cons (RandomTileGenerator (first L)) (lineRandomaizer (rest L))))))

(define (RandomTileGenerator L)
  (cond
    ((= (random 2) 1) 'X)
    (else '_)))
