(define testBoard '((_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _))) ;an empty 5*5 board
(define testBoardOK '((_ X _ _ _)(_ _ X _ X)(X X _ _ _)(_ _ _ X _)(_ X X X X))) ;a passible 5*5 maze
(define testBoardNO '((X X _ X _)(X _ X _ X)(X X X _ X)(_ X _ X _)(_ X _ X _))) ;an unpassible 5*5 maze

;new board creation
(define (BoardSize length width) ;creates an empty board for the maze to be generated into
  (cond
    ((= width 0) '())
    (else (cons (line length) (BoardSize length (sub1 width))))))

(define (line width) ;creates one line of _ in a given length
  (cond
    ((= width 0) '())
    (else (cons '_ (line (sub1 width))))))

;printing section
(define (printBoard B) ;prints the whole maze
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

(define (printSectorLine B Xpos Ypos counter) ;Print Sector helper, prints one 1*5 line, the given location is the middle of the line
  (cond
    ((= 5 counter) '())
    (else (cons (findTile B (+ Xpos (+ -2 counter)) Ypos) (printSectorLine B Xpos Ypos (add1 counter))))))

(define (findTile B Xpos Ypos) ;returnes a tile in a given location
  (list-ref (list-ref B Ypos) Xpos))

;random section
(define (MazeRandomaizer B) ;fulls the maze with random walls (the maze isn't alwasy passible but I'll fix that later)
  (cond
    ((empty? B) (lineRandomaizer B))
    (else (cons (lineRandomaizer (first B)) (MazeRandomaizer (rest B))))))

(define (lineRandomaizer L) ;MazeRandomaizer helper, runs the RandomTileGenerator for each tile in one line
    (cond
    ((empty? L) '())
    (else (cons (RandomTileGenerator) (lineRandomaizer (rest L))))))

(define (RandomTileGenerator) ;returns a X (wall) or _ (path) tile on random
  (cond
    ((= (random 2) 1) 'X)
    (else '_)))

(define (passibleMaze? B) ;a simple version of the function passibleMaze? (does NOT use maze solving)
  (cond
    ((empty? (rest (rest B))) (passibleLine? (first B) (first (rest B))))
    (else (and (passibleLine? (first B) (first (rest B))) (passibleMaze? (rest B))))))

(define (passibleLine? L1 L2) ;passibleMaze? helper, looks for a path between 2 lines
  (cond
    ((or (empty? L1) (empty? L2)) #f)
    ((and (equal? (first L1) '_) (equal? (first L2) '_)) #t)
    (else (passibleLine? (rest L1) (rest L2)))))

;turn management section
(define (MovePlayerTo PlayerXpos PlayerYpos Xpos ypos)
  (cond-
    ((validMove?) (ClearTileAt PlayerXpos PlayerYpos))))

;board management section
(define (ClearTileAt Xpos Ypos)
  (updateBoard B Xpos Ypos '_))

;"admin" commands section
(define (RegenerateTile B Xpos Ypos)
  (cond
    ((passibleMaze? (updateBoard B Xpos Ypos (RandomTileGenerator))) (updateBoard B Xpos Ypos (RandomTileGenerator)))
    (else '(sorry, but the new maze isn't passible) (printBoard (updateBoard B Xpos Ypos (RandomTileGenerator))))))

;auto bord creation and printing
(define B1 (MazeRandomaizer (BoardSize 10 10)))
(printBoard B1)
(newline)
(print (passibleMaze? B1))

;missing comands list (names in use)
;passibleMaze? - its working.. BUT it's cathing only some unpassible mazes (becouse it's not trying to solve the maze but looks for 2 conected passible tiles between 2 lines (these 2 passible tiles may be completly isolated from the rest of the maze))
;updateBoard
;validMove?

;planned commands
;MoveCam (moves the 5*5 visible maze to the player position (just gives the PrintSector the player pos us input)) => maybe I dont need that..... it’s way to simple
;CreateMobs (creates a given amount of mobs (by some dificulty choise or by a set number from the player or by the maze size) in the maze)
;MoveMobs (makes the mobs move to the player location once every 2 turnes (so you chould run away from them buts whould still lose if you're not cerefull)
;FindPath (finds a path betwwen 2 locations and returnes the next step)
;FindStart
;FindExit
;more will folow (maybe ;))
