(define testBoard '((_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _))) ;an empty 5*5 board
(define testBoardOK '((_ X _ _ _)(_ _ X _ X)(X X _ _ _)(_ _ _ X _)(_ X X X X))) ;a passable 5*5 maze
(define testBoardNO '((X X _ X _)(X _ X _ X)(X X X _ X)(_ X _ X _)(_ X _ X _))) ;an unpassable 5*5 maze

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
(define (MazeRandomaizer B) ;fulls the maze with random walls (the maze isn't always passable but I'll fix that later)
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

(define (passableMaze? B) ;a simple version of the function passableMaze? (does NOT use maze solving)
  (cond
    ((empty? (rest (rest B))) (passableLine? (first B) (first (rest B))))
    (else (and (passableLine? (first B) (first (rest B))) (passableMaze? (rest B))))))

(define (passableLine? L1 L2) ;passableMaze? helper, looks for a path between 2 lines
  (cond
    ((or (empty? L1) (empty? L2)) #f)
    ((and (equal? (first L1) '_) (equal? (first L2) '_)) #t)
    (else (passableLine? (rest L1) (rest L2)))))

;turn management section

;(define (MovePlayerTo B PlayerXpos PlayerYpos Xpos Ypos) - not functional (yet)
;  (cond
;    (validMove? B Xpos Ypos) (ClearTileAt PlayerXpos PlayerYpos)))

(define (validMove? B Xpos Ypos)
  (cond
    ((and (legalTile? B Xpos Ypos) (equal? (findTile B Xpos Ypos) 'X)) #F)
    (else #T)))

;board management section
(define (ClearTileAt B Xpos Ypos)
  (cond
    ((legalTile B Xpos Ypos) (updateBoard B Xpos Ypos '_))
    (else '(ERR - (ClearTileAt B Xpos Ypos)))))

(define (updateBoard B Xpos Ypos input)
  (cond
    ((= Ypos 0) (cons (updateCol (first B) Xpos input) (rest B)))
    (else (cons (first B) (updateBoard (rest B) Xpos (sub1 Ypos) input)))))

(define (updateCol L Xpos input)
  (cond
    ((= Xpos 0) (cons input (rest L)))
    (else (cons (first L) (updateCol (rest L) (sub1 Xpos) input)))))

(define (legalTile? B Xpos Ypos)
  (cond
    ((and (and (or (> Ypos 0) (= Ypos 0)) (or (> Xpos 0) (= Xpos 0))) (and (< Ypos (length B)) (< Xpos (length (first B))))) #T)
    (else #F)))


;"admin" commands section
(define (RegenerateTile B Xpos Ypos)
  (cond
    ((passableMaze? (updateBoard B Xpos Ypos (RandomTileGenerator))) (printBoard (updateBoard B Xpos Ypos (RandomTileGenerator))))
    (else (print '(sorry, but the new maze isn't passable)) (newline) (printBoard (updateBoard B Xpos Ypos (RandomTileGenerator))))))

;auto bord creation and printing
(define B1 (MazeRandomaizer (BoardSize 10 10)))
(printBoard B1)
(newline)
(print (passableMaze? B1))
(newline) (newline)
(printBoard testBoardOK)

;demo section
(define (PathFinder B startXpos startYpos targetXpos targetYpos)
  (cond
    ((or (validMove? B startXpos startYpos) (validMove? B targetXpos targetYpos)) #F)
    (else
     (printBoard (updateBoard B startXpos startYpos 'U))
     (newline) (newline)
     (cond
       ((and (= startXpos targetXpos) (= startYpos targetYpos)) (cons (cons startXpos (cons startYpos '())) '()))
       ((and (legalTile? B (add1 startXpos) startYpos) (not (or (equal? (findTile B (add1 startXpos) startYpos) 'U) (equal? (findTile B (add1 startXpos) startYpos) 'X)))) (cons (cons startXpos (cons startYpos '())) (PathFinder (updateBoard B startXpos startYpos 'U) (add1 startXpos) startYpos targetXpos targetYpos)))
       ((and (legalTile? B startXpos (add1 startYpos)) (not (or (equal? (findTile B startXpos (add1 startYpos)) 'U) (equal? (findTile B startXpos (add1 startYpos)) 'X)))) (cons (cons startXpos (cons startYpos '())) (PathFinder (updateBoard B startXpos startYpos 'U) startXpos (add1 startYpos) targetXpos targetYpos)))
       ((and (legalTile? B (sub1 startXpos) startYpos) (not (or (equal? (findTile B (sub1 startXpos) startYpos) 'U) (equal? (findTile B (sub1 startXpos) startYpos) 'X)))) (cons (cons startXpos (cons startYpos '())) (PathFinder (updateBoard B startXpos startYpos 'U) (sub1 startXpos) startYpos targetXpos targetYpos)))
       ((and (legalTile? B startXpos (sub1 startYpos)) (not (or (equal? (findTile B startXpos (sub1 startYpos)) 'U) (equal? (findTile B startXpos (sub1 startYpos)) 'X)))) (cons (cons startXpos (cons startYpos '())) (PathFinder (updateBoard B startXpos startYpos 'U) startXpos (sub1 startYpos) targetXpos targetYpos)))
       (else (cons (cons 'DEADEND (cons startXpos (cons startYpos '()))) '()))))))

(define (wallFollower B startXpos startYpos targetXpos targetYpos pathL)
  (printBoard (updateBoard B startXpos startYpos 'U))
  (newline)
  (print pathL)
  (newline) (newline)
  (cond
    ((and (= startXpos targetXpos) (= startYpos targetYpos)) (cons (cons startXpos (cons startYpos '())) pathL))
    ((and (legalTile? B (add1 startXpos) startYpos) (not (or (equal? (findTile B (add1 startXpos) startYpos) 'U) (equal? (findTile B (add1 startXpos) startYpos) 'X)))) (wallFollower (updateBoard B startXpos startYpos 'U) (add1 startXpos) startYpos targetXpos targetYpos (cons (cons startXpos (cons startYpos '())) pathL)))
    ((and (legalTile? B startXpos (add1 startYpos)) (not (or (equal? (findTile B startXpos (add1 startYpos)) 'U) (equal? (findTile B startXpos (add1 startYpos)) 'X)))) (wallFollower (updateBoard B startXpos startYpos 'U) startXpos (add1 startYpos) targetXpos targetYpos (cons (cons startXpos (cons startYpos '())) pathL)))
    ((and (legalTile? B (sub1 startXpos) startYpos) (not (or (equal? (findTile B (sub1 startXpos) startYpos) 'U) (equal? (findTile B (sub1 startXpos) startYpos) 'X)))) (wallFollower (updateBoard B startXpos startYpos 'U) (sub1 startXpos) startYpos targetXpos targetYpos (cons (cons startXpos (cons startYpos '())) pathL)))
    ((and (legalTile? B startXpos (sub1 startYpos)) (not (or (equal? (findTile B startXpos (sub1 startYpos)) 'U) (equal? (findTile B startXpos (sub1 startYpos)) 'X)))) (wallFollower (updateBoard B startXpos startYpos 'U) startXpos (sub1 startYpos) targetXpos targetYpos (cons (cons startXpos (cons startYpos '())) pathL)))
    ((empty? pathL) #F)
    (else (wallFollower (updateBoard B startXpos startYpos 'U) (first(first(reverse pathL))) (first(rest(first(reverse pathL)))) targetXpos targetYpos (reverse(rest(reverse pathL)))))))
  


;missing comands list (names in use)
;passableMaze? - its working.. BUT it's catching only some unpassable mazes (because it's not trying to solve the maze but looks for 2 connected passable tiles between 2 lines (these 2 passable tiles may be completely isolated from the rest of the maze))
;validMove?

;WIP notes
;PathFinder - i just need to make him search for a new path from the last point without the “DEADEND" mark (and maybe make him change the ‘step marker’ (the char used to mark visited tiles (U by default)))
;wallfollower - it was able to solve (or discard) all the randomised mazed i threw at it but it's still unable to find the best possible path - shudent be to hard (i can make it run on all the tiles and then run on the U marks like it did on the _ and removing dead ends and again and again buts its not effective enough)

;planned commands
;MoveCam (moves the 5*5 visible maze to the player position (just gives the PrintSector the player pos us input)) => maybe I don't need that..... it’s way to simple
;CreateMobs (creates a given amount of mobs (by some difficulty choice or by a set number from the player or by the maze size) in the maze)
;MoveMobs (makes the mobs move to the player location once every 2 turns (so you chould run away from them buts whould still lose if you're not cerefull)
;FindPath (finds a path between 2 locations and returns the next step) - pathfinder demo WIP 
;FindStart
;FindExit
;error log - will get a function name and the given input, the function will print something like: "fintTile failed with (input) (input) (input)"
;more will follow (maybe ;))
