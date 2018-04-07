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


;turn management section
(define (MovePlayerTo B PlayerXpos PlayerYpos Xpos Ypos)
  (cond
    (validMove? B Xpos Ypos) (ClearTileAt PlayerXpos PlayerYpos)))

(define (validMove? B Xpos Ypos)
  (cond
    ((and (legalTile? B Xpos Ypos) (equal? (findTile B Xpos Ypos) 'X)) #F)
    (else #T)))

;(define (play))
  

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

(define (findEntries B index)
  (cond
    ((> index (sub1 (length (first B)))) '())
    ((findEdgeHelper (first (reverse B)) index) (cons index (findEntries B (add1 index))))
    (else (findEntries B (add1 index)))))

(define (findExits B index)
  (cond
    ((> index (sub1 (length (first B)))) '())
    ((findEdgeHelper (first B) index) (cons index (findExits B (add1 index))))
    (else (findExits B (add1 index)))))

(define (findEdgeHelper L index)
  (cond
    ((equal? (list-ref L index) '_) #T)
    (else #F)))

(define (MazeChecker B startL exitL index)
  (cond
    ((empty? startL) #F)
    ((> index (sub1(length exitL))) (MazeChecker B (rest startL) exitL 0))
    ((PathFinder B (first startL) (sub1(length B)) (list-ref exitL index) 0) #T)
    (else (MazeChecker B startL exitL (add1 index)))))

;"admin" commands section
(define (RegenerateTile B Xpos Ypos)
  (cond
    ((passableMaze? (updateBoard B Xpos Ypos (RandomTileGenerator))) (printBoard (updateBoard B Xpos Ypos (RandomTileGenerator))))
    (else (print '(sorry, but the new maze isn't passable)) (newline) (printBoard (updateBoard B Xpos Ypos (RandomTileGenerator))))))


;auto bord creation and printing
(define B1 (MazeRandomaizer (BoardSize 10 10)))
(printBoard B1)
(newline)(newline)
(printBoard testBoardNO)

;demo section
;empty? #T ;)


;missing comands list (names in use)
;none!!! yay!!!

;WIP notes
;PathFinder - changed to just being input cheker
;wallfollower - 100% acurate but super ineffective (its ok for esting mazes but i'll have to make sometinh else for the mob's movement and the whole "player chasing" part)

;planned commands
;MoveCam (moves the 5*5 visible maze to the player position (just gives the PrintSector the player pos us input)) => maybe I don't need that..... it’s way to simple (just move the printsector to the new player location)
;CreateMobs (creates a given amount of mobs (by some difficulty choice or by a set number from the player or by the maze size) in the maze)
;MoveMobs (makes the mobs move to the player location once every 2 turns (so you chould run away from them buts whould still lose if you're not cerefull)
;FindPath (finds a path between 2 locations and returns the next step) - still a nope... its only #T or #F for now
;FindStart
;FindExit
;error log - will get a function name and the given input, the function will print something like: "pintTile failed with (input) (input) (input)"
;reverseMaze - will change all X tiles to _ and _ to X in a given board
;more will follow (maybe ;))
