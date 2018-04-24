(define testBoard '((_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _)(_ _ _ _ _))) ;an empty 5*5 board
(define testBoardOK '((_ X _ _ _)(_ _ X _ X)(X X _ _ _)(_ _ _ X _)(_ X X X X))) ;a passable 5*5 maze
(define testBoardNO '((X X _ X _)(X _ X _ X)(X X X _ X)(_ X _ X _)(_ X _ X _))) ;an unpassable 5*5 maze

;start game function
(define (play)
  (print '(Enter the size of the board (it dosen't have to be a square), the recomended size is up to 15*15. the first number is the width and the second one in the length of the maze))
  (newline)
  (printBoard (NewBoard)))

(define (inputChecker input)
  (cond
;    ((letter filtering))
    ((< input 1) (print '(0 and below isn't a valid input)) (newline)(newline) (inputChecker (read)))
    ((= input 1) (print '(it's too easy)) (newline) (print '(let's make it a litle more interesting)) (newline)(newline) (inputChecker (read)))
    ((> input 50) (print '(that's a big number, it may couse the maze to generate slowly)) (newline) (print '(pick again please)) (newline)(newline) (inputChecker (read)))
    (else input)))
(define (inputMoveChecker input)
  (cond
    ((equal? input 'W) )
    ((equal? input 'A) )
    ((equal? input 'S) )
    ((equal? input 'D) )
    (else (print '(wrong input, pleaze use W/A/S/D)) (inputMoveChecker (read)))))

;(define (YorN B input)
;  (cond
;    ((equal? input 'y) (printBoard (MazeRandomaizer B)))
;    ((equal? input 'yes) (printBoard (MazeRandomaizer B)))
;    ((equal? input 'yea) (printBoard (MazeRandomaizer B)))
;    ((equal? input 'yep) (printBoard (MazeRandomaizer B)))
;    ((equal? input 'n) (play))
;    ((equal? input 'no) (play))
;    ((equal? input 'nope) (play))
;    (else (print '(um..... I have NO idea what you just said, please try again)) (newline) (YorN B (read)))))
  

;new board creation
(define (NewBoard)
  (SpawnPlayer (CheckAndRegenerate (MazeRandomaizer (BoardSize (inputChecker (read)) (inputChecker (read)))))))

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
(define (MovePlayer B PlayerXpos PlayerYpos Xpos Ypos)
  (cond
    ((validMove? B Xpos Ypos) (updateBoard (ClearTileAt PlayerXpos PlayerYpos) Xpos Ypos 'P))
    (else (print '(Invalid location, please try again)) (MovePlayer B PlayerXpos PlayerYpos (read)))))

(define (validMove? B Xpos Ypos)
  (cond
    ((and (legalTile? B Xpos Ypos) (equal? (findTile B Xpos Ypos) 'X)) #F)
    (else #T)))

(define (nextTurn B PlayerXpos PlayerYpos)
  (cond
    ((= PlayerYpos 0) (print '(you win)))
    (else (print '(press the direction on your next move (W-up,S-down,A-left,D-rigth))) (MovePlayer B PlayerXpos PlayerYpos (inputMoveChecker (read)))))
  (newline)
  (printBoard B))

(define (findPlayer B Xpos Ypos)
  (cond
    ((= Ypos (sub1(length B))) #F)
    ((= Xpos (sub1(length (first B)))) (findPlayer B 0 (add1 Ypos)))
    ((equal? 'P (findTile B Xpos Ypos)) (cons Xpos (cons Ypos '())))
    (else (findPlayer B (add1 Xpos) Ypos))))

(define (SpawnPlayer B)
  (updateBoard B (list-ref (MazeChecker B (findEntries B 0) (findExits B 0) 0 '()) (random (length (MazeChecker B (findEntries B 0) (findExits B 0) 0 '())))) (sub1(length B)) 'P))

;troll logs section
;(define (playTrollEdition)
;  (print '(Enter the size of the board (it dosen't have to be a square), the recomended size is up to 15*15. the first number is the width and the second one in the length of the maze))
;  (newline)
;  (printBoard (CheckAndRegenerate (TROLLLLLLL (MazeRandomaizer (BoardSize (read) (read)))))))

;(define (TROLLLLLLL B)
;  (cond
;    ((= (length B) 1) (print '(again..... realy? )) (print '(just play by the book)) (newline) (play))
;    ((= (length (first B)) 1) (print '(it's not even funny...)) (newline) (print '(let's try that again shall we)) (newline) (play))
;    (else (print '(finally.....that's better)) (newline) B)))


;board management section
(define (updateBoard B Xpos Ypos input)
  (cond
    ((= Ypos 0) (cons (updateCol (first B) Xpos input) (rest B)))
    (else (cons (first B) (updateBoard (rest B) Xpos (sub1 Ypos) input)))))

(define (updateCol L Xpos input)
  (cond
    ((= Xpos 0) (cons input (rest L)))
    (else (cons (first L) (updateCol (rest L) (sub1 Xpos) input)))))

(define (ClearTileAt B Xpos Ypos)
  (cond
    ((legalTile? B Xpos Ypos) (updateBoard B Xpos Ypos '_))
    (else (print '(invalid tile)))))

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

(define (MazeChecker B startL exitL index validEntrances)
  (cond
    ((= (length (first B)) 1) #F)
    ((empty? exitL) #F)
    ((empty? startL) #F)
    ((> index (sub1(length exitL))) (MazeChecker B (rest startL) exitL 0 validEntrances))
    ((and (=(length startL)1) (PathFinder B (first startL) (sub1(length B)) (list-ref exitL index) 0)) (cons (first startL) validEntrances))
    ((PathFinder B (first startL) (sub1(length B)) (list-ref exitL index) 0) (MazeChecker B (rest startL) exitL 0 (cons (first startL) validEntrances)))
    (else (MazeChecker B startL exitL (add1 index) validEntrances))))

(define (CheckAndRegenerate B)
  (cond
    ((MazeChecker B (findEntries B 0) (findExits B 0) 0 '()) B)
    (else (CheckAndRegenerate (MazeRandomaizer B)))))


;pathfinding section
(define (PathFinder B startXpos startYpos targetXpos targetYpos)
  (cond
    ((not (or (validMove? B startXpos startYpos) (validMove? B targetXpos targetYpos))) #F)
    (else (wallFollower B  startXpos startYpos targetXpos targetYpos '()))))

(define (wallFollower B startXpos startYpos targetXpos targetYpos pathL)
  (cond
    ((and (= startXpos targetXpos) (= startYpos targetYpos)) #T)
    ((GoRight? B startXpos startYpos) (wallFollower (updateBoard B startXpos startYpos 'U) (sub1 startXpos) startYpos targetXpos targetYpos (cons (cons startXpos (cons startYpos '())) pathL)))
    ((GoUp?    B startXpos startYpos) (wallFollower (updateBoard B startXpos startYpos 'U) startXpos (sub1 startYpos) targetXpos targetYpos (cons (cons startXpos (cons startYpos '())) pathL)))
    ((GoLeft?  B startXpos startYpos) (wallFollower (updateBoard B startXpos startYpos 'U) (add1 startXpos) startYpos targetXpos targetYpos (cons (cons startXpos (cons startYpos '())) pathL)))
    ((GoDown?  B startXpos startYpos) (wallFollower (updateBoard B startXpos startYpos 'U) startXpos (add1 startYpos) targetXpos targetYpos (cons (cons startXpos (cons startYpos '())) pathL)))
    ((empty? pathL) #F)
    (else (wallFollower (updateBoard B startXpos startYpos 'U) (first(first(reverse pathL))) (first(rest(first(reverse pathL)))) targetXpos targetYpos (reverse(rest(reverse pathL)))))))

(define (GoUp? B startXpos startYpos)
  (cond
    ((and (legalTile? B startXpos (sub1 startYpos)) (not (or (equal? (findTile B startXpos (sub1 startYpos)) 'U) (equal? (findTile B startXpos (sub1 startYpos)) 'X)))) #T)
    (else #F )))

(define (GoDown? B startXpos startYpos)
  (cond
    ((and (legalTile? B startXpos (add1 startYpos)) (not (or (equal? (findTile B startXpos (add1 startYpos)) 'U) (equal? (findTile B startXpos (add1 startYpos)) 'X)))) #T)
    (else #F)))

(define (GoLeft? B startXpos startYpos)
  (cond
    ((and (legalTile? B (add1 startXpos) startYpos) (not (or (equal? (findTile B (add1 startXpos) startYpos) 'U) (equal? (findTile B (add1 startXpos) startYpos) 'X)))) #T)
    (else #F)))

(define (GoRight? B startXpos startYpos)
  (cond
    ((and (legalTile? B (sub1 startXpos) startYpos) (not (or (equal? (findTile B (sub1 startXpos) startYpos) 'U) (equal? (findTile B (sub1 startXpos) startYpos) 'X)))) #T)
    (else #F)))


;"admin" commands section
(define (RegenerateTile B Xpos Ypos)
  (cond
    ((passableMaze? (updateBoard B Xpos Ypos (RandomTileGenerator))) (printBoard (updateBoard B Xpos Ypos (RandomTileGenerator))))
    (else (print '(sorry, but the new maze isn't passable)) (newline) (printBoard (updateBoard B Xpos Ypos (RandomTileGenerator))))))


;startup
(define B (CheckAndRegenerate (MazeRandomaizer (BoardSize 10 10))))
(printBoard B)
(newline)(newline)
(printBoard testBoardOK)


;demo section
;empty? #T ;)


;missing comands list (names in use)
;none!!! yay!!!


;WIP notes
;(= (length) 0)

;planned commands
;MoveCam (moves the 5*5 visible maze to the player position (just gives the PrintSector the player pos us input)) => maybe I don't need that..... it’s way to simple (just move the printsector to the new player location)
;CreateMobs (creates a given amount of mobs (by some difficulty choice or by a set number from the player or by the maze size) in the maze)
;MoveMobs (makes the mobs move to the player location once every 2 turns (so you chould run away from them buts whould still lose if you're not cerefull)
;FindPath (finds a path between 2 locations and returns the next step) - still a nope... its only #T or #F for now
;error log - will get a function name and the given input, the function will print something like: "pintTile failed with (input) (input) (input)"
;reverseMaze - will change all X tiles to _ and _ to X in a given board
;more will follow (maybe ;))
