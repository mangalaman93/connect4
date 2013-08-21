#lang racket
(require graphics/graphics)

;defining 2d-vector functions
;makes a 2d vector having r rows & c column
(define (make-2d-vector r c)
  (build-vector r (lambda (x) (make-vector c #\#))))

;returns the value stored at position (r,c) in vector vec
(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

;set the value val at position (r,c) in vector vec
(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

;creates copy of the vec2 in vec1 such that vec1 remains the same while changing vec2
(define (2d-vector-copy! vec1 vec2)
  (define (copy-helper count)
    (if(= count (vector-length vec1)) (display "")
       (begin (vector-copy! (vector-ref vec1 count)
                            0
                            (vector-ref vec2 count)
                            0
                            (vector-length (vector-ref vec1 0)))
              (copy-helper (+ count 1)))))
  (copy-helper 0))


;defining the game variable
;the main board
(define board (make-2d-vector 6 7))
;game-status will be 1 if game is won by any of the player
;it will be -1 if the game is a draw
(define game-status 0)
(define temp-status 0)
;some colors
(define backg-color (make-rgb .1019 .8 1))
(define board-color "Blue")
(define line-color "Black")
(define button-color "Blue")
;games won by each player
(define games-won-yellow 0)
(define games-won-red 0)
;defining users name
(define red-player "")
(define yellow-player "")
;the one having current turn
(define current-player "")
;determining the chance of playing first is alternate
(define alter-turn 1)
;counting number of threes in the board
(define temp-count 0)


;initialiing the game variable
(define (init)
  ;initializing the board with all zeros
  (set! board (build-vector 6 (lambda (x) (make-vector 7 0))))
  ;set the three count 0
  (set! temp-count 0)
  ;set the status zero
  (set! temp-status 0)
  (set! game-status 0)
  ;changing the alter-turn
  (if(= alter-turn 0) (begin (set! alter-turn 1)
                             (set! current-player yellow-player))
     (begin (set! alter-turn 0)
            (set! current-player red-player))))


;defining the functions used in the main program
;defining the row check initialied with r = 0
(define (row-check r temp-board)
  ;second loop
  (define (row-check-helper counter) 
    ;defining & assigning values to selected four positions where the check is applied
    (define vr0 (2d-vector-ref temp-board r counter))
    (define vr1 (2d-vector-ref temp-board r (+ 1 counter)))
    (define vr2 (2d-vector-ref temp-board r (+ 2 counter)))
    (define vr3 (2d-vector-ref temp-board r (+ 3 counter)))
    ;calculating the sum of values
    (define sum (+ vr0 vr1 vr2 vr3))
    ;changing the temp-status accordingly
    (if(= 4 (abs sum)) (set! temp-status 1)
       (if(= counter 3) '()
          (row-check-helper (+ counter 1)))))
  ;initializing the second loop
  (row-check-helper 0)
  ;calling the function again if required
  (if(or (= temp-status 1) (= r 5)) '()
     (row-check (+ r 1) temp-board)))

;defining the column check
;initialized with the c = 0
(define (column-check c temp-board)
  ;second loop
  (define (column-check-helper counter)
    ;defining & assigning values to selected four positions where the check is applied
    (define vr0 (2d-vector-ref temp-board counter c))
    (define vr1 (2d-vector-ref temp-board (+ 1 counter) c))
    (define vr2 (2d-vector-ref temp-board (+ 2 counter) c))
    (define vr3 (2d-vector-ref temp-board (+ 3 counter) c))
    ;calculating the sum of stored values
    (define sum (+ vr0 vr1 vr2 vr3))
    ;changing the temp-status accordingly
    (if(= 4 (abs sum)) (set! temp-status 1)
       (if(= counter 2) '()
          (column-check-helper (+ counter 1)))))
  ;calling the second loop
  (column-check-helper 0)
  ;calling again the function if required
  (if(or (= temp-status 1) (= c 6)) '()
     (column-check (+ c 1) temp-board)))

;defining the diagonal check for positively sloped diagonals
;initialize column c with 3 & row r with 0
(define (positive-diagonal-check r c temp-board)
  ;defining the second loop
  (define (positive-diagonal-check-helper counter)
    ;calculating sum of values stored at the four position where check is applied at a given loop
    (define vr0 (2d-vector-ref temp-board (+ r counter 0) (- (- c counter) 0)))
    (define vr1 (2d-vector-ref temp-board (+ r counter 1) (- (- c counter) 1)))
    (define vr2 (2d-vector-ref temp-board (+ r counter 2) (- (- c counter) 2)))
    (define vr3 (2d-vector-ref temp-board (+ r counter 3) (- (- c counter) 3)))
    (define sum (+ vr0 vr1 vr2 vr3))
    ;updating the temp-status accordingly
    (if(= 4 (abs sum)) (set! temp-status 1)
       (if(= counter (if(= c 6) (- 2 r) (- c 3))) '()
          (positive-diagonal-check-helper (+ counter 1)))))
  ;initialiing the second loop
  (positive-diagonal-check-helper 0)
  ;calling the function if required
  (if(or (= temp-status 1) (and (= c 6) (= r 2))) '()
     (positive-diagonal-check (if(= c 6) (+ r 1) r)
                              (if(= c 6) 6 (+ 1 c)) temp-board)))

;defining the diagonal check for negatively sloped diagonal
;initialize with row r = 2 & column c = 0
(define (negative-diagonal-check r c temp-board)
  ;defining the second loop
  (define (negative-diagonal-check-helper counter)
    ;calculating sum of values stored at the four position where check is applied at a given loop
    (define vr0 (2d-vector-ref temp-board (+ r counter 0) (+ c counter 0)))
    (define vr1 (2d-vector-ref temp-board (+ r counter 1) (+ c counter 1)))
    (define vr2 (2d-vector-ref temp-board (+ r counter 2) (+ c counter 2)))
    (define vr3 (2d-vector-ref temp-board (+ r counter 3) (+ c counter 3)))
    (define sum (+ vr0 vr1 vr2 vr3))
    ;updating the temp-status accordingly
    (if(= 4 (abs sum)) (set! temp-status 1)
       (if(= counter (if(= c 0) (- 2 r) (- 3 c))) '()
          (negative-diagonal-check-helper (+ counter 1)))))
  ;initialiing the second loop
  (negative-diagonal-check-helper 0)
  ;calling the function if required
  (if(or (= temp-status 1) (and (= c 3) (= r 0))) '()
     (negative-diagonal-check (if(= r 0) r (- r 1))
                              (if(= r 0) (+ 1 c) 0) temp-board)))

;THE MAIN CHECK FUNCTION
(define (check-game-win flag temp-board)
  ;set the temp status 0
  (set! temp-status 0)
  ;calling all the checks
  (row-check 0 temp-board)
  (column-check 0 temp-board)
  (positive-diagonal-check 0 3 temp-board)
  (negative-diagonal-check 2 0 temp-board)
  ;changing the game-satatus if check is made for real check of a game state
  (if(and (= flag 0) (= temp-status 1)) (set! game-status 1)
     (display "")))

;function used in the AI
;defining the row count initialied with r = 0
(define (row-count r temp-board ex-sum)
  ;second loop
  (define (row-count-helper counter) 
    ;defining & assigning values to selected four positions where the count is applied
    (define vr0 (2d-vector-ref temp-board r counter))
    (define vr1 (2d-vector-ref temp-board r (+ 1 counter)))
    (define vr2 (2d-vector-ref temp-board r (+ 2 counter)))
    (define vr3 (2d-vector-ref temp-board r (+ 3 counter)))
    ;calculating the sum of values
    (define sum (+ vr0 vr1 vr2 vr3))
    ;call for another loop  
    (if(= ex-sum sum) (set! temp-count (+ temp-count 1))
       (display ""))
    (if(= counter 3) (display "")
       (row-count-helper (+ counter 1))))
  ;initializing the second loop
  (row-count-helper 0)
  ;calling the function again
  (if(= r 5) (display "")
     (row-count (+ r 1) temp-board ex-sum)))

;defining the column count initialied with c = 0
(define (column-count c temp-board ex-sum)
  ;second loop
  (define (column-count-helper counter) 
    ;defining & assigning values to selected four positions where the count is applied
    (define vr0 (2d-vector-ref temp-board counter c))
    (define vr1 (2d-vector-ref temp-board (+ 1 counter) c))
    (define vr2 (2d-vector-ref temp-board (+ 2 counter) c))
    (define vr3 (2d-vector-ref temp-board (+ 3 counter) c))
    ;calculating the sum of values
    (define sum (+ vr0 vr1 vr2 vr3))
    (if(= ex-sum sum) (set! temp-count (+ temp-count 1))
       (display ""))
    (if(= counter 2) (display "")
       (column-count-helper (+ counter 1))))
  ;initializing the second loop
  (column-count-helper 0)
  ;calling the function again
  (if(= c 6) (display "")
     (column-count (+ c 1) temp-board ex-sum)))

;initialize column c with 3 & row r with 0
(define (positive-diagonal-count r c temp-board ex-sum)
  ;defining the second loop
  (define (positive-diagonal-count-helper counter)
    ;calculating sum of values stored at the four position where count is applied at a given loop
    (define vr0 (2d-vector-ref temp-board (+ r counter 0) (- (- c counter) 0)))
    (define vr1 (2d-vector-ref temp-board (+ r counter 1) (- (- c counter) 1)))
    (define vr2 (2d-vector-ref temp-board (+ r counter 2) (- (- c counter) 2)))
    (define vr3 (2d-vector-ref temp-board (+ r counter 3) (- (- c counter) 3)))
    (define sum (+ vr0 vr1 vr2 vr3))
    ;calling the function again
    (if(= ex-sum sum) (set! temp-count (+ temp-count 1))
       (display ""))
    (if(= counter (if(= c 6) (- 2 r) (- c 3))) (display "")
       (positive-diagonal-count-helper (+ counter 1))))
  ;initialiing the second loop
  (positive-diagonal-count-helper 0)
  ;calling the function if required
  (if(and (= c 6) (= r 2)) (display "")
     (positive-diagonal-count (if(= c 6) (+ r 1) r)
                              (if(= c 6) 6 (+ 1 c))
                              temp-board
                              ex-sum)))

;defining the diagonal count for negatively sloped diagonals
;initialize column c with 3 & row r with 0
(define (negative-diagonal-count r c temp-board ex-sum)
  ;defining the second loop
  (define (negative-diagonal-count-helper counter)
    ;calculating sum of values stored at the four position where count is applied at a given loop
    (define vr0 (2d-vector-ref temp-board (+ r counter 0) (+ c counter 0)))
    (define vr1 (2d-vector-ref temp-board (+ r counter 1) (+ c counter 1)))
    (define vr2 (2d-vector-ref temp-board (+ r counter 2) (+ c counter 2)))
    (define vr3 (2d-vector-ref temp-board (+ r counter 3) (+ c counter 3)))
    (define sum (+ vr0 vr1 vr2 vr3))
    ;calling the function again
    (if(= ex-sum sum) (set! temp-count (+ temp-count 1))
       (display ""))
    (if(= counter (if(= c 0) (- 2 r) (- 3 c))) (display "")
       (negative-diagonal-count-helper (+ counter 1))))
  ;initialiing the second loop
  (negative-diagonal-count-helper 0)
  ;calling the function if required
  (if(and (= c 3) (= r 0)) (display "")
     (negative-diagonal-count (if(= r 0) r (- r 1))
                              (if(= r 0) (+ 1 c) 0)
                              temp-board
                              ex-sum)))

;THE MAIN COUNT FUNCTION
(define (count-game-win temp-board ex-sum)
  (set! temp-count 0)
  ;calling all the counts
  (row-count 0 temp-board ex-sum)
  (column-count 0 temp-board ex-sum)
  (positive-diagonal-count 0 3 temp-board ex-sum)
  (negative-diagonal-count 2 0 temp-board ex-sum))


;initialiing graphics
(open-graphics)
;defining game window
(define game-window (open-viewport "connect 4" 720 560))

;function to draw loading image
(define (draw-loading)
  ((clear-viewport game-window))
  ((draw-solid-rectangle game-window) (make-posn 0 0) 720 560 backg-color)
  ((draw-pixmap game-window) "images/loading.png" (make-posn 260 240)))

;checks if the game is draw
(define (check-draw)
  ;it will be draw if the sum of absolute cell values of uppermost row is 7
  (define sum 0)
  ;calculating the sum
  (define (check-draw-helper colu)
    (if(= colu 7) (if(= sum 7) (set! game-status -1) (display ""))
       (begin (set! sum (+ sum (abs (2d-vector-ref board 0 colu))))
              (check-draw-helper (+ colu 1)))))
  (check-draw-helper 0))

;defining various animation function for drawing the pieces
(define (circle-animation a b e)
  ;first animation function
  (define (circle-animation0 rec-x rec-y color)
    ;co-ordinate of center
    (define cx (+ rec-x 35))
    (define cy (+ rec-y 35))
    ;actual function for drawing
    (define (helper count)
      (if(= count 36) (display "")
         (begin ((draw-solid-ellipse game-window) (make-posn (- cx count) (- cy count))
                                                  (* 2 count)
                                                  (* 2 count)
                                                  color)
                (sleep .01)
                (helper (+ count 1)))))
    (helper 0))
  
  ;second animation function
  (define (circle-animation1 rec-x rec-y color)
    ;co-ordinate of center
    (define cx (+ rec-x 35))
    (define cy (+ rec-y 35))
    ;actual functoin which draws the circle
    (define r 35)
    (define (helper1 theta)
      (if(= theta 0) (display "")
         (begin ((draw-line game-window) (make-posn cx cy)
                                         (make-posn (- cx (* r (sin (* (/ theta 180) pi))))
                                                    (- cy (* r (cos (* (/ theta 180) pi)))))
                                         color)
                (sleep .001)
                (helper1 (- theta 1)))))
    (helper1 360)
    ((draw-solid-ellipse game-window) (make-posn rec-x rec-y) 70 70 color))
  
  ;third animation function
  (define (circle-animation2 rec-x rec-y color)
    ;co-ordinate of center
    (define cx (+ rec-x 35))
    (define cy (+ rec-y 35))
    ;actual function to draw the circle
    (define r 35)
    (define (helper1 theta)
      (if(= theta 0) (display "")
         (begin ((draw-line game-window) (make-posn (- cx (* r (sin (* (/ theta 180) pi))))
                                                    (- cy (* r (cos (* (/ theta 180) pi)))))
                                         (make-posn (+ cx (* r (sin (* (/ theta 180) pi))))
                                                    (+ cy (* r (cos (* (/ theta 180) pi)))))
                                         color)
                (sleep .001)
                (helper1 (- theta 1)))))
    (helper1 180)
    ((draw-solid-ellipse game-window) (make-posn rec-x rec-y) 70 70 color))
  
  (define (circle-animation3 rec-x rec-y color)
    ;co-ordinate of function
    (define cx (+ rec-x 35))
    (define cy (+ rec-y 35))
    ;actual function to draw the circle
    (define r 35)
    (define (helper3 xx)
      (define temp (truncate (sqrt (- (sqr r) (sqr (- xx 35))))))
      (if(= xx 70) (display "")
         (begin ((draw-line game-window) (make-posn (+ rec-x xx) (- cy temp))
                                         (make-posn (+ rec-x xx) (+ cy temp))
                                         color)
                (sleep .005)
                (helper3 (+ xx 1)))))
    (helper3 0)
    ((draw-solid-ellipse game-window) (make-posn rec-x rec-y) 70 70 color))  
  
  ;a random number
  (define tem (random 4))
  
  ;main function to call any of these functions randomly
  (cond
    ((= tem 0) (circle-animation0 a b e))
    ((= tem 1) (circle-animation1 a b e))
    ((= tem 2) (circle-animation2 a b e))
    (else (circle-animation3 a b e))))


;function to fill the given column
(define (fill-cell col)
  ;first checks which row to fill
  (define (fill-cell-helper r0)
    (cond
      ((= (2d-vector-ref board r0 col) 0)
       (begin (if(equal? current-player red-player) (2d-vector-set! board r0 col -1)
                 (2d-vector-set! board r0 col 1))
              (circle-animation (+ (* 80 col) 165) (+ (* 80 r0) 85) (if(equal? current-player red-player) "Red" "yellow"))))
      (else
       (fill-cell-helper (- r0 1)))))
  (fill-cell-helper 5))

;impoting initial image
(draw-loading)
((draw-pixmap game-window) "images/initialwindow.png" (make-posn 0 0))

;click on initial window
(define (initial-window-click)
  ;defining mouse click position
  (define mouse-clik-pos (mouse-click-posn (get-mouse-click game-window)))
  (define xx (posn-x mouse-clik-pos))
  (define yy (posn-y mouse-clik-pos))
  (if(and (> xx 400) (> yy 330)
          (< xx 630) (< yy 395)) (begin ((clear-viewport game-window))
                                        (draw-loading)
                                        ((draw-pixmap game-window) "images/instructions.png"
                                                                   (make-posn 0 0)))
                                 (initial-window-click)))

;instruction window
(define(instruction-window-click)
  ;defining mouse click position
  (define mouse-clik-pos (mouse-click-posn (get-mouse-click game-window)))
  (define xx (posn-x mouse-clik-pos))
  (define yy (posn-y mouse-clik-pos))
  ;responding to click
  (if(and (> xx 250) (> yy 412)
          (< xx 475) (< yy 477)) (begin ((clear-viewport game-window))
                                        (draw-loading)
                                        ((clear-viewport game-window))
                                        ((draw-solid-rectangle game-window) (make-posn 0  0) 720 560 backg-color)
                                        ((draw-pixmap game-window) "images/singleplayer.png" (make-posn 260 200))
                                        ((draw-pixmap game-window) "images/doubleplayer.png" (make-posn 260 250)))
                                 (instruction-window-click)))

;responding to the click (single & double player)
(define (player-window-click)
  ;recording the mouse click
  (define mouse-clik-pos (mouse-click-posn (get-mouse-click game-window)))
  (define xx (posn-x mouse-clik-pos))
  (define yy (posn-y mouse-clik-pos))
  ;responding to mouse click
  (cond
    ((and (< xx 460) (> xx 260)
          (< yy 250) (> yy 200)) (begin (set! red-player "Player1")
                                        (set! yellow-player "Computer")))
    ((and (< xx 460) (> xx 260)
          (< yy 300) (> yy 250)) (begin (set! red-player "Player1")
                                        (set! yellow-player "Player2")))
    (else
     (player-window-click))))

;if player wants to exit
(define (draw-exit)
  ;opens the exit window
  (define exit-window (open-viewport "Exit" 300 150))
  ;colors it
  ((draw-solid-rectangle exit-window) (make-posn 0 0) 300 150 backg-color)
  ;preparing the winodw
  ((draw-string exit-window) (make-posn 40 50) "Are you sure you want to exit?" "White")
  ((draw-pixmap exit-window) "images/yes.png" (make-posn 60 70))
  ((draw-pixmap exit-window) "images/no.png" (make-posn 180 70))
  ;capturing & responding to mouse click
  (define (exit-helper)
    (define mouse-clik-pos (mouse-click-posn (get-mouse-click exit-window)))
    (define xx (posn-x mouse-clik-pos))
    (define yy (posn-y mouse-clik-pos))
    (cond
      ((and (< xx 120) (> xx 60)
            (< yy 110) (> yy 70)) (close-graphics))
      ((and (< xx 230) (> xx 180)
            (< yy 110) (> yy 70)) (begin (close-viewport exit-window)
                                         (play-game-click)))
      (else (exit-helper))))
  (exit-helper))

;if player want to play another game quitting this game
(define (draw-new-game)
  ;opens the confirmation window
  (define exit-window (open-viewport "New Game" 300 150))
  ((draw-solid-rectangle exit-window) (make-posn 0 0) 300 150 backg-color)
  ((draw-string exit-window) (make-posn 40 50) "Are you sure you want to quit this game?" "White")
  ((draw-pixmap exit-window) "images/yes.png" (make-posn 60 70))
  ((draw-pixmap exit-window) "images/no.png" (make-posn 180 70))
  ;captures & responds to the clicks
  (define (new-game-helper)
    (define mouse-clik-pos (mouse-click-posn (get-mouse-click exit-window)))
    (define xx (posn-x mouse-clik-pos))
    (define yy (posn-y mouse-clik-pos))
    (cond
      ((and (< xx 120) (> xx 60)
            (< yy 110) (> yy 70)) (begin (close-viewport exit-window)
                                         (play-game)))
      ((and (< xx 230) (> xx 180)
            (< yy 110) (> yy 70)) (begin (close-viewport exit-window)
                                         (play-game-click)))
      (else (new-game-helper))))
  (new-game-helper))

(define (string-write)
  ;writing the name of the player whose gonna to play
  ((clear-solid-rectangle game-window) (make-posn 20 205) 120 25)
  ((draw-solid-rectangle game-window) (make-posn 20 205) 120 25 (if(equal? current-player red-player) "Red" "Yellow"))
  ((draw-string game-window) (make-posn 30 220) (string-append current-player "'s turn") line-color)
  ((draw-string game-window) (make-posn 31 219) (string-append current-player "'s turn") line-color))

(define (draw-game-status)
  ;opens the status window
  (define status-window (open-viewport "Game Status" 300 250))
  ;colors the window
  ((draw-solid-rectangle status-window) (make-posn 0 0) 300 300 backg-color)
  ;writes the appreciation statements
  ((draw-string status-window) (make-posn 110 20) (cond
                                                    ((= game-status -1) "nicely played")
                                                    ((equal? current-player red-player) "Yellow Player won")
                                                    (else "Red Player won")) line-color)
  ;writing red player name & games won
  ((draw-solid-rectangle status-window) (make-posn 20 40) 120 100 "red")
  ((draw-string status-window) (make-posn 40 65) "Red Player" line-color)
  ((draw-string status-window) (make-posn 41 65) "Red Player" line-color)
  ((draw-line status-window) (make-posn 42 67) (make-posn 102 67) line-color)
  ((draw-string status-window) (make-posn 30 90) red-player line-color)
  ((draw-string status-window) (make-posn 30 110) "Games Won -" line-color)
  ((draw-string status-window) (make-posn 120 110) (number->string games-won-red) line-color)
  
  ;writing yellow player name & games won
  ((draw-solid-rectangle status-window) (make-posn 160 40) 120 100 "yellow")
  ((draw-string status-window) (make-posn 180 65) "Yellow Player" line-color)
  ((draw-string status-window) (make-posn 181 65) "Yellow Player" line-color)
  ((draw-line status-window) (make-posn 183 67) (make-posn 253 67) line-color)
  ((draw-string status-window) (make-posn 170 90) yellow-player line-color)
  ((draw-string status-window) (make-posn 170 110) "Games Won -" line-color)
  ((draw-string status-window) (make-posn 260 110) (number->string games-won-yellow) line-color)
  
  ((draw-string status-window) (make-posn 40 170) "Have another try?" line-color)
  ((draw-pixmap status-window) "images/yes.png" (make-posn 60 185))
  ((draw-pixmap status-window) "images/no.png" (make-posn 180 185))
  
  ;capturing & responding to the mouse click
  (define (status-helper)
    (define mouse-clik-pos (mouse-click-posn (get-mouse-click status-window)))
    (define xx (posn-x mouse-clik-pos))
    (define yy (posn-y mouse-clik-pos))
    (cond
      ((and (< xx 120) (> xx 60)
            (< yy 225) (> yy 185)) (begin (close-viewport status-window)
                                          (play-game)))
      ((and (< xx 230) (> xx 180)
            (< yy 225) (> yy 185)) (begin ((draw-pixmap status-window) "images/credits.png" (make-posn 0 0))
                                          (sleep 1)
                                          (close-graphics)))
      (else (status-helper))))
  (status-helper))


;for human player to play
(define (play-player)
  ;capturing the click position
  (define mouse-clik-pos (mouse-click-posn (get-mouse-click game-window)))
  (define xx (posn-x mouse-clik-pos))
  (define yy (posn-y mouse-clik-pos))
  ;responding to the click
  (cond
    ;for Exit
    ((and (< xx 100) (> xx 40)
          (< yy 174) (> yy 149)) (draw-exit))
    ;for new-game
    ((and (< xx 120) (> xx 31)
          (< yy 124) (> yy 95)) (draw-new-game))
    ;for playing
    ((and (> xx 160) (> yy 80))
     (if(= (2d-vector-ref board 0 (quotient (- xx 160) 80)) 0)
        (begin 
          ;fills the cell
          (fill-cell (quotient (- xx 160) 80))
          ;change the current player
          (if(equal? current-player red-player) (set! current-player yellow-player)
             (set! current-player red-player))
          ;change the name player on window
          (string-write)
          ;checks if the game is won
          (check-game-win 0 board)
          ;analyzing the game-status
          (cond
            ((= game-status 1) (if(equal? current-player yellow-player)
                                  (begin 
                                    ;change the number of games
                                    (set! games-won-red (+ games-won-red 1))
                                    ;calls for another game
                                    (draw-game-status))
                                  ;for another player
                                  (begin (set! games-won-yellow (+ games-won-yellow 1))
                                         (draw-game-status))))
            ;if game is draw
            ((= game-status -1) (draw-game-status))
            (else
             ;calls for another move
             (play-game-click)))
          ;checks if game is draw
          (check-draw))
        ;if the column is already filled
        (play-game-click)))
    ;if some random useless click is made
    (else (play-game-click))))

;returns the column
(define (best-column)
  ;checks the depth & returns accordingly
  (define (get-flag i)
    (if(odd? i) -1 1))
  (define (get-fun i)
    (if(odd? i) < >))
  
  ;checks if the whole board is empty i.e. inital move
  (define (empty-board? b)
    (define sum 0)
    (define (helper col)
      (if(= col 7) (if(= sum 0) #t #f)
         (begin (set! sum (+ sum (abs (2d-vector-ref b 5 col))))
                (helper (+ col 1)))))
    (helper 0))
  
  ;adds a piece in a given column
  (define (add-column! col b count)
    ;finds lowest empty cell of the given column and as soon as get it, fills it
    (define (add-helper r0)
      (if(= (2d-vector-ref b r0 col) 0)
         (2d-vector-set! b r0 col (get-flag (+ 1 count)))
         (add-helper (- r0 1))))
    (add-helper 5))
  
  ;terminates the evalution
  (define (ter-evalute b count)
    (check-game-win 1 b)
    (if(= temp-status 1) (* (get-flag (+ 1 count)) 1000)
       ;ramdomize the the value for a usless board
       (random 500)))
       ;counting could be done but it takes time (max = 16 sec)
       ;(begin (count-game-win b (* (get-flag count) 3))
        ;      (* temp-count 100 (get-flag count)))))
  
  ;evalute the given board on the basis of certain rule
  (define (evalute b dep count1)
    (define (helper2 c board-val count2)
      (define new-board (make-2d-vector 6 7))
      (2d-vector-copy! new-board b)
      (cond
        ;if all the columns are treversed
        ((= c 7) board-val)
        ;if the column is filled already
        ((= 1 (abs (2d-vector-ref new-board 0 c))) (helper2 (+ c 1) board-val count2))
        (else
         (begin (add-column! c new-board count2)
                (let ((next (evalute new-board (+ 1 dep) (+ count2 1))))
                  (if((get-fun count2) next board-val) (helper2 (+ c 1) next count2)
                     (helper2 (+ c 1) board-val count2)))))))
    ;cut at depth equal to 3
    (if(> dep 3) (ter-evalute b (+ count1 1))
       (helper2 0 (* (get-flag (+ 1 count1)) +inf.0) count1)))
  
  (define (helper1 c board-val col)
    (define first-board (make-2d-vector 6 7))
    (2d-vector-copy! first-board board)
    (cond
      ;if all the columns are treversed
      ((= c 7) col)
      ;if the column is filled already
      ((= 1 (abs (2d-vector-ref first-board 0 c))) (helper1 (+ c 1) board-val col))
      (else
       (begin (add-column! c first-board 2) ;coming to depth 1
              (check-game-win 1 first-board)
              (if(= temp-status 1) c
                 (let ((temp (evalute first-board 2 1)))
                   (if(> temp board-val) (helper1 (+ c 1) temp c)
                      (helper1 (+ c 1) board-val col))))))))
  
  ;makes a move in a middle column if board is empty
  (if(empty-board? board) 3
     (helper1 0 -inf.0 -1)))

(define (play-computer)
  ;fills the cell
  (fill-cell (best-column))
  ;change the current player
  (if(equal? current-player red-player) (set! current-player yellow-player)
     (set! current-player red-player))
  ;change the name of player on window
  (string-write)
  ;checks if the game is won
  (check-game-win 0 board)
  ;analyzing the game-status
  (cond
    ((= game-status 1) (if(equal? current-player yellow-player)
                          (begin 
                            ;change the number of games
                            (set! games-won-red (+ games-won-red 1))
                            ;calls for another game
                            (draw-game-status))
                          ;for another player
                          (begin (set! games-won-yellow (+ games-won-yellow 1))
                                 (draw-game-status))))
    ;if game is draw
    ((= game-status -1) (draw-game-status))
    (else
     ;calls for another move
     (play-game-click))))

;calls the function according to the player
(define (play-game-click)
  (if(equal? current-player "Computer") (play-computer)
     (play-player)))

;function for playing game
(define (play-game)
  ;initialies the game-variable
  (init)
  ;coloring the window with background color
  ((draw-solid-rectangle game-window) (make-posn 0 0) 720 560 board-color)
  ;importing the connect 4 image
  ((draw-pixmap game-window) "images/connect4.jpg" (make-posn 0 0))
  ;drawing the circles on the board
  (define (draw-board-circles x-pos y-pos)
    (if(< y-pos 560) (begin
                       ((draw-solid-ellipse game-window) (make-posn x-pos y-pos) 70 70 backg-color)
                       ;drawing borders around circle
                       ((draw-rectangle game-window) (make-posn (- x-pos 5)
                                                                (- y-pos 5)) 80 80 line-color)
                       (draw-board-circles x-pos (+ y-pos 80)))
       (if(< x-pos 720) (draw-board-circles (+ x-pos 80) 85)
          (display ""))))
  (draw-board-circles 165 85)
  ((draw-solid-rectangle game-window) (make-posn 0 80) 159 480 backg-color)
  ;drwing the borders
  ((draw-line game-window) (make-posn 159 80) (make-posn 159 560) line-color)
  
  ;drawing "New Game" buttons on the window
  ((draw-solid-rectangle game-window) (make-posn 31 95) 89 29 button-color)
  ((draw-string game-window) (make-posn 40 115) "New Game" line-color)
  ((draw-string game-window) (make-posn 41 114) "New Game" line-color)
  
  ;writing red player name & games won
  ((draw-solid-rectangle game-window) (make-posn 20 270) 120 100 "red")
  ((draw-string game-window) (make-posn 40 295) "Red Player" line-color)
  ((draw-string game-window) (make-posn 41 295) "Red Player" line-color)
  ((draw-line game-window) (make-posn 42 297) (make-posn 100 297) line-color)
  ((draw-string game-window) (make-posn 30 320) red-player line-color)
  ((draw-string game-window) (make-posn 30 340) "Games Won -" line-color)
  ((draw-string game-window) (make-posn 120 340) (number->string games-won-red) line-color)
  
  ;writing yellow player name & games won
  ((draw-solid-rectangle game-window) (make-posn 20 410) 120 100 "yellow")
  ((draw-string game-window) (make-posn 40 435) "Yellow Player" line-color)
  ((draw-string game-window) (make-posn 41 435) "Yellow Player" line-color)
  ((draw-line game-window) (make-posn 47 437) (make-posn 115 437) line-color)
  ((draw-string game-window) (make-posn 30 460) yellow-player line-color)
  ((draw-string game-window) (make-posn 30 480) "Games Won -" line-color)
  ((draw-string game-window) (make-posn 120 480) (number->string games-won-yellow) line-color)
  (string-write)
  
  ;drawing the exit button
  ((draw-solid-rectangle game-window) (make-posn 40 149) 60 25 button-color)
  ((draw-string game-window) (make-posn 55 168) "Exit" line-color)
  ((draw-string game-window) (make-posn 56 167) "Exit" line-color)
  (play-game-click))

;calling the functions
(initial-window-click)
(instruction-window-click)
(player-window-click)
(play-game)