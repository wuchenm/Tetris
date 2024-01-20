(require 2htdp/image)
(require 2htdp/universe)

;;; Data Definitions

;;; A Brick is a (make-brick Number Number Color)
(define-struct brick [x y color])

;(define (brick-temp b)
;  (...(brick-x b)...
;      (brick-y b)...
;      (brick-color b)...))

;;; A Pt (2D point) is a (make-posn Integer Integer)

;(define (PT-temp p)
;  (... (posn-x p)...
;       (posn-y p)...))

;;; Dir = 'up | 'down | 'left | 'right

;(define (dir-template d)
;  (cond [(symbol=? d 'right) ...]
;        [(symbol=? d 'left) ...]
;        [(symbol=? d 'up) ...]
;        [else ...])) ; down case

;;; A Tetra is a (make-tetra Pt Bricks)
;;; The center point is the point around which the tetra
;;; rotates when it spins.
(define-struct tetra [center bricks])

;(define (tetra-temp t)
;  (...(posn-temp (tetra-center t))...
;      (tetra-temp (brick-temp (tetra-bricks)))...))

;;; A Bricks (Set of Bricks) is one of:
;;; - empty
;;; - (cons Brick Bricks)
;;; Order does not matter.

;(define (bricks-temp b)
;  (cond [(empty? b)...]
;        [else (brick-temp (first b)
;                          (bricks-temp (rest b)))]))


;;; A World is a (make-world Tetra Bricks)
;;; The set of bricks represents the pile of bricks
;;; at the bottom of the screen.
(define-struct world [tetra pile])

;(define (world-temp w)
;  (...(tetra-temp (world-tetra w))...
;      (brick-temp (world-pile w))))

; CONSTANTS
(define BOARD-HEIGHT 20)
(define BOARD-WIDTH 10)
(define PIXELS/CELL 20)
(define START-X 5)
(define START-Y 19)
(define TICK-RATE .2)
(define BRICK-PIXELS 20)
(define GAME-BOARD (empty-scene (* BOARD-WIDTH PIXELS/CELL) (* BOARD-HEIGHT PIXELS/CELL)))

; place-image/cell: Image Number Number Image -> Image
; Places an image with cell coordinated
(define GS (overlay
            (square 5 'outline 'black)
            (square 5 'solid 'green)))
(define BS (overlay
            (square 5 'outline 'black)
            (square 5 'solid 'green)))

(check-expect (place-image/cell GS 5 10 GAME-BOARD)
              (place-image GS
                           (* PIXELS/CELL (+ 1/2 5))
                           (* PIXELS/CELL (- BOARD-HEIGHT (+ 1/2 10)))
                           GAME-BOARD))
(check-expect (place-image/cell GS 6 11 GAME-BOARD)
              (place-image GS
                           (* PIXELS/CELL (+ 1/2 6))
                           (* PIXELS/CELL (- BOARD-HEIGHT (+ 1/2 11)))
                           GAME-BOARD))
                           
(define (place-image/cell i1 x y i2)
  (place-image i1
               (* PIXELS/CELL (+ 1/2 x))
               (* PIXELS/CELL (- BOARD-HEIGHT (+ 1/2 y)))
               i2))

; TETRA SHAPE FUNCTIONS ---------------------------------------------------------------------

; o-tet: Number Number -> Tetra
; Makes the O tetra

(check-expect (o-tet 0 0)
              (make-tetra (make-posn 0 0)
                          (list (make-brick 0 0 'green)
                                (make-brick 0 -1 'green)
                                (make-brick 1 0 'green)
                                (make-brick 1 -1 'green))))
(check-expect (o-tet 5 10)
              (make-tetra (make-posn 5 10)
                          (list (make-brick 5 10 'green)
                                (make-brick 5 9 'green)
                                (make-brick 6 10 'green)
                                (make-brick 6 9 'green))))

(define (o-tet x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'green)
                    (make-brick x (- y 1) 'green)
                    (make-brick (+ 1 x) y 'green)
                    (make-brick (+ 1 x) (- y 1) 'green))))

; i-tet: Number Number -> Tetra
; Makes the I tetra

(check-expect (i-tet 0 0)
              (make-tetra (make-posn 0 0)
                          (list (make-brick 0 0 'blue)
                                (make-brick -1 0 'blue)
                                (make-brick 1 0 'blue)
                                (make-brick 2 0 'blue))))
(check-expect (i-tet 5 10)
              (make-tetra (make-posn 5 10)
                          (list (make-brick 5 10 'blue)
                                (make-brick 4 10 'blue)
                                (make-brick 6 10 'blue)
                                (make-brick 7 10 'blue))))

(define (i-tet x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'blue)
                    (make-brick (- x 1) y 'blue)
                    (make-brick (+ 1 x) y 'blue)
                    (make-brick (+ 2 x) y 'blue))))

; l-tet: Number Number -> Tetra
; Makes the L tetra

(check-expect (l-tet 0 0)
              (make-tetra (make-posn 0 0)
                          (list (make-brick 0 0 'purple)
                                (make-brick -1 0 'purple)
                                (make-brick 1 0 'purple)
                                (make-brick 1 1 'purple))))
(check-expect (l-tet 5 10)
              (make-tetra (make-posn 5 10)
                          (list (make-brick 5 10 'purple)
                                (make-brick 4 10 'purple)
                                (make-brick 6 10 'purple)
                                (make-brick 6 11 'purple))))

(define (l-tet x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'purple)
                    (make-brick (- x 1) y 'purple)
                    (make-brick (+ 1 x) y 'purple)
                    (make-brick (+ 1 x) (+ 1 y) 'purple))))

; J-tet: Number Number -> Tetra
; Makes the J tetra

(check-expect (j-tet 0 0)
              (make-tetra (make-posn 0 0)
                          (list (make-brick 0 0 'cyan)
                                (make-brick 0 1 'cyan)
                                (make-brick 1 0 'cyan)
                                (make-brick 2 0 'cyan))))
(check-expect (j-tet 5 10)
              (make-tetra (make-posn 5 10)
                          (list (make-brick 5 10 'cyan)
                                (make-brick 5 11 'cyan)
                                (make-brick 6 10 'cyan)
                                (make-brick 7 10 'cyan))))

(define (j-tet x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'cyan)
                    (make-brick x (+ 1 y) 'cyan)
                    (make-brick (+ 1 x) y 'cyan)
                    (make-brick (+ 2 x) y 'cyan))))

; t-tet: Number Number -> Tetra
; Makes the T tetra

(check-expect (t-tet 0 0)
              (make-tetra (make-posn 0 0)
                          (list (make-brick 0 0 'orange)
                                (make-brick -1 0 'orange)
                                (make-brick 0 1 'orange)
                                (make-brick 1 0 'orange))))
(check-expect (t-tet 5 10)
              (make-tetra (make-posn 5 10)
                          (list (make-brick 5 10 'orange)
                                (make-brick 4 10 'orange)
                                (make-brick 5 11 'orange)
                                (make-brick 6 10 'orange))))

(define (t-tet x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'orange)
                    (make-brick (- x 1) y 'orange)
                    (make-brick x (+ 1 y) 'orange)
                    (make-brick (+ x 1) y 'orange))))

; z-tet: Number Number -> Tetra
; Makes the z tetra

(check-expect (z-tet 0 0)
              (make-tetra (make-posn 0 0)
                          (list (make-brick 0 0 'pink)
                                (make-brick -1 0 'pink)
                                (make-brick 0 1 'pink)
                                (make-brick 1 1 'pink))))
(check-expect (z-tet 5 10)
              (make-tetra (make-posn 5 10)
                          (list (make-brick 5 10 'pink)
                                (make-brick 4 10 'pink)
                                (make-brick 5 11 'pink)
                                (make-brick 6 11 'pink))))

(define (z-tet x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'pink)
                    (make-brick (- x 1) y 'pink)
                    (make-brick x (+ y 1) 'pink)
                    (make-brick (+ 1 x) (+ y 1) 'pink))))

; s-tet: Number Number -> Tetra
; Makes the S tetra

(check-expect (s-tet 0 0)
              (make-tetra (make-posn 0 0)
                          (list (make-brick 0 0 'red)
                                (make-brick -1 0 'red)
                                (make-brick 0 1 'red)
                                (make-brick 1 1 'red))))
(check-expect (s-tet 5 10)
              (make-tetra (make-posn 5 10)
                          (list (make-brick 5 10 'red)
                                (make-brick 4 10 'red)
                                (make-brick 5 11 'red)
                                (make-brick 6 11 'red))))

(define (s-tet x y)
  (make-tetra (make-posn x y)
              (list (make-brick x y 'red)
                    (make-brick (- x 1) y 'red)
                    (make-brick x (+ 1 y) 'red)
                    (make-brick (+ x 1) (+ 1 y) 'red))))

; RENDERING MOVEMENT FUNCTIONS ---------------------------------------------------------------------

; brick+scene: Brick Image -> Image
; Adds a single brick to a scene  
(define brick-1 (make-brick 2 3 'blue))
(define brick-2 (make-brick 2 4 'purple))

(check-expect (brick+scene brick-1 GAME-BOARD)
              (place-image/cell
               (overlay
                (square BRICK-PIXELS 'outline 'black)
                (square BRICK-PIXELS 'solid 'blue))
               2 3 
               GAME-BOARD))
(check-expect (brick+scene brick-2 GAME-BOARD)
              (place-image/cell
               (overlay
                (square BRICK-PIXELS 'outline 'black)
                (square BRICK-PIXELS 'solid 'purple))
               2 4
               GAME-BOARD))

(define (brick+scene b i)
  (place-image/cell
   (overlay
    (square BRICK-PIXELS 'outline 'black)
    (square BRICK-PIXELS 'solid (brick-color b)))
   (brick-x b)
   (brick-y b)
   i))

; bricks+scene: Bricks Image -> Image
; Adds all bricks to the given scene
(define B0 '())
(define B1 (list brick-1 brick-2))

(check-expect (bricks+scene B0 GAME-BOARD) GAME-BOARD)
(check-expect (bricks+scene B1 GAME-BOARD) (brick+scene brick-1
                                                        (brick+scene brick-2 GAME-BOARD)))

(define (bricks+scene b i)
  (cond [(empty? b) i]
        [else (brick+scene (first b)
                           (bricks+scene (rest b) i))]))

; tetra+scene: Tetra Image -> Image
; Adds a tetra to the given scene

(define T0 (make-tetra (make-posn 2 3) B0))
(define T1 (make-tetra (make-posn 2 3) B1))

(check-expect (tetra+scene T0 GAME-BOARD) GAME-BOARD)
(check-expect (tetra+scene T1 GAME-BOARD) (bricks+scene B1 GAME-BOARD))

(define (tetra+scene t i)
  (bricks+scene (tetra-bricks t) i))

; pile+scene: Bricks -> Scene 
; Adds a world pile to a given scene

(define (pile+scene w)
  (cond [(empty? w) GAME-BOARD]
        [else (tetra+scene (first w)
                           (pile+scene (rest w)))]))
                           
; world->scene: World -> Image
; Renders the given world onto an image

(define W0 (make-world (make-tetra (make-posn 2 3) B0) (list T0)))
(define W1 (make-world (make-tetra (make-posn 2 3) B1) (list T1)))

;(check-expect (world->scene W0) GAME-BOARD)
;(check-expect (world->scene W1) (tetra+scene T1 GAME-BOARD))

(define (world->scene w)
  (tetra+scene (world-tetra w) (pile+scene (world-pile w))))

; DOWN MOVEMENT FUNCTIONS ---------------------------------------------------------------------

; move-down-brick: Brick -> Brick
; Moves a brick one cell down

(check-expect (move-down-brick brick-1) (make-brick 2 2 'blue))
(check-expect (move-down-brick brick-2) (make-brick 2 3 'purple))

(define (move-down-brick b)
  (make-brick
   (brick-x b)
   (- (brick-y b) 1)
   (brick-color b)))

; move-down-bricks: Bricks -> Bricks
; Moves down a list of bricks

(check-expect (move-down-bricks B0) B0)
(check-expect (move-down-bricks B1) (list (move-down-brick brick-1)
                                          (move-down-brick brick-2)))

(define (move-down-bricks b)
  (cond [(empty? b) b]
        [else (cons (move-down-brick (first b))
                    (move-down-bricks (rest b)))]))

; move-down-tetra: Tetra -> Tetra
; Moves a tetra down

(check-expect (move-down-tetra T0) (make-tetra (make-posn 2 2) B0))
(check-expect (move-down-tetra T1) (make-tetra (make-posn 2 2) (move-down-bricks B1)))

(define (move-down-tetra t)
  (make-tetra (make-posn (posn-x (tetra-center t))
                         (- (posn-y (tetra-center t)) 1))
              (move-down-bricks (tetra-bricks t))))

; next-world: World -> World 
; Returns the next world state

(check-random (next-world (make-world
                           (make-tetra (make-posn 2 2)
                                       (list (make-brick 2 2 'green) (make-brick 2 3 'green)))
                           (list (make-tetra (make-posn 2 2)
                                             (list (make-brick 2 2 'green))))))
              (tetra->pile (make-world
                            (make-tetra (make-posn 2 2)
                                        (list (make-brick 2 2 'green) (make-brick 2 3 'green)))
                            (list (make-tetra (make-posn 2 2)
                                              (list (make-brick 2 2 'green)))))))
(check-random (next-world (make-world
                           (make-tetra (make-posn 2 2)
                                       (list (make-brick 2 2 'green) (make-brick 2 3 'green)))
                           (list (make-tetra (make-posn 6 6)
                                             (list (make-brick 6 6 'green))))))
              (make-world (move-down-tetra
                           (make-tetra (make-posn 2 2)
                                       (list (make-brick 2 2 'green) (make-brick 2 3 'green))))
                          (list (make-tetra (make-posn 6 6)
                                            (list (make-brick 6 6 'green))))))  

(define (next-world w)
  (if (and (world-bottom? w)
           (world-overlap? (make-world (move-down-tetra (world-tetra w)) (world-pile w)))
           (not (pile-out-of-bounds? w)))
      (make-world (move-down-tetra (world-tetra w)) (world-pile w))
      (tetra->pile w)))

; LEFT MOVEMENT FUNCTIONS ---------------------------------------------------------------------

; move-left-brick: Brick -> Brick
; Moves a brick one cell left

(check-expect (move-left-brick brick-1) (make-brick 1 3 'blue))
(check-expect (move-left-brick brick-2) (make-brick 1 4 'purple))

(define (move-left-brick b)
  (make-brick
   (- (brick-x b) 1)
   (brick-y b)
   (brick-color b)))

; move-left-bricks: Bricks -> Bricks
; Moves left a list of bricks

(check-expect (move-left-bricks B0) B0)
(check-expect (move-left-bricks B1) (list (move-left-brick brick-1)
                                          (move-left-brick brick-2)))

(define (move-left-bricks b)
  (cond [(empty? b) b]
        [else (cons (move-left-brick (first b))
                    (move-left-bricks (rest b)))]))

; move-left-tetra: Tetra -> Tetra
; Moves a tetra left

;(check-expect (move-left-tetra T0) (make-tetra (make-posn 2 3) B0))
;(check-expect (move-left-tetra T1) (make-tetra (make-posn 2 3) (move-left-bricks B1)))

(define (move-left-tetra t)
  (make-tetra (make-posn (- (posn-x (tetra-center t)) 1)
                         (posn-y (tetra-center t)))       
              (move-left-bricks (tetra-bricks t))))


; RIGHT MOVEMENT FUNCTIONS ---------------------------------------------------------------------

; move-right-brick: Brick -> Brick
; Moves a brick one cell right

(check-expect (move-right-brick brick-1) (make-brick 3 3 'blue))
(check-expect (move-right-brick brick-2) (make-brick 3 4 'purple))

(define (move-right-brick b)
  (make-brick
   (+ (brick-x b) 1)
   (brick-y b)
   (brick-color b)))

; move-right-bricks: Bricks -> Bricks
; Moves right a list of bricks

(check-expect (move-right-bricks B0) B0)
(check-expect (move-right-bricks B1) (list (move-right-brick brick-1)
                                           (move-right-brick brick-2)))

(define (move-right-bricks b)
  (cond [(empty? b) b]
        [else (cons (move-right-brick (first b))
                    (move-right-bricks (rest b)))]))

; move-right-tetra: Tetra -> Tetra
; Moves a tetra right

;(check-expect (move-right-tetra T0) (make-tetra (make-posn 2 3) B0))
;(check-expect (move-right-tetra T1) (make-tetra (make-posn 2 3) (move-right-bricks B1)))

(define (move-right-tetra t)
  (make-tetra (make-posn (+ (posn-x (tetra-center t)) 1)
                         (posn-y (tetra-center t)))        
              (move-right-bricks (tetra-bricks t))))

; ROTATION FUNCTIONS ---------------------------------------------------------------------

;; brick-rotate-ccw : Brick Pt -> Brick
;; Rotate the brick 90 counterclockwise around the posn.
;; Can call 3 times for clockwise 

(check-expect (brick-rotate-ccw brick-1 (make-posn 5 5)) (make-brick 7 2 'blue))
(check-expect (brick-rotate-ccw brick-2 (make-posn 3 3)) (make-brick 2 2 'purple))

(define (brick-rotate-ccw sqr c)
  (make-brick (+ (posn-x c)
                 (- (posn-y c)
                    (brick-y sqr)))
              (+ (posn-y c)
                 (- (brick-x sqr)
                    (posn-x c)))
              (brick-color sqr)))

; bricks-rotate-ccw: Bricks Pt -> Bricks
; Rotates all bricks in a list 90 counterclockwise around the posn

(check-expect (bricks-rotate-ccw B0 (make-posn 5 5)) B0)
(check-expect (bricks-rotate-ccw B1 (make-posn 3 3))
              (list (brick-rotate-ccw brick-1 (make-posn 3 3))
                    (brick-rotate-ccw brick-2 (make-posn 3 3))))

(define (bricks-rotate-ccw b c)
  (cond [(empty? b) b]
        [else (cons (brick-rotate-ccw (first b) c) (bricks-rotate-ccw (rest b) c))]))

; tetra-rotate-ccw: Tetra Pt -> Tetra
; Rotates a tetra counterclockwise

(check-expect (tetra-rotate-ccw T0)
              (make-tetra
               (tetra-center T0)
               (bricks-rotate-ccw B0 (tetra-center T0))))
(check-expect (tetra-rotate-ccw T1)
              (make-tetra
               (tetra-center T1)
               (bricks-rotate-ccw B1 (tetra-center T1))))

(define (tetra-rotate-ccw t)
  (make-tetra (tetra-center t) (bricks-rotate-ccw (tetra-bricks t) (tetra-center t))))

; BOTTOM BOUNDARY FUNCTIONS -------------------------------------------------------------

; bottom-brick?: Brick -> Boolean
; Checks if a brick has reached the bottom of the grid

(define BB1 (make-brick 6 0 'green))
(define BB2 (make-brick 5 1 'green))

(check-expect (bottom-brick? BB1) #f)
(check-expect (bottom-brick? BB2) #t)

(define (bottom-brick? b)
  (if (> (brick-y b) 0) #t #f))

; bottom-bricks?: Bricks -> Boolean
; Checks if the bricks have reached the bottom of the grid

(define BL0 '())
(define BL1 (list BB1 BB2))

(check-expect (bottom-bricks? BL0) #t)
(check-expect (bottom-bricks? BL1) #f)

(define (bottom-bricks? b)
  (cond [(empty? b) #t]
        [else (if (bottom-brick? (first b))
                  (bottom-bricks? (rest b))
                  #f)]))

; bottom-tetra?: Tetra -> Boolean
; Checks if the tetra has reached the bottom of the grid

(define BT0 (make-tetra (make-posn 4 7) BL0))
(define BT1 (make-tetra (make-posn 4 7) BL1))

(check-expect (bottom-tetra? BT0) #t)
(check-expect (bottom-tetra? BT1) #f)

(define (bottom-tetra? t)
  (bottom-bricks? (tetra-bricks t)))

; world-bottom?: World -> Boolean
; Checks to see if a world is valid by checking the tetra

(define WB0 (make-world BT0 (list BT1)))
(define WB1 (make-world BT1 (list (make-tetra (make-posn 5 8) BL1))))

(check-expect (world-bottom? WB0) #t)
(check-expect (world-bottom? WB1) #f)

(define (world-bottom? w)
  (bottom-tetra? (world-tetra w)))

; GRID BOUNDARY FUNCTIONS -------------------------------------------------------------

; in-grid-brick?: Brick -> Boolean
; Checks to see if the brick is valid

(define VB1 (make-brick 5 19 'green))
(define VB2 (make-brick 1 15 'green))
(define IB (make-brick -1 -1 'green))

(check-expect (in-grid-brick? VB1) #t)
(check-expect (in-grid-brick? VB2) #t)
(check-expect (in-grid-brick? IB) #f)

(define (in-grid-brick? b)
  (if (and
       (>= (brick-x b) 0)
       (<= (brick-x b) 9)) #t #f))

; in-grid-bricks?: Bricks -> Boolean 
; Checks to see if bricks are valid

(define VL (list VB1 VB2))
(define IL (list VB1 IB))
(define EL '())

(check-expect (in-grid-bricks? VL) #t)
(check-expect (in-grid-bricks? IL) #f)
(check-expect (in-grid-bricks? EL) #t)

(define (in-grid-bricks? b)
  (cond [(empty? b) #t]
        [else (if (in-grid-brick? (first b)) 
                  (in-grid-bricks? (rest b))
                  #f)]))
       
; in-grid-tetra?: Tetra -> Boolean
; Checks to see if the tetra is valid

(define VT (make-tetra (make-posn 5 19) VL))
(define IT1 (make-tetra (make-posn -1 -1) VL))
(define IT2 (make-tetra (make-posn 6 15) IL))

(check-expect (in-grid-tetra? VT) #t)
(check-expect (in-grid-tetra? IT1) #f)
(check-expect (in-grid-tetra? IT2) #f)

(define (in-grid-tetra? t)
  (and
   (and (>= (posn-x (tetra-center t)) 0)
        (<= (posn-x (tetra-center t)) 10))
   (in-grid-bricks? (tetra-bricks t))))

; in-grid-world?: World -> Boolean
; Checks to see if the world is valid                             

(define VW (make-world VT (list (make-tetra (make-posn 5 2) VL))))
(define IW (make-world IT1 (list (make-tetra (make-posn 5 2) VL))))

(check-expect (in-grid-world? VW) #t)
(check-expect (in-grid-world? IW) #f)

(define (in-grid-world? w)
  (in-grid-tetra? (world-tetra w)))

; OVERLAP FUNCTIONS ---------------------------------------------------------------------------------

; brick-overlap?: Brick Brick -> Boolean
; Does a brick overlap with another brick

(check-expect (brick-overlap? (make-brick 5 5 'green) (make-brick 5 5 'green)) #f)
(check-expect (brick-overlap? (make-brick 6 6 'blue) (make-brick 6 7 'blue)) #t)
(check-expect (brick-overlap? (make-brick 6 6 'blue) (make-brick 7 6 'blue)) #t)

(define (brick-overlap? b1 b2)
  (cond [(= (brick-x b1) (brick-x b2))
         (not (= (brick-y b1) (brick-y b2)))]
        [(= (brick-y b1) (brick-y b2))
         (not (= (brick-x b1) (brick-x b2)))]
        [else #t]))


; bricks-overlap?: Bricks Bricks -> Boolean
; Do a list of bricks overlap 

(check-expect (bricks-overlap?
               (list (make-brick 5 5 'red) (make-brick 5 6 'red))
               (list (make-brick 7 7 'red) (make-brick 5 5 'red))) #f)          
(check-expect (bricks-overlap?
               (list (make-brick 5 5 'red) (make-brick 5 6 'red))
               (list (make-brick 3 4 'red) (make-brick 9 10 'red))) #t)
(check-expect (bricks-overlap?
               (list (make-brick 3 4 'red) (make-brick 9 10 'red))
               '()) #t)


(define (bricks-overlap? b1 b2)
  (cond [(empty? b1) #t]
        [(empty? b2) #t]
        [else (if (and 
                   (brick-list-overlap? (first b1) b2)
                   (brick-list-overlap? (first b2) b1))
                  (bricks-overlap? (rest b1) (rest b2))
                  #f)]))


; brick-list-overlap?: Brick Bricks -> Boolean
; Checks to see if a brick over laps with any individual brick in a list

(check-expect (brick-list-overlap?
               (make-brick 5 5 'green)
               (list (make-brick 4 4 'green) (make-brick 5 5 'green))) #f)
(check-expect (brick-list-overlap?
               (make-brick 6 6 'green)
               (list (make-brick 4 4 'green) (make-brick 5 5 'green))) #t)

(define (brick-list-overlap? b bl)
  (cond [(empty? bl) #t]
        [else (and
               (brick-overlap? b (first bl))
               (brick-list-overlap? b (rest bl)))]))


; tetra-overlap?: Tetra -> Boolean
; Does the tetra overlap with another tetra

(check-expect (tetra-overlap?
               (make-tetra (make-posn 4 4) (list (make-brick 5 5 'red) (make-brick 5 6 'red)))
               (make-tetra (make-posn 4 5) (list (make-brick 4 4 'red) (make-brick 5 5 'red))))
              #f)
(check-expect (tetra-overlap?
               (make-tetra (make-posn 5 6) (list (make-brick 5 5 'red) (make-brick 5 7 'red)))
               (make-tetra (make-posn 4 5) (list (make-brick 3 4 'red) (make-brick 9 10 'red))))
              #t)

(define (tetra-overlap? t1 t2)
  (bricks-overlap? (tetra-bricks t1) (tetra-bricks t2)))

; world-overlap? World -> Boolean
; Does the tetra overlap with any tetra in the pile?

(check-expect (world-overlap?
               (make-world
                (make-tetra (make-posn 4 4) (list (make-brick 4 4 'red) (make-brick 4 5 'red)))
                (list
                 (make-tetra (make-posn 4 5) (list (make-brick 4 5 'red) (make-brick 5 6 'red))))))
              #f)
(check-expect (world-overlap?
               (make-world
                (make-tetra (make-posn 4 4) (list (make-brick 5 5 'red) (make-brick 5 6 'red)))
                (list
                 (make-tetra (make-posn 9 10) (list (make-brick 8 10 'red) (make-brick 7 10 'red))))))
              #t)

(define (world-overlap? w)
  (pile-overlap? (world-tetra w) (world-pile w)))

; pile-overlap: Tetra Pile -> Boolean
; Checks if a tetra overlaps with any pile in a list

; A Pile is a list of Tetras within a World

(check-expect (pile-overlap?
               (make-tetra (make-posn 3 3) (list (make-brick 3 3 'green)))
               (list (make-tetra (make-posn 3 3)
                                 (list (make-brick 3 3 'green) (make-brick 3 4 'green)))))
              #f)
(check-expect (pile-overlap?
               (make-tetra (make-posn 3 3) (list (make-brick 3 3 'green)))
               (list (make-tetra (make-posn 3 4)
                                 (list (make-brick 3 4 'green) (make-brick 3 3 'green)))))
              #f)
(check-expect (pile-overlap?
               (make-tetra (make-posn 3 3) (list (make-brick 3 3 'green)))
               (list (make-tetra (make-posn 5 6)
                                 (list (make-brick 5 6 'green) (make-brick 5 7 'green)))))
              #t)
(check-expect (pile-overlap?
               (make-tetra (make-posn 3 3) (list (make-brick 3 3 'green)))
               (list (make-tetra (make-posn 5 6)
                                 (list (make-brick 5 6 'green) (make-brick 5 7 'green)))
                     (make-tetra (make-posn 6 6)
                                 (list (make-brick 6 6 'green) (make-brick 6 7 'green)))))
              #t)

(define (pile-overlap? t p)
  (cond [(empty? p) #t]
        [(tetra-overlap? t (first p))
         (pile-overlap? t (rest p))]
        [else #f]))


; tetra->pile: World -> World
; Moves the current tetra to the pile when it has reached the bottom
; OR touches another tetra in the pile 

(check-random (tetra->pile
               (make-world (make-tetra (make-posn 5 5) (list (make-brick 5 5 'green)))
                           (list (make-tetra (make-posn 5 5) (list (make-brick 5 5 'green))))))
              (make-world (generate-piece (random 7))
                          (list (make-tetra (make-posn 5 5) (list (make-brick 5 5 'green)))
                                (make-tetra (make-posn 5 5) (list (make-brick 5 5 'green))))))
(check-random (tetra->pile
               (make-world (make-tetra (make-posn 4 4) (list (make-brick 4 4 'green)))
                           (list (make-tetra (make-posn 4 4) (list (make-brick 4 4 'green))))))
              (make-world (generate-piece (random 7))
                          (list (make-tetra (make-posn 4 4) (list (make-brick 4 4 'green)))
                                (make-tetra (make-posn 4 4) (list (make-brick 4 4 'green))))))
                                                                         
(define (tetra->pile w)
  (make-world (generate-piece (random 7)) (cons (world-tetra w) (world-pile w))))
        
; IN BOUNDS FUNCTION ---------------------------------------------------------------------

; pile-not-in-bounds?: World -> Boolean
; Checks if the pile out of bounds

(check-expect (pile-out-of-bounds?
               (make-world
                (make-tetra (make-posn START-X START-Y) (list (make-brick START-X START-Y 'green)))
                (list (make-tetra (make-posn 21 21) (list (make-brick 21 21 'red)))))) #t)
(check-expect (pile-out-of-bounds?
               (make-world
                (make-tetra (make-posn START-X START-Y) (list (make-brick START-X START-Y 'green)))
                (list (make-tetra (make-posn 5 5) (list (make-brick 5 5 'red)))))) #f)

(define (pile-out-of-bounds? w)
  (cond [(empty? (world-pile w)) #f]
        [(>= (brick-y (first (tetra-bricks (first (world-pile w))))) 19) #t]
        [else #f]))

; VALID? FUNCTION ---------------------------------------------------------------------

; valid?: World -> Boolean
; Determines if a world has its tetra in the grid and if there is any overlap

(check-expect (valid?
               (make-world (make-tetra (make-posn 5 5) (list (make-brick 5 5 'blue)))
                           (list (make-tetra (make-posn 5 5) (list (make-brick 5 5 'blue)))))) #f)
(check-expect (valid?
               (make-world (make-tetra (make-posn 11 11) (list (make-brick 11 11 'blue)))
                           (list (make-tetra (make-posn 11 11) (list (make-brick 11 11 'blue)))))) #f)
(define (valid? w)
  (and
   (in-grid-world? w)
   (world-overlap? w)))

; SCORING FUNCTION ---------------------------------------------------------------------

; count-bricks: World -> Natural
; Counts the number of bricks on-screen in the pile

(define W8 (make-world
            (make-tetra
             (make-posn 5 5) (list (make-brick 5 5 'green)))
            (list (make-tetra (make-posn 3 3)
                              (list (make-brick 3 3 'red) (make-brick 4 4 'red)
                                    (make-brick 4 5 'red) (make-brick 4 6 'red)))
                  (make-tetra (make-posn 8 8)
                              (list (make-brick 8 8 'red) (make-brick 8 9 'red)
                                    (make-brick 8 7 'red) (make-brick 8 9 'red))))))
(define W4 (make-world
            (make-tetra
             (make-posn 5 5) (list (make-brick 5 5 'green)))
            (list (make-tetra (make-posn 3 3)
                              (list (make-brick 3 3 'red) (make-brick 4 4 'red)
                                    (make-brick 4 5 'red) (make-brick 4 6 'red))))))

(check-expect (count-bricks W8) 8)
(check-expect (count-bricks W4) 4)

(define (count-bricks w)
  (* 4 (length (world-pile w))))

; KEY AND BIG BANG ---------------------------------------------------------------------

; key-handler: World KE -> World
; Key Handler function 

(check-expect (key-handler (make-world
                            (make-tetra (make-posn 5 5) (list (make-brick 3 3 'green))) '()) "left")
              (make-world
               (make-tetra (make-posn 4 5) (list (make-brick 2 3 'green)))
               '()))
(check-expect (key-handler (make-world
                            (make-tetra (make-posn 5 5) (list (make-brick 3 3 'green))) '()) "a")
              (make-world
               (make-tetra (make-posn 5 5) (list (make-brick 7 3 'green)))
               '()))

(define (key-handler w ke)
  (cond [(key=? "left" ke)
         (if (valid?
              (make-world (move-left-tetra (world-tetra w)) (world-pile w)))
             (make-world (move-left-tetra (world-tetra w)) (world-pile w))
             w)]
        [(key=? "right" ke)
         (if (valid? (make-world (move-right-tetra (world-tetra w)) (world-pile w)))
             (make-world (move-right-tetra (world-tetra w)) (world-pile w))
             w)]
        [(key=? "a" ke)
         (if (valid? (make-world (tetra-rotate-ccw (world-tetra w)) (world-pile w)))
             (make-world (tetra-rotate-ccw (world-tetra w)) (world-pile w))
             w)]
        [(key=? "s" ke)
         (if
          (valid?
           (make-world
            (tetra-rotate-ccw (tetra-rotate-ccw (tetra-rotate-ccw (world-tetra w))))
            (world-pile w)))
          (make-world
           (tetra-rotate-ccw (tetra-rotate-ccw (tetra-rotate-ccw (world-tetra w))))
           (world-pile w))
          w)]
        [else w]))

; generate-piece: Number -> Tetra
; Generates a tetra at the start of the game  

(check-expect (generate-piece 2) (l-tet START-X START-Y))
(check-expect (generate-piece 0) (o-tet START-X START-Y))

(define (generate-piece n)
  (cond [(= n 0) (o-tet START-X START-Y)]
        [(= n 1) (i-tet START-X START-Y)]
        [(= n 2) (l-tet START-X START-Y)]
        [(= n 3) (j-tet START-X START-Y)]
        [(= n 4) (t-tet START-X START-Y)]
        [(= n 5) (z-tet START-X START-Y)]
        [(= n 6) (s-tet START-X START-Y)]))
                                                                                   
; BIG-BANG

(define START-GAME (make-world (generate-piece (random 7)) '()))

(big-bang
    START-GAME
  [on-key key-handler]
  [on-tick next-world TICK-RATE]
  [on-draw world->scene]
  [stop-when pile-out-of-bounds?])

  