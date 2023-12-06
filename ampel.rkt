;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ampel) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; constants
(define BULBRAD 50) ; light bulb radius
(define RED 1)
(define YELLOW 2)
(define GREEN 3)
(define BLUE 42) ; stop the world!


;functions;

; WORLDSTATE -> WORLDSTATE
; iterate the traffic light, for ever
(define (main color)
  (big-bang color
    [to-draw ampelmacher]
    [on-tick color-change 1]
    [on-mouse blueit]
    [stop-when finished?]
    ))

; TLCOLOR -> TLCOLOR
; change the color of the light in a systematic way
(check-expect (color-change RED) GREEN) ; checks
(check-expect (color-change YELLOW) RED) ; checks
(check-expect (color-change GREEN) YELLOW) ; checks
(define (color-change color)
  (cond [(equal? RED color) GREEN]
        [(equal? YELLOW color) RED]
        [(equal? GREEN color) YELLOW]))

; TLCOLOR -> IMG
; produce an image of a well-functioning traffic light
(check-expect (ampelmacher RED) (above (lightbulb "red" "solid") (lightbulb "yellow" "outline") (lightbulb "green" "outline")))
(check-expect (ampelmacher YELLOW) (above (lightbulb "red" "outline") (lightbulb "yellow" "solid") (lightbulb "green" "outline")))
(check-expect (ampelmacher GREEN) (above (lightbulb "red" "outline") (lightbulb "yellow" "outline") (lightbulb "green" "solid")))
(define (ampelmacher color)
  (above (lightbulb "red" (if (equal? RED color) "solid" "outline"))
          (lightbulb "yellow" (if (equal? YELLOW color) "solid" "outline"))
          (lightbulb "green" (if (equal? GREEN color) "solid" "outline"))))
  
; (TLCOLOR, STRING) -> IMG
; a helper function that generates colored bulbs in on or off state
(check-expect (lightbulb "red" "outline") (overlay (circle BULBRAD "outline" "red") (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb "red" "solid") (overlay (circle BULBRAD "solid" "red") (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb "yellow" "outline") (overlay (circle BULBRAD "outline" "yellow") (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb "yellow" "solid") (overlay (circle BULBRAD "solid" "yellow") (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb "green" "outline") (overlay (circle BULBRAD "outline" "green") (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb "green" "solid") (overlay (circle BULBRAD "solid" "green") (square (* 3 BULBRAD) "solid" "black"))) ; checks
(define (lightbulb color impact)
  (overlay (circle BULBRAD impact color) (square (* 3 BULBRAD) "solid" "black")))

; (TLCOLOR, INT, INT, STRING) -> TLCOLOR
; end cycle on mouseclick in ampel window
(define (blueit color x y me)
  (cond [(string=? "button-down" me) BLUE]
        [else color]))

; TLCOLOR -> BOOLEAN
; end when color is blue
(check-expect (finished? RED) #false) ; checks
(check-expect (finished? YELLOW) #false) ; checks
(check-expect (finished? GREEN) #false) ; checks
(check-expect (finished? BLUE) #true) ; checks
(define (finished? color)
  (equal? BLUE color))


; actions!

(main RED)