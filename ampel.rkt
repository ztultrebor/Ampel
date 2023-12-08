;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ampel) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; data definitions

;; Tlcolor is one of
;; - false
;; - String
;; represents any of the available traffic light colors, or false
(define STOP "red")
(define CAUTION "yellow")
(define GO "green")
(define EXIT #false) ; stop the world!
#;
(define (fn-for-tlcolor color)
  (... t))


; constants

(define BULBRAD 50) ; light bulb radius


;functions

; Tlcolor -> Tlcolor
; iterate the traffic light, for ever
(define (main color)
  (big-bang color
    [to-draw ampelmacher]
    [on-tick color-change 1]
    [on-mouse terminate]
    [stop-when finished?]
    ))

; Tlcolor -> Tlcolor
; change the color of the light in a systematic way
(check-expect (color-change STOP) GO) ; checks
(check-expect (color-change CAUTION) STOP) ; checks
(check-expect (color-change GO) CAUTION) ; checks
(define (color-change color)
  (cond [(equal? STOP color) GO]
        [(equal? CAUTION color) STOP]
        [(equal? GO color) CAUTION]))

; Tlcolor -> Img
; produce an image of a well-functioning traffic light
(check-expect (ampelmacher STOP) (above (lightbulb STOP "solid") (lightbulb CAUTION "outline") (lightbulb GO "outline")))
(check-expect (ampelmacher CAUTION) (above (lightbulb STOP "outline") (lightbulb CAUTION "solid") (lightbulb GO "outline")))
(check-expect (ampelmacher GO) (above (lightbulb STOP "outline") (lightbulb CAUTION "outline") (lightbulb GO "solid")))
(define (ampelmacher color)
  (above (lightbulb STOP (if (equal? STOP color) "solid" "outline"))
          (lightbulb CAUTION (if (equal? CAUTION color) "solid" "outline"))
          (lightbulb GO (if (equal? GO color) "solid" "outline"))))
  
; (Tlcolor, String) -> Img
; a helper function that generates colored bulbs in on or off state
(check-expect (lightbulb STOP "outline") (overlay (circle BULBRAD "outline" STOP) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb STOP "solid") (overlay (circle BULBRAD "solid" STOP) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb CAUTION "outline") (overlay (circle BULBRAD "outline" CAUTION) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb CAUTION "solid") (overlay (circle BULBRAD "solid" CAUTION) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb GO "outline") (overlay (circle BULBRAD "outline" GO) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb GO "solid") (overlay (circle BULBRAD "solid" GO) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(define (lightbulb color impact)
  (overlay (circle BULBRAD impact color) (square (* 3 BULBRAD) "solid" "black")))

; (Tlcolor, Int, Int, MouseEvent) -> Tlcolor
; end cycle on mouseclick in ampel window
(check-expect (terminate CAUTION 20 200 "button-down") EXIT)
(check-expect (terminate CAUTION 20 200 "button-up") CAUTION)
(define (terminate color x y me)
  (cond [(mouse=? "button-down" me) EXIT]
        [else color]))

; Tlcolor -> Boolean
; end when commanded to exit by user
(check-expect (finished? STOP) #false) ; checks
(check-expect (finished? CAUTION) #false) ; checks
(check-expect (finished? GO) #false) ; checks
(check-expect (finished? EXIT) #true) ; checks
(define (finished? color)
  (equal? EXIT color))


; actions!

(main STOP)