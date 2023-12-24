;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ampel) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; data definitions

(define-struct transition [from to])
; A Transition is a [Tlcolor Tlcolor]
; If the current state is equal to from-state, the Transition
; takes the current state to to-state
#;
(define (fn-on-tr tr)
  (...(fn-on-tlcolor (transition-from tr))
      ... (fn-on-tlcolor (transition-to tr))))


; A FiniteStateMachine (FSM) is one of
;     - '()
;     - (cons Transition FSM)
#;
(define (fn-on-fsm fsm)
  (cond
    [(empty? fsm) ...]
    [...(fn-on-tr (first fsm)) ... (fn-on-fsm (rest fsm))]))


(define-struct state+fsm [state fsm])
; A State+FSM is a [Tlcolor FSM]
; Clusters the current state with the governing FSM for use in MVC
#;
(define (fn-on-spf spf)
  (...(fn-on-tlcolor (state+fsm-state spf))
      ... (fn-on-fsm (state+fsm-fsm spf))))


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
  (... color))



; constants


(define FSM-AMPEL (list (make-transition STOP GO) (make-transition GO CAUTION)
                        (make-transition CAUTION STOP)))
(define BULBRAD 50) ; light bulb radius



;functions

(define (main spf)
  ; State+FSM -> State+FSM
  ; iterate the traffic light, for ever
  (big-bang spf
    [on-tick color-change 1]
    [to-draw ampelmacher]
    #;[on-mouse terminate]
    #;[stop-when finished?]))


(define (color-change spf)
  ; State+FSM -> State+FSM
  ; change the color of the light in a systematic way
  (make-state+fsm
   (change-color (state+fsm-state spf) (state+fsm-fsm spf))
   (state+fsm-fsm spf)))
; checks
(check-expect (color-change (make-state+fsm STOP FSM-AMPEL))
              (make-state+fsm GO FSM-AMPEL))


(define (ampelmacher spf)
  ; State+FSM -> Img
  ; produce an image of a well-functioning traffic light
  (illuminate-ampel (state+fsm-state spf)))
; checks
(check-expect (ampelmacher (make-state+fsm STOP FSM-AMPEL))
              (above (lightbulb STOP "solid")
                     (lightbulb CAUTION "outline")
                     (lightbulb GO "outline")))
(check-expect (ampelmacher (make-state+fsm CAUTION FSM-AMPEL))
              (above (lightbulb STOP "outline")
                     (lightbulb CAUTION "solid")
                     (lightbulb GO "outline")))
(check-expect (ampelmacher (make-state+fsm GO FSM-AMPEL))
              (above (lightbulb STOP "outline")
                     (lightbulb CAUTION "outline")
                     (lightbulb GO "solid")))


(define (change-color state fsm)
  ; Tlcolor FSM -> Tlcolor
  ; given a tlcolor and an fsm, return a new tlcolor according to fsm rules
  (cond
    [(empty? fsm) #f]
    [(state=? state (first fsm))
     (transition-to (first fsm))]
    [else (change-color state (rest fsm))]))
; checks
(check-expect (change-color STOP FSM-AMPEL) GO)
(check-expect (change-color CAUTION FSM-AMPEL) STOP)
(check-expect (change-color GO FSM-AMPEL) CAUTION)


(define (state=? current tr)
  ; Tlcolor Transition -> Boolean
  ; determine if the current state matches
  ; the from-state of the given transition
  (equal? current (transition-from tr)))
; checks


(define (illuminate-ampel color)
  ; State+Tlcolor -> Img
  ; light 'em up
  (above (lightbulb STOP (if (equal? STOP color) "solid" "outline"))
         (lightbulb CAUTION (if (equal? CAUTION color) "solid" "outline"))
         (lightbulb GO (if (equal? GO color) "solid" "outline"))))
  

(define (lightbulb color impact)
  ; Tlcolor String -> Img
; a helper function that generates colored bulbs in on or off state
  (overlay (circle BULBRAD impact color) (square (* 3 BULBRAD) "solid" "black")))
; checks
(check-expect (lightbulb STOP "outline") (overlay (circle BULBRAD "outline" STOP) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb STOP "solid") (overlay (circle BULBRAD "solid" STOP) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb CAUTION "outline") (overlay (circle BULBRAD "outline" CAUTION) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb CAUTION "solid") (overlay (circle BULBRAD "solid" CAUTION) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb GO "outline") (overlay (circle BULBRAD "outline" GO) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb GO "solid") (overlay (circle BULBRAD "solid" GO) (square (* 3 BULBRAD) "solid" "black"))) ; checks







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

(main (make-state+fsm STOP FSM-AMPEL))