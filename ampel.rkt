;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ampel) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)


; data definitions

(define-struct transition [from to])
; A Transition is a [State State]
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



;; State is one of
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


(define FSM-AMPEL (list (make-transition STOP GO)
                        (make-transition GO CAUTION)
                        (make-transition CAUTION STOP)))
(define BULBRAD 50) ; light bulb radius
(define COOLDOWN 512)



;functions

(define (simulate fsm init-state)
  ; FSM State -> State
  ; iterate the traffic light, for ever
  (local (
          (define (color-change state)
            ; State -> State
            ; change the color of the light in a systematic way
            (cond
              [(= (random COOLDOWN) 0)
               (local (
                       (define (find-in state fsm)
                         ; State FSM -> State
                         ; searches an fsm for a from state
                         ; that matches the current state
                         (cond
                           [(state=? state (first fsm))
                            (transition-to (first fsm))]
                           [else (find-in state (rest fsm))])))
                 (find-in state fsm))]
              [else state])))
    (big-bang init-state
      [on-tick color-change]
      [to-draw ampelmacher]
      #;[on-mouse terminate]
      #;[stop-when finished?])))


(define (ampelmacher state)
  ; State -> Img
  ; produce an image of a well-functioning traffic light
  (illuminate-ampel state))
; checks
(check-expect (ampelmacher STOP)
              (above (lightbulb STOP "solid")
                     (lightbulb CAUTION "outline")
                     (lightbulb GO "outline")))
(check-expect (ampelmacher CAUTION)
              (above (lightbulb STOP "outline")
                     (lightbulb CAUTION "solid")
                     (lightbulb GO "outline")))
(check-expect (ampelmacher GO)
              (above (lightbulb STOP "outline")
                     (lightbulb CAUTION "outline")
                     (lightbulb GO "solid")))


(define (state=? current tr)
  ; State Transition -> Boolean
  ; determine if the current state matches
  ; the from-state of the given transition
  (equal? current (transition-from tr)))
; checks


(define (illuminate-ampel color)
  ; State -> Img
  ; light 'em up
  (above (lightbulb STOP (if (equal? STOP color) "solid" "outline"))
         (lightbulb CAUTION (if (equal? CAUTION color) "solid" "outline"))
         (lightbulb GO (if (equal? GO color) "solid" "outline"))))
  

(define (lightbulb color impact)
  ; State String -> Img
  ; a helper function that generates colored bulbs in on or off state
  (overlay (circle BULBRAD impact color) (square (* 3 BULBRAD) "solid" "black")))
; checks
(check-expect (lightbulb STOP "outline") (overlay (circle BULBRAD "outline" STOP) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb STOP "solid") (overlay (circle BULBRAD "solid" STOP) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb CAUTION "outline") (overlay (circle BULBRAD "outline" CAUTION) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb CAUTION "solid") (overlay (circle BULBRAD "solid" CAUTION) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb GO "outline") (overlay (circle BULBRAD "outline" GO) (square (* 3 BULBRAD) "solid" "black"))) ; checks
(check-expect (lightbulb GO "solid") (overlay (circle BULBRAD "solid" GO) (square (* 3 BULBRAD) "solid" "black"))) ; checks


; (State, Int, Int, MouseEvent) -> State
; end cycle on mouseclick in ampel window
(check-expect (terminate CAUTION 20 200 "button-down") EXIT)
(check-expect (terminate CAUTION 20 200 "button-up") CAUTION)
(define (terminate color x y me)
  (cond [(mouse=? "button-down" me) EXIT]
        [else color]))


; State -> Boolean
; end when commanded to exit by user
(check-expect (finished? STOP) #false) ; checks
(check-expect (finished? CAUTION) #false) ; checks
(check-expect (finished? GO) #false) ; checks
(check-expect (finished? EXIT) #true) ; checks
(define (finished? color)
  (equal? EXIT color))



; actions!

(simulate FSM-AMPEL STOP)