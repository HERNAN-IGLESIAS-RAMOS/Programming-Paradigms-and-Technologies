(require mzlib/compat racket/function)

;Ejercicio 1           

(define (diferencia l1 l2)
  (cond ((or (null? l1) (null? l2)) ())
  ((member (car l1) l2) (diferencia (cdr l1) l2))
  (else (cons (car l1) (diferencia (cdr l1) l2)))))
  
  
(display "diferencia: ")
(diferencia '(1 2 3) '(4 3 5)) ; => (1 2)
(diferencia '(1 2 3) '(4 5)) ; => (1 2 3)
(diferencia '(1 2 3) '(3 2 1)) ; => ()

;Ejercicio 2

(define (menor . resto)
  (apply min (map (curry apply min) resto)))

(display "menor: ")
(menor '(1 5) '(2 3) '(5 7)); => 1
(menor '(3 4 5) '(5 7 8) '(9 2 10)); => 2

;Ejercicio 3

(define (expt2e e . resto)
  (map (curryr expt (* 2 e)) resto))

(display "expt2e: ")
(expt2e 2 2 3 4 5) ;=> (16 81 256 625)
(expt2e 3 2 3 4) ;=> (64 729 4096)

;Ejercicio 4
(display "\nRaids: \n")
(define Raids
  '(((Ninja 10486 84) (Reaper 9546 81) (Dancer 7796 87) (Sage 5997 91) (Warrior 5419 29) (Gunbreaker 5317 31) (Bard 4847 22) (Scholar 3207 37))
    ((Machinist 9560 81) (Reaper 8976 77) (Ninja 7282 47) (Paladin 6127 42) (Warrior 5818 69) (WhiteMage 3458 48) (Machinist 8187 30) (Astrologian 1364 4))
    ((Reaper 10631 84) (RedMage 7185 48) (Dancer 7132 73) (Machinist 6761 20) (Astrologian 3981 71) (Gunbreaker 3952 7) (DarkKnight 3514 4) (Astrologian 1781 10))
    ((Dragoon 9029 82) (RedMage 7748 71) (Summoner 6756 27) (DarkKnight 6649 81) (Paladin 6377 80) (Machinist 5188 4) (Sage 4691 63) (WhiteMage 2027 6))
    ((Bard 9245 99) (Reaper 8949 70) (Dragoon 8937 77) (Summoner 6575 27) (Warrior 6522 67) (Gunbreaker 6306 62) (Astrologian 3077 64) (WhiteMage 2032 6))
    ((Samurai 10053 85) (Monk 8990 73) (Ninja 7717 56) (Summoner 6672 36) (DarkKnight 6146 63) (Warrior 6142 64) (Sage 5257 81) (WhiteMage 3900 49))
    ))

(define Raids2
  '((Ninja 10486 84) (Reaper 9546 81) (Dancer 7796 87) (Sage 5997 91) (Reaper 5419 29) (Gunbreaker 5317 31) (Bard 4847 22) (Scholar 3207 37))
    )

(define (+dps dps raid)
  ;(filter (lambda(x) (<= dps (cadr x))) raid))
  (filter (compose (curry <= dps) cadr) raid))
  
(display "+dps: ")
(+dps 7000 (cadr Raids)) ; => ((Machinist 9560) (Reaper 8976) (Ninja 7282) (Machinist 8187))
(+dps 10000 (cadr Raids)) ; => ()

(define (shieldhealer raid)
  (filter (lambda(x) (or (eq? 'Sage (car x)) (eq? 'Scholar (car x)))) raid))


(display "shieldhealer: ")
(shieldhealer (car Raids)) ; => ((Sage 5997 91) (Scholar 3207 37))
(shieldhealer (cadr Raids)) ; => ()

(define (buscarclase clase raid)
  (apply append (map (lambda(x) (filter (lambda(y) (eq? (car y) clase)) x))raid)))
 
(display "buscarclase: ")
(buscarclase 'Reaper Raids) ; => ((Reaper 9546 81) (Reaper 8976 77) (Reaper 10631 84) (Reaper 8949 70))
(buscarclase 'BlackMage Raids) ; => ()