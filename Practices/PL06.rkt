(require mzlib/compat racket/function)

;Ejercicio 1


(display "diferenciasim: ")

(define (simultaneo e A B)
  (if [and (member e A) (member e B)]
      #t
      #f))


  
       

;(diferenciasim '(1 2 3) '(3 4 5)) ; => (1 2 4 5)
;(diferenciasim '(1 2 3) '(4 5)) ; => (1 2 3 4 5)
;(diferenciasim '(1 2 3) '(3 2 1)) ; => ()

;Ejercicio 2

;(map max '(1 2) '(3 4)) => (3 4)


(display "maxmin: ")


(define (maxmin . listas)
  (apply max (map(lambda (x) (apply min x)) listas)))

(maxmin '(1 4) '(2 3) '(5 7)); => 5
(maxmin '(3 4 5) '(5 7 8) '(9 2 10)); => 5

;Ejercicio 3


  

(display "raiz: ")
;(raiz 2 9 16 25) ;=> (3 4 5)
;(raiz 3 8 27 64) ;=> (2.0 3.0 3.9999999999999996)

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


(display "+dpsparse: ")

;(filter (lambda (x) (< dps (cadr x))) raid)) Filtra para mayor dps del parametro
;;(filter (lambda (x) (< parse (caddr x))) raid)) Filtra para mayor %parse del parametro

        
(define (+dpsparse dps parse raid)
  (filter (lambda (x) (and (< dps (cadr x)) (< parse (caddr x)))) raid))

(+dpsparse 8000 50 (cadr Raids)) ; => ((Machinist 9560 81) (Reaper 8976 77))
;(+dpsparse 10000 50 (cadr Raids)) ; => ()

(define (encontrarclases c1 c2 raid)
  (filter (lambda (x) (or (equal? c1 (car x)) (equal? c2 (car x)))) raid))

(display "encontrarclases: ")
(encontrarclases 'Paladin 'DarkKnight (cadddr Raids)) ; => ((Paladin 6377 80) (DarkKnight 6649 81))
(encontrarclases 'Paladin 'DarkKnight (car Raids)) ; => ()


(display "buscarparse: ")


(define (auxiliar parse raid)
  (if [null? (filter (lambda (x) (equal? parse (caddr x))) raid)]
      '()
      (filter (lambda (x) (equal? parse (caddr x))) raid)))
  

(auxiliar 81 (cadddr Raids)) 

(define (buscarparse parse raids)
  (map (curry auxiliar parse) raids))
   
(buscarparse 81 Raids) ; => ((Reaper 9546 81) (Machinist 9560 81) (DarkKnight 6649 81) (Sage 5257 81))
;(buscarparse 98 Raids) ; => ()