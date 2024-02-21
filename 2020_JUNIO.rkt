(require mzlib/compat racket/function)
; 1. Base       : listaAtomos(S-expr): si Sexpr es atomo --> S-expr
;                 listaAtomos(()) = 0
; 2. Recurrencia: S-expr no es un átomo => S-expr = cons(car(S-expr), cdr(S-expr))
;    Hipótesis  : H1 = listaAtomos(car(S-expr)) y
;                 H2 = listaAtomos(cdr(S-expr))
;    Tesis      : listaAtomos(S-expr) = cons(H1,list(H2))

;(define (listaAtomos S-expr)
;  (cond ([null? S-expr] 0)
;        ([atom? S-expr] S-expr)
;        (else(cons(listaAtomos(car S-expr))
;                      (list(listaAtomos(cdr S-expr)))))))




; RECURSION
;1. Base       : listaAtomos(S-expr) = si S-expr es atomo
;                                      entonces list(S-expr)
;              : listaAtomos(()) = ()
;2. Recurrencia: S-expr no es un atomo, S-expr = cons(car(S-expr), cdr(S-expr))
;     Hipotesis: se supone conocido H1 = listaAtomos(car(S-expr))
;                                   H2 = listaAtomos(cdr(S-expr))
;         Tesis: listaAtomos(S-expr) = append(H1,H2)
(define (listaAtomos s-expr)
  (cond ((null? s-expr) ())
        ((atom? s-expr) (list s-expr))
        (else (append (listaAtomos (car s-expr)) (listaAtomos (cdr s-expr))))))


(displayln "Ejercicio 1: ")
(listaAtomos '(a 4 ((4)) (c 7 (2 4)))) ;--> (a 4 4 c 7 2 4)
(listaAtomos '((a 4 () ((5 (4 ())))) (h 7 (2 4)))) ;--> (a 4 5 4 h 7 2 4)
(listaAtomos '(a 4 ((5 (4))) (h 7 (2 4)))) ;--> (a 4 5 4 h 7 2 4))
(newline)

;Dada la lista datos que se define posteriormente, proporcionar una expresión FOS que retorne todos los pares 
;formados por el átomo simbólico y el mayor de sus valores asociados. Es decir, la lista que se indica a continuación:
(define datos '((x 2.3 9.8 3.5 7.5 2.15 8.3)
                (y 3.5 3.6 9.75 9.4 3.45 7.2)
                (z 7.2 8.4 7.25 14.8 13.4 2.4)
                (u 8.3 2.3 7.6 8.4 9.2 3.45)
                (v 3.5 6.8 7.9 7.25 2.3 7.7)
                (w 3.25 9.8 2.3 10.2 9.75 9.4)))
(displayln "Ejercicio 2: ")
;(filter(lambda(x)(quote (caar x))(> ))datos)
(map (lambda (x) (list (car x) (apply max (cdr x)))) datos)
(newline)


;proporcionar una expresión FOS que retorne una lista de átomos con todos los valores numéricos 
;de la lista datos y en el mismo orden en que aparecen 
(displayln "Ejercicio3: ")
;(list(filter(not(lambda(x)(quote (cadr x)))datos)))
(apply append (map (lambda (x) (filter number? x)) datos))
(newline)


;Dada la lista datos que se define posteriormente, definir mediante FOS la función listaDatos(atomo,lista) 
;que dado un átomo y una lista de estructura análoga a la lista datos, retorne:
;Si atom se encuentra en lista, la lista de números asociados.
;Si atomo no se encuentre en lista, la lista vacía.
(displayln "Ejercicio 4: ")
;(define (listaDatos atomo lista)
;  (cond ((filter (lambda(x)(equal? (quote (cadr x)) atomo)) lista)(cadr lista))
;        (else(not(filter(lambda(x)(equal? (quote (cadr x)) atomo))lista))'())))

(define (listaDatos atomo lista)
  (cond ((not (null? (filter (lambda(x) (eq? (car x) atomo)) lista)))
         (filter number? (car (filter (lambda(x) (member atomo x)) lista))))
        (else ())))
(listaDatos 'x datos) ;=> (2.3 9.8 3.5 7.5 2.15 8.3)
(listaDatos 'u datos) ;=> (8.3 2.3 7.6 8.4 9.2 3.45)
(listaDatos 't datos) ;=> ()
(newline)


;(define (listaDatos atomo lista)
;  (car (map (lambda (x)(if (member atomo x) (append (cdr x)) ())) lista)))


;Dada la lista listaNombres definida posteriormente de nombres de personas representadados como listas de átomos, 
;proporcionar una expresión FOS que retorne todos los nombres con menos de siete caracteres.
(define listaNombres'(('n 'i 'e 'v 'e 's)
                      ('h 'i 'p 'o 'l 'i 't 'a)
                      ('r 'o 's 'a)
                      ('v 'i 'c 't 'o 'r 'i 'a)
                      ('a 'n 'a)))

(filter(lambda(x)(< (length x) 7)) listaNombres)



