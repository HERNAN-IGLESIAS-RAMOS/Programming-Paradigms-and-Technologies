;***** PL-05
;;; Completar la definición de la función add(n1,n2) que se proporciona
;;; parcialmente en el archivo pl-05.rkt. Dados dos números enteros positivos
;;; representados mediante la lista de sus dígitos, la evaluación de esta
;;; función retorna la suma de ambos con la misma representación.
;;;
;;; A tal fin proporcionar la definición de la función recursiva auxiliar
;;; add-aux(n1,n2) que recibe los números con los dígitos en orden inverso
;;; y obtiene la suma también con los dígitos en orden inverso, del menos
;;; significativo al más significativo.

(define (add n1 n2)
  (letrec [(add-aux (lambda(n1 n2)
                      (cond ([null? n1] n2)
                            ([null? n2] n1)
                            ([< (+ (car n1) (car n2))
                                10]
                             (cons (+ (car n1) (car n2))
                                   (add-aux (cdr n1) (cdr n2))))
                            (else
                             (cons (- (+ (car n1) (car n2))
                                      10)
                                   (add-aux '(1) (add-aux (cdr n1) (cdr n2))))))))]
    (reverse (add-aux (reverse n1)
                      (reverse n2)))))

;***** PL-05
;;; Definir la función previous(x,l) de forma que si la S-expresión x es un elemento de la
;;; lista l, retorne la lista de todos los elementos de l desde el primero hasta la última
;;; ocurrencia de x en l (esta incluida). En caso contrario, retornará la lista vacía.
(define (previous x l)
  (let ([found (member x (reverse l))])
    (if found
        (reverse found)
        ())))
(previous '(a) '((a) (a) 9 (a)))



;***** PL-05
;;; Definir mediante FOS la función compare(num1, num2) que dadas dos números enteros representados
;;; mediante la lista de sus dígitos, retorna -1, 0, o 1, según sea num1<num2, num1=num2, o num1>num2
;;; respectivamente. Supuesto que num1 y num2 son listas de la misma longitud.
(define (compare num1 num2)
  (let ([l (filter (compose not zero?)
                   (map - num1 num2))])
    (cond ([null? l] 0)
          ([positive? (car l)] 1)
          (else -1))))

;;; El símbolo %usoInternet-16a74 es una lista de datos de las comunidades autónomas que muestra
;;; la proporción de personas (de 16 a 74 años )que utilizan Internet en cada comunidad entre los
;;; años 2021 y 2015
;;;			                    Porcentajes de uso de Internet
;;; Comunidad autónoma		2021	2020	2019	2018	2017	2016	2015
(define %usoInternet-16a74
  '((Andalucía            	92.8	92.4	89.4	84.8	83.9	78.8	74.1)
    (Aragón               	94.8	94.2	91.8	89.2	89.8	83.9	79.8)
    (Asturias             	92.6	91.1	89.2	85.7	82.3	76.7	78.3)
    (Illes_Balears        	95.1	94.3	94.1	89.8	88.5	81.7	82.6)
    (Canarias             	93.3	91.6	89.7	84.6	83.5	78.7	75.7)
    (Cantabria            	92.5	91.7	89.1	82.3	82.7	80.1	78.8)
    (Castilla-La_Mancha   	92.7	92.7	87.2	80.5	78.3	78.0	74.3)
    (Castilla-León        	92.1	90.7	88.6	82.5	81.3	77.0	77.6)
    (Cataluña             	95.5	95.7	93.7	88.0	85.7	82.8	83.1)
    (Ceuta                	94.0	95.3	94.9	85.9	81.4	74.7	82.9)
    (Comunitat_Valenciana 	94.8	93.1	89.7	86.3	84.0	78.4	77.1)
    (Extremadura          	90.9	91.6	88.6	82.5	80.2	75.7	72.6)
    (Galicia              	90.2	87.4	84.0	80.4	79.4	74.6	71.9)
    (Comunidad_de_Madrid  	95.9	96.0	94.1	91.0	90.0	86.9	85.9)
    (Melilla              	97.4	96.7	87.6	88.8	88.0	79.4	74.3)
    (Región_Murcia        	94.8	90.6	89.8	85.7	84.5	79.1	78.0)
    (Navarra              	93.6	95.4	95.0	88.1	86.7	81.9	79.6)
    (País_Vasco           	93.3	93.4	91.5	86.3	85.7	84.8	81.5)
    (La_Rioja             	92.5	92.7	89.6	82.8	82.0	80.1	78.7)))

(displayln "%usoInternet-16a74: ")
%usoInternet-16a74
(displayln "")

;***** PL-05
;;; Definir el símbolo %usoEn2015"<" 75 como la lista de nombres de las comunidades autónomas
;;; que en el primer año registrado (2015) tuvieron un porcentaje de uso de Internet inferior
;; al 75.00%
(define %usoEn2015<75
  (map car
       (filter [lambda(com)
                 (< (car (reverse com)) 75.0)]
               %usoInternet-16a74)))

;***** PL-05
;;; Definir mediante FOS el símbolo %usoMaxAnual como la lista de los mayores porcentajes de uso
;;; de Internet en cada año entre 2021 y 2015
(define %usoMaxAnual
  (apply map max (map cdr %usoInternet-16a74)))

;***** PL-05
;;; Definir el símbolo %usoInternet-21a12, para anexionar los
;;; siguientes datos a %usoInternet-16a74
;;;                       Porcentajes de uso de Internet
;;; Comunidad Autónoma          2012    2013    2014
(define %usoInternet-12a14
  '((Andalucía                  69.7    73.1	72.8)
    (Aragón                     71.2    70.4    73.1)
    (Asturias             	73.3	74.8	76.5)
    (Illes_Balears        	82.6	81.7	83.8)
    (Canarias             	73.2	74.9	75.2)
    (Cantabria            	75.3	76.4	77.3)
    (Castilla-La_Mancha   	78.3	72.0	74.1)
    (Castilla-León        	72.1	72.7	75.4)
    (Cataluña             	85.7	82.8	83.1)
    (Ceuta                	71.4	75.6	79.7)
    (Comunitat_Valenciana 	74.0	75.6	75.3)
    (Extremadura          	68.2	70.8	70.6)
    (Galicia              	69.4	71.6	71.2)
    (Comunidad_de_Madrid  	80.6	83.1	84.7)
    (Melilla              	60.8	67.4	71.3)
    (Región_Murcia        	70.5	73.1	75.6)
    (Navarra              	68.4	71.7	74.3)
    (País_Vasco           	73.9	74.5	79.4)
    (La_Rioja             	68.7	71.8	76.5)))


(define %usoInternet-21a12
  (map (lambda(21a15 12a14)
         (cons (car 21a15)
               (append (cdr 21a15)
                       (reverse (cdr 12a14)))))
       %usoInternet-16a74
       %usoInternet-12a14))
