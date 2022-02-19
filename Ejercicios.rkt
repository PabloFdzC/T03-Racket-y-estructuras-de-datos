#lang racket

#|

La función duplicate consiste en que recibe una lista y devuelve
una lista con los elementos de la lista original duplicados.

El parámetro de entrada es la lista cuyos elementos se deben duplicar.

La salida es la lista original con sus elementos duplicados.

Ejemplo de uso:
(duplicate '(A B C D))
Lo que debería devolver:
'(A A B B C C D D)

|#

(define (duplicate l)
  (if (empty? l) '()
      ; Une el elemento duplicado con el resultado de usar recursión con el resto de la lista
     (append (list (car l) (car l)) (duplicate (cdr l)))))

(duplicate '(A B C D))

#|

La siguiente función es la de pack, que lo que hace es una recursión
de cola al llamar a la función packAux. Lo que hace esta función es
agrupar los elementos contiguos repetidos y colocarlos dentro de listas
diferentes dentro de una lista. Con el ejemplo se mostrará su funcionamiento.

El parámetro de entrada es la lista de la cual se desea que se agrupen sus
elementos contiguos repetidos.

La salida es la lista original pero con sus elementos contiguos repetidos
agrupados en diferentes listas.

Ejemplo de uso:
(pack '(A A A B B C D D D))
El resultado debe ser el siguiente:
'((A A A)(B B)(C)(D D D))

|#

(define (pack l)
  (if (empty? l) '()
      (packAux l '())))

; l2 es la sublista que se va formando con los elementos repetidos
(define (packAux l l2)
  (if (empty? l)
      (if (empty? l2) '()
          (list l2))
      (if (empty? l2)
          (packAux (cdr l) (list (car l)))
          (if(equal? (car l) (car l2))
             ; si se encuentra el elemento repetido se agrega a l2
             (packAux (cdr l) (append l2 (list (car l))))
             ; si el elemento no es repetido l2 se agrega a la lista resultado.
             (append (list l2) (packAux l '()))))))

(pack '(A A A B B C D D D))

#|

Encode también es una función que realiza recursión de cola al llamar
a la función encodeAux. Al igual que pack, agrupa a los elementos contiguos
repetidos, pero en este caso lo hace en pares que contienen el número de veces
que se repite el elemento y el elemento, de la siguiente manera: '(2 A), lo que
signigica que el elemento A se repite dos veces.

El parámetro de entrada es la lista que se desea que se agurpen sus elementos
contiguos repetidos en pares.

La salida es la lista con los elementos contiguos repetidos de la lista original
agrupados en pares.

Ejemplo de uso:
(encode '(A A B B B B C D D D))
Lo que debería resultar en:
'((2 A) (4 B) (1 C) (3 D))

|#

(define (encode l)
  (if (empty? l) '()
      (encodeAux l '())))

; l2 es el par que se va formando con un elementos
(define (encodeAux l l2)
  (if (empty? l)
      (if (empty? l2) '()
          (list l2))
      (if (empty? l2)
          (encodeAux (cdr l) (list 1 (car l)))
          (if(equal? (car l) (second l2))
             ; si se encuentra un elemento igual se le suma 1 al par
             (encodeAux (cdr l) (list (+ (car l2) 1) (second l2)))
             ; si el elemento que se encuentra no es igual se agrega el par a la lista final.
             (append (list l2) (encodeAux l '()))))))

(encode '(A A B B B B C D D D))

#|

Decode hace lo contrario a la función encode, es decir, recibe una lista
con pares de la forma (2 A) y lo traduce a una lista con los elementos
repetidos la cantidad de veces que diga el par, por lo que (2 A) se
convertiría en (A A) y así con todos los pares.

El parámetro de entrada es la lista con los pares.

La salida es la lista de pares convertida en la lista con los elementos
repetidos.

Ejemplo de uso:
(decode '((3 A) (1 B) (5 C) (2 D)))

Lo cual debería dar como resultado:
'(A A A B C C C C C D D)

|#

(define (decode l )
  (if (empty? l) '()
      (if (= (car (car l)) 1)
          ; si el par que se encuentra tiene 1, se agrega el elemento a la lista resultante, se elimina el par y se hace recurisón
          (append (list(second (car l))) (decode (cdr l)))
          ; si no es 1 se le resta 1 al número del par, se agrega el elemento a la lista resultate y se hace recursión.
          (append (list(second (car l))) (decode (append (list(list (- (car (car l)) 1) (second (car l)))) (cdr l)))))))

(decode '((3 A) (1 B) (5 C) (2 D)))