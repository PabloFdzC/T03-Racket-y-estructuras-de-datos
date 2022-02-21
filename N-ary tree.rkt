#lang racket
#|
Crea un nodo
Parámetros:
- id: se usa para identificar al nodo
- name: es el nombre del nodo
- value: es el valor que tiene el nodo
Salida:
- Se devuelve una lista con dos listas, la primera
- corresponde a la información del nodo y la segunda
- se usa para albergar a sus hijos.
Ejemplo:
(node 1 "a" "a") devuelve '((1 "a" "a") '())
|#
(define (node id name value)
  (list (list id name value) '()))

#|
Crea una arista
Parámetros:
- newNode: es el nodo de la arista
- weight: es el peso de la arista
Salida:
- Se devuelve una lista con dos listas, la primera
- corresponde a la información del nodo y la segunda
- se usa para albergar a sus hijos.
Ejemplo:
(edge (node 1 "a" "a") 10) devuelve '(((1 "a" "a") '()) 10)
|#
(define (edge newNode weight)
  (list newNode weight))

#|
Crea una arista
Parámetros:
- newNode: es el nodo de la arista
- weight: es el peso de la arista
Salida:
- Se devuelve una lista con dos listas, la primera
- corresponde a la información del nodo y la segunda
- se usa para albergar a sus hijos.
Ejemplo:
(list-all-nodes (insert-node (insert-node (insert-node (node 1 "a" "a") 1 2 "b" "b" 10) 1 3 "c" "c" 20) 3 4 "d" "d" 30))
devuelve ((1 a a) ((((2 b b) ()) 10) (((3 c c) ((((4 d d) ()) 30))) 20)))
|#
(define (list-all-nodes tree)
  (display tree))

#|
Encuentra un nodo en el árbol según el id
Parámetros:
- tree: el árbol que tiene la forma del nodo
-     explicado en la función node
- id: es el id del nodo a buscar
Salida:
- Se devuelve los datos del nodo
Ejemplo:
-(find-node (insert-node (insert-node (insert-node (node 1 "a" "a") 1 2 "b" "b" 10) 1 3 "c" "c" 20) 3 4 "d" "d" 30) 4)
-   devuelve '(4 "d" "d")
|#
(define (find-node tree id)
  (cond
    [(equal? (caar tree) id) (car tree)]
    [(= (length (cdr tree)) 0) null]
    [else
     (find-aux (cadr tree) id)
     ]
    ))
#|
Sirve para iterar los hijos de un nodo en la función find-node
Parámetros:
- lst: son los hijos de un nodo
- id: es el id del nodo a buscar
Salida:
- Se devuelve los datos del nodo o null
|#
(define (find-aux lst id)
  (cond
    [(empty? lst) null]
    [(empty? (find-node (caar lst) id))
      (find-aux (rest lst) id)]
    [else
     (find-node (caar lst) id)]
      ))

#|
Inserta un nodo nuevo en el árbol
Parámetros:
- tree: el árbol que tiene la forma del nodo
-     explicado en la función node
- parent: es el id del que queremos que sea el
-     padre del nodo nuevo
- id: es el id del nuevo nodo
- name: es el nombre del nuevo nodo
- value: es el valor que tiene el nuevo nodo
Salida:
- Se devuelve el nuevo árbol con el nodo insertado
Ejemplo:
-(insert-node (node 1 "a" "a") 1 4 "d" "d" 10)
-   devuelve '((1 "a" "a") ((((4 "d" "d") ()) 10)))
-
-(insert-node (insert-node (insert-node (node 1 "a" "a") 1 2 "b" "b" 10) 1 3 "c" "c" 20) 3 4 "d" "d" 30)
-   devuelve '((1 "a" "a") ((((2 "b" "b") ()) 10) (((3 "c" "c") ((((4 "d" "d") ()) 30))) 20)))
|#
(define (insert-node tree parent id name value weight)
  (cond
    [(equal? (caar tree) parent) (list (car tree) (append (cadr tree) (list (edge (node id name value) weight))))]
    [(= (length (cdr tree)) 0) tree]
    [else (list (car tree)
                (for/list ([elem (cadr tree)])
                  (edge (insert-node (car elem) parent id name value weight) (cadr elem))
                  )
                )]
    ))

#|
Elimina un nodo en el árbol según el id
en caso de que el nodo tenga hijos el primero
estos lo remplazará como raíz
Parámetros:
- tree: el árbol que tiene la forma del nodo
-     explicado en la función node
- id: es el id del nuevo nodo
Salida:
- Se devuelve el nuevo árbol con el nodo eliminado
Ejemplos:
-(delete-node (node 1 "a" "a") 1)
-   devuelve '()
-
-(delete-node (insert-node (insert-node (insert-node (node 1 "a" "a") 1 2 "b" "b" 10) 1 3 "c" "c" 20) 3 4 "d" "d" 30) 1)
-   devuelve '((2 "b" "b") ((((3 "c" "c") ((((4 "d" "d") ()) 30))) 20)))
|#

(define (delete-node tree id)
  (cond
    [(equal? (caar tree) id)
     (if (empty? (cadr tree))
         null
         (list (car (caaadr tree)) (append (cadr (caaadr tree)) (rest (cadr tree))))
         )
     ]
    [(= (length (cdr tree)) 0) tree]
    [else
     (list (car tree)
           (for/list
               ([elem (cadr tree)]
                #:unless (empty? (delete-node (car elem) id)))
             (edge (delete-node (car elem) id) (cadr elem))
             )
           )]
    ))


 #|
Encuentra un nodo en el árbol según el id
Parámetros:
- tree: el árbol que tiene la forma del nodo
-     explicado en la función node
- id: es el id del nodo a buscar
Salida:
- Se devuelve los datos del nodo que tenga como
- padre, si es la raíz devuelve una lista vacía
Ejemplo:
- (ancestor (insert-node (insert-node (insert-node (node 1 "a" "a") 1 2 "b" "b" 10) 1 3 "c" "c" 20) 3 4 "d" "d" 30) 4)
-   devuelve '(3 "c" "c")
|#
(define (ancestor tree id)
  (cond
    [(equal? (caar tree) id) null]
    [else
     (ancestor-aux (cadr tree) id (car tree))
     ]
    ))

#|
Sirve de ayuda para la función ancestor
Parámetros:
- lst: son los hijos de un nodo
- id: es el id del nodo a buscar
- parent: es el padre de la lista de nodos
-   que se están revisando
Salida:
- Se devuelve los datos del nodo o null
|#
(define (ancestor-aux lst id parent)
  (cond
    [(empty? lst) null]
    [(equal? (caaaar lst) id)
      parent]
    [else
     (if (empty? (ancestor-aux (rest lst) id parent))
         (ancestor-aux (cadr (caar lst)) id (caaar lst))
         (ancestor-aux (rest lst) id parent))]
      ))