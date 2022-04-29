(defun inicia-patrons()
    (putprop 'cub '((-0.5 -0.5 0.5) (-0.5 -0.5 -0.5) (0.5 -0.5 -0.5) (0.5 -0.5 0.5) (-0.5 0.5 0.5) (-0.5 0.5 -0.5) (0.5 0.5 -0.5) (0.5 0.5 0.5)) 'punts)
    (putprop 'cub '((1 2) (2 3) (3 4) (1 4) (5 6) (6 7) (7 8) (5 8) (1 5) (2 6) (3 7) (4 8)) 'arestes)
    (putprop 'cub '((1 2 3 4) (5 6 7 8) (4 8 9 12) (1 5 9 10) (2 6 10 11) (11 12 3 7)) 'cares)

    (putprop 'octaedre '((0 -1 0) (-0.5 0 0.5) (-0.5 0 -0.5) (0.5 0 -0.5) (0.5 0 0.5) (0 1 0)) 'punts)
    (putprop 'octaedre '((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (4 5) (2 5) (2 6) (3 6) (4 6) (5 6)) 'arestes)
    (putprop 'octaedre '((1 4 8) (1 2 5) (2 3 6) (3 4 7) (8 9 12) (5 9 10) (6 10 11) (7 11 12)) 'cares)    ;inicialitzam figures
    
    (putprop 'prisma '((0 -1 0) (0 -1 -1) (1 -1 0) (0 1 0) (0 1 -1) (1 1 0)) 'punts)
    (putprop 'prisma '((1 2) (2 3) (1 3) (1 4) (2 5) (3 6) (4 5) (5 6) (4 6)) 'arestes)
    (putprop 'prisma '((3 4 6 9) (1 4 5 7) (2 5 6 8) (3 4 6 9) (7 8 9)) 'cares)

    (putprop 'escena nil 'figures)
    (putprop 'vars 1 'comptador)  
)

(defun crea-figura(nom patro color)
    (putprop nom patro 'patro)
    (putprop nom color 'color)
    (putprop nom (identitat) 'matriu)
    (putprop 'escena (cons nom (get 'escena 'figura)) 'figura)
)

;sera una copia de pintar-figura pero con color = background color  
(defun cls-figura (f)
    (color 0 0 0)
    (pinta-cares (get (get f 'patro) 'cares) f)
)

(defun pinta-figura(f)
    (eval (cons 'color (get f 'color)))
    (pinta-cares (get (get f 'patro) 'cares) f)
)

(defun pinta-cares(cares f)
    (cond ((null cares) nil)
            (t (pinta-arestes (car cares) f) (pinta-cares (cdr cares) f)))
)



(defun pinta-arestes(cara f)
    (cond ((null cara) nil)
            (t (pinta-aresta (agafa (car cara) (get (get f 'patro) 'arestes)) f) (pinta-arestes (cdr cara) f))
    )
)

(defun pinta-aresta (aresta f)
   (print (list (+ 320 (realpart (round (car (multVecMatrix (snoc 1 (agafa (car aresta) (get (get f 'patro) 'punts))) (get f 'matriu))))))
        (+ 187 (realpart (round (cadr (multVecMatrix (snoc 1 (agafa (car aresta) (get (get f 'patro) 'punts))) (get f 'matriu))))))))
        
    (print (list (+ 320 (realpart (round (car (multVecMatrix (snoc 1 (agafa (cadr aresta) (get (get f 'patro) 'punts))) (get f 'matriu))))))
            (+ 187 (realpart (round (cadr (multVecMatrix (snoc 1 (agafa (cadr aresta) (get (get f 'patro) 'punts))) (get f 'matriu))))))
    ))
    (print (get 'vars 'comptador))
    (putprop 'vars (+ 1 (get 'vars 'comptador)) 'comptador)
)

(defun borra-figura (f)
    (cls-figura f)
    (putprop 'escena (borra-element f (get 'escena 'figura)) 'figura)
)

;defun paint each element of a list
(defun pinta-figures ()
    (pinta-figures-recursive (get 'escena 'figura))
)

(defun pinta-figures-recursive (l)
    (cond ((null l) nil)
            ((null (cdr l)) (pinta-figura (car l)))
            (t (pinta-figura (car l)) (pinta-figures-recursive (cdr l))))
)

(defun borra-figures()
    (cls)
    (putprop 'escena nil 'figura)
)

;remove element from list
(defun borra-element(x l)
    (cond   ((null l) nil)
            ((equal x (car l))  (cdr l))
            (t (cons (car l) (borra x (cdr l))))
    )
)

;create 4x4 identity matrix
(defun identitat()
    '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))
)

(defun snoc(x l)
    (cond   ((null l) (cons x l))
            (t (cons (car l) (snoc x (cdr l))))
    )
)

; coge el enesimo elemento de la lista
(defun agafa (n list)
    (cond ((null list) nil)
            ((= n 1) (car list))
            (t (agafa (- n 1) (cdr list))))
)

;FIXEAR MULT (NO SABEM A QUIN NIVELL PETA)
(defun mult(m1 m2)  
    (multMatrix m1 (transpose m2))
)

(defun multMatrix(m1 m2)
    (cond ((null m1) nil)
        (t (cons (multVecMatrix (car m1) m2) (multMatrix (cdr m1) m2)))
)
)

(defun multVecMatrix(v m)
    (cond ((null m) nil)
        (t (cons (pescalar v (car m)) (multVecMatrix v (cdr m)))))
)


;scalar product of two vectors
(defun pescalar (v1 v2)
    (cond ((null v1) 0) 
    (t (+ (* (car v1) (car v2)) (pescalar (cdr v1) (cdr v2))))) 
)

(defun escala(x l)
    (cond   ((null l) nil)
            (t (cons (* x (car l)) (escala x (cdr l))))
    )
)

(defun cars (matrix)
    (if (null matrix)
        nil
        (cons (car (car matrix)) (cars (cdr matrix))))
        )

(defun cdrs (matrix)
    (if (null matrix)
        nil
        (cons (cdr (car matrix)) (cdrs (cdr matrix))))
        )

(defun transpose (matrix)
    (cond ((null matrix) nil)
        ((null (car matrix)) nil)
        (t (cons (cars matrix) (transpose (cdrs matrix))))))





(inicia-patrons)