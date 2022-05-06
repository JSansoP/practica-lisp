; Práctica 1 -  Llenguatges de Programació (2721)

; Alumnes: 
;   - Joan Sansó Pericás
;   - Joan Vilella Candia
;   - Julián Wallis Medina

; Professors:
;   - Dr. Ramon Mas
;   - Dra. Xisca Roig

; Hem duit a terme la primera pràctica de l'assignatura, que consisteix en 
; programar un entorn de dibuix de figures 3D.
; Hem implementat la part 1 i la part 2. A més hem afegit dues figures extra,
; La pirámide i un diamant. Totes les figures han estat calculades pels membres
; de l'equip.
; a la funció de animar la rotació hem afegit una rotació sobre l'eix z a més
; de les demanades per la pràctica, per rotar sobre l'eix z, s'empreran les
; tecles w i s.

(defun inicia-patrons()
    (putprop 'cub '((-0.5 -0.5 0.5) (-0.5 -0.5 -0.5) (0.5 -0.5 -0.5) (0.5 -0.5 0.5) (-0.5 0.5 0.5) (-0.5 0.5 -0.5) (0.5 0.5 -0.5) (0.5 0.5 0.5)) 'punts)
    (putprop 'cub '((1 2) (2 3) (3 4) (1 4) (5 6) (6 7) (7 8) (5 8) (1 5) (2 6) (3 7) (4 8)) 'arestes)
    (putprop 'cub '((1 2 3 4) (5 6 7 8) (4 8 9 12) (1 5 9 10) (2 6 10 11) (11 12 3 7)) 'cares)

    (putprop 'octaedre '((0 -1 0) (-0.5 0 0.5) (-0.5 0 -0.5) (0.5 0 -0.5) (0.5 0 0.5) (0 1 0)) 'punts)
    (putprop 'octaedre '((1 2) (1 3) (1 4) (1 5) (2 3) (2 5) (4 5) (4 3) (2 6) (3 6) (4 6) (5 6)) 'arestes)
    (putprop 'octaedre '((1 4 8) (1 2 5) (2 3 6) (3 4 7) (8 9 12) (5 9 10) (6 10 11) (7 11 12)) 'cares)    ;inicialitzam figures
    
    (putprop 'prisma '((0 -1 0) (0 -1 -1) (1 -1 0) (0 1 0) (0 1 -1) (1 1 0)) 'punts)
    (putprop 'prisma '((1 2) (2 3) (1 3) (1 4) (2 5) (3 6) (4 5) (5 6) (4 6)) 'arestes)
    (putprop 'prisma '((3 4 6 9) (1 4 5 7) (2 5 6 8) (3 4 6 9) (7 8 9)) 'cares)

    (putprop 'piramide '((0 1 0) (-0.5 0 0.5) (-0.5 0 -0.5) (0.5 0 -0.5) (0.5 0 0.5)) 'punts)
    (putprop 'piramide '((1 2) (1 3) (1 4) (1 5) (2 3) (2 5) (4 5) (4 3)) 'arestes)
    (putprop 'piramide '((1 4 8) (1 2 5) (2 3 6) (3 4 7) (2 3 4 5)) 'cares)    ;inicialitzam figures

    (putprop 'diamant '((0 -0.5 0) (1 0.5 0) (0.5 0.5 0.86602) (-0.5 0.5 0.86602) (-1 0.5 0) (-0.5 0.5 -0.86602) (0.5 0.5 -0.86602) (0.75 0.75 0) (0.375 0.75 0.649519) (-0.375 0.75 0.649519) (-0.75 0.75 0) (-0.375 0.75 -0.649519) (0.375 0.75 -0.649519)) 'punts)
    (putprop 'diamant '((1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (2 3) (3 4) (4 5) (5 6) (6 7) (7 2) (2 8) (3 9) (4 10) (5 11) (6 12) (7 13) (8 9) (9 10) (10 11) (11 12) (12 13) (13 8)) 'arestes)
    (putprop 'diamant '((1 2 7) (2 3 8) (3 4 9) (4 5 10) (5 6 11) (1 6 12) (13 7 14 19) (14 20 15 8) (9 15 16 21) (10 16 17 22) (11 17 18 23)  (13 18 12 24) (19 20 21 22 23 24)) 'cares)    ;inicialitzam figures

    (putprop 'icosaedre '((0 1 1.61803) (0 1 -1.61803) (0 -1 1.61803) (0 -1 -1.61803) (1 1.61803 0) (1 -1.61803 0) (-1 1.61803 0) (-1 -1.61803 0) (1.61803 0 1) (1.61803 0 -1) (-1.61803 0 1) (-1.61803 0 -1)) 'punts)
    (putprop 'icosaedre '((11 3) (3 1) (11 1) (11 8) (8 3) (8 6) (6 3) (6 9) (9 3) (9 1) (9 5) (1 5) (1 7) (11 7) (11 12) (8 12) (8 4) (6 4) (6 10) (9 10) (5 7) (7 12) (12 4) (4 10) (10 5) (10 2) (5 2) (2 7) (2 12) (2 4)) 'arestes)
    (putprop 'icosaedre '((2 9 10) ( 10 11 12) ( 12 13 21) (13 14 3) (3 2 1) ( 23 17 16) (17 6 18) (18 19 24) (5 6 7) (24 26 30) (29 30 23) (7 8 9) (8 20 19) (20 11 25) (25 26 27) (21 28 27) (28 29 22) (22 14 15) (15 16 4) (4 1 5)) 'cares)
    
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
    (color 0 0 0)
)

(defun translacio(dx dy dz)
    (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list dx dy dz 1))
)

(defun escalat(ex ey ez)
    (list (list ex 0 0 0) (list 0 ey 0 0) (list 0 0 ez 0) (list 0 0 0 1))
)

(defun rotax(a)
    (list (list 1 0 0 0) (list 0 (cos (radians a)) (- 0 (sin (radians a))) 0) (list 0 (sin (radians a)) (cos (radians a)) 0) (list 0 0 0 1))
)

(defun rotay(a)
    (list (list (cos (radians a)) 0 (- 0 (sin (radians a))) 0) (list 0 1 0 0) (list (sin (radians a)) 0 (cos (radians a)) 0) (list 0 0 0 1))
)

(defun rotaz(a)
    (list (list (cos (radians a)) (- 0 (sin (radians a))) 0 0) (list (sin (radians a)) (cos (radians a)) 0 0) (list 0 0 1 0) (list 0 0 0 1))
)

(defun trasllada-figura(f x y z)
    (putprop f (multMatrix (get f 'matriu) (translacio x y z)) 'matriu)
    
)

(defun rota-figura(f x y z)
    (putprop f (multMatrix (multMatrix (multMatrix (get f 'matriu) (rotax x)) (rotay y)) (rotaz z)) 'matriu)
)

(defun escala-figura (f x y z)
    (putprop f (multMatrix (get f 'matriu) (escalat x y z)) 'matriu)
)

(defun inicia-figura(f)
    (putprop f (identitat) 'matriu)
)
;convert from degrees to radians
(defun radians(a)
    (* a (/ pi 180))
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
   (move (+ 320 (realpart (round (car (multVecMatrix (snoc 1 (agafa (car aresta) (get (get f 'patro) 'punts))) (get f 'matriu))))))
        (+ 187 (realpart (round (cadr (multVecMatrix (snoc 1 (agafa (car aresta) (get (get f 'patro) 'punts))) (get f 'matriu)))))))
        
    (draw (+ 320 (realpart (round (car (multVecMatrix (snoc 1 (agafa (cadr aresta) (get (get f 'patro) 'punts))) (get f 'matriu))))))
            (+ 187 (realpart (round (cadr (multVecMatrix (snoc 1 (agafa (cadr aresta) (get (get f 'patro) 'punts))) (get f 'matriu))))))
    )
)

(defun borra-figura (f)
    (cls-figura f)
    (putprop 'escena (borra-element f (get 'escena 'figura)) 'figura)
)

;defun paint each element of a list
(defun pinta-figures ()
    (cls)
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

;Funció que retorna la transposada d'una matriu
(defun transpose (matrix)
    (cond ((null matrix) nil)
        ((null (car matrix)) nil)
        (t (cons (cars matrix) (transpose (cdrs matrix))))))

;Funció que anima una figura
(defun animacio (f)
    (print 'animacio)
    (setq key (get-key))
    (cond ((equal key 114) (anima-rotacio f))
        ((equal key 116) (anima-translacio f))
        ((equal key 101) (anima-escalat f))
        ((equal key 113) (pinta-figura f))
        (T (anima-rotacio f))
    )
)
;Funció que anima la rotació d'una figura depenent de l'input de l'usuari
(defun anima-rotacio (f)
    (print 'rotacio)
    ;Print the points of the figure
    (print (car (get f 'punts)))
    
    ;(putprop f (+ 0.5 (cadar (get f 'punts)) 'punts))
    (setq key (get-key))
    (cond 
        ((equal key 331) (rota-figura f 0 -5 0) (cls f) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix X (esquerra)
        ((equal key 333) (rota-figura f 0 5 0) (cls f) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix X (dreta)
        ((equal key 328) (rota-figura f 5 0 0) (cls f) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix Y (amunt)
        ((equal key 336) (rota-figura f -5 0 0) (cls f) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix Y (abaix)
        ((equal key 119) (rota-figura f 0 0 5) (cls f) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix Z (w)
        ((equal key 115) (rota-figura f 0 0 -5) (cls f) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix Z (s)
        ((equal key 43) (putprop f (+ 0.5 (cadar (get f 'punts)) 'punts)) (cls f) (pinta-figura f) (anima-rotacio f))
        ((equal key 45) (putprop f (- 0.5 (cadar (get f 'punts)) 'punts)) (cls f) (pinta-figura f) (anima-rotacio f))


        ((equal key 100) (print 100) (anima-rotacio f)) 

        ;Change first point from figure

        ((equal key 113) (animacio f)) 
        (T (anima-rotacio f))
    )
)

;Funció que anima la translació d'una figura depenent de l'input de l'usuari
(defun anima-translacio (f)
    (print 'translacio)
    (setq key (get-key))
    (cond 
        ((equal key 331) (trasllada-figura f -5 0 0) (cls f) (pinta-figura f) (anima-translacio f))
        ((equal key 333) (trasllada-figura f 5 0 0) (cls f) (pinta-figura f) (anima-translacio f))
        ((equal key 328) (trasllada-figura f 0 5 0) (cls f) (pinta-figura f) (anima-translacio f))
        ((equal key 336) (trasllada-figura f 0 -5 0 ) (cls f) (pinta-figura f) (anima-translacio f)) 
        ((equal key 113) (animacio f)) 
        (T  (anima-translacio f))
    )
)

;Funció que anima l'escalat d'una figura depenent de l'input de l'usuari
(defun anima-escalat (f)
    (print 'escalat)
    (setq key (get-key))
    (cond 
        ((equal key 331) (escala-figura f 2 2 2) (cls f) (pinta-figura f) (anima-escalat f))
        ((equal key 333) (escala-figura f 0.5 0.5 0.5) (cls f) (pinta-figura f) (anima-escalat f))
        ((equal key 113) (animacio f)) 
        (T (anima-escalat f))
    )
)

(inicia-patrons)