;===============================================================================
;           Práctica 1 -  Llenguatges de Programació (2721)                    =
;===============================================================================

                        ;======== ALUMNES ========;
                        ;   Joan Sansó Pericàs    ;
                        ;   Joan Vilella Candia   ;
                        ;   Julián Wallis Medina  ;
                        ;==========================

                        ;====== PROFESSORS =======;
                        ;   Dr. Ramon Mas Sansó   ;
                        ;   Dra. Xisca Roig Maimó ;
                        ;=========================;

;===============================================================================
; Hem duit a terme la primera pràctica de l'assignatura, que consisteix en
; programar un entorn de dibuix de figures 3D.

; Hem implementat la part 1 i la part 2. A més hem afegit tres figures extra,
; La pirámide, un diamant i un icosaedre. Totes les figures han estat calculades
; pels membresde l'equip. Recomanam augmentar la RAM del programa si es vol fer
; servir l'icosaedre i el diamant.

; A la funció de animar la rotació hem afegit una rotació sobre l'eix z a més
; de les demanades per la pràctica, per rotar sobre l'eix z, s'empreran les
; tecles W i S.
;
; Hem dividit l'arxiu en 5 parts, depenent del tipus de funció.
;   - Part 1: Funcions principals
;   - Part 2: Funcions per pintar
;   - Part 3: Funcions d'animació
;   - Part 4: Funcions de transformacions
;   - Part 5: Funcions auxiliars
;===============================================================================



;===============================================================================
;======================     FUNCIONS PRINCIPALS     ============================
;===============================================================================


;===============================================================================
; Funció que s'encarrega de definir els átoms de les figures a través dels punts,
; arestes i cares.
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

    (putprop 'diamant '((0 0 0) (1 1 0) (0.5 1 0.86602) (-0.5 1 0.86602) (-1 1 0) (-0.5 1 -0.86602) (0.5 1 -0.86602) (0.75 1.25 0) (0.375 1.25 0.649519) (-0.375 1.25 0.649519) (-0.75 1.25 0) (-0.375 1.25 -0.649519) (0.375 1.25 -0.649519)(anima-rotacio f) ) 'punts)
    (putprop 'diamant '((1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (2 3) (3 4) (4 5) (5 6) (6 7) (7 2) (2 8) (3 9) (4 10) (5 11) (6 12) (7 13) (8 9) (9 10) (10 11) (11 12) (12 13) (13 8)) 'arestes)
    (putprop 'diamant '((1 2 7) (2 3 8) (3 4 9) (4 5 10) (5 6 11) (1 6 12) (13 7 14 19) (14 20 15 8) (9 15 16 21) (10 16 17 22) (11 17 18 23)  (13 18 12 24) (19 20 21 22 23 24)) 'cares)    ;inicialitzam figures

    (putprop 'icosaedre '((0 1 1.61803) (0 1 -1.61803) (0 -1 1.61803) (0 -1 -1.61803) (1 1.61803 0) (1 -1.61803 0) (-1 1.61803 0) (-1 -1.61803 0) (1.61803 0 1) (1.61803 0 -1) (-1.61803 0 1) (-1.61803 0 -1)) 'punts)
    (putprop 'icosaedre '((11 3) (3 1) (11 1) (11 8) (8 3) (8 6) (6 3) (6 9) (9 3) (9 1) (9 5) (1 5) (1 7) (11 7) (11 12) (8 12) (8 4) (6 4) (6 10) (9 10) (5 7) (7 12) (12 4) (4 10) (10 5) (10 2) (5 2) (2 7) (2 12) (2 4)) 'arestes)
    (putprop 'icosaedre '((2 9 10) (10 11 12) (12 13 21) (13 14 3) (3 2 1) (23 17 16) (17 6 18) (18 19 24) (5 6 7) (24 26 30) (29 30 23) (7 8 9) (8 20 19) (20 11 25) (25 26 27) (21 28 27) (28 29 22) (22 14 15) (15 16 4) (4 1 5)) 'cares)

    (putprop 'escena nil 'figures)
    (putprop 'vars 1 'comptador)
)

;===============================================================================
; Funció que donat el nom, el patró i el color d'una figura crea una de nova
; dins l'àtom escena.
(defun crea-figura(nom patro color)
    (putprop nom patro 'patro)
    (putprop nom color 'color)
    (putprop nom (identitat) 'matriu)
    (putprop 'escena (cons nom (get 'escena 'figura)) 'figura)
)

;===============================================================================
; Inicia la matriu identitat de la figura.
(defun inicia-figura(f)
    (putprop f (identitat) 'matriu)
)

;===============================================================================
; Borra la pantalla i la llista de figures de l'escena.
(defun borra-figures()
    (cls)
    (putprop 'escena nil 'figura)
)

;===============================================================================
;======================     FUNCIONS PER PINTAR     ============================
;===============================================================================

;===============================================================================
; Pinta totes les figures de l'escena.
(defun pinta-figures()
    (cls)
    (pinta-figures-recursive (get 'escena 'figura))
)

;===============================================================================
; Pinta les figures de la llista passada recursivament.
(defun pinta-figures-recursive (l)
    (cond
        ((null l) nil)
        ((null (cdr l)) (pinta-figura (car l)))
        (t (pinta-figura (car l)) (pinta-figures-recursive (cdr l)))
    )
)

;===============================================================================
; S'encarrega de pintar la figura. Deixa el color de la figura llest i crida a
; pinta-cares amb la llista de cares del patró de la figura.
(defun pinta-figura(f)
   (eval (cons 'color (get f 'color)))
    (pinta-cares (get (get f 'patro) 'cares) f)
    (color 0 0 0)
)

;===============================================================================
; Per a cada cara de la llista de cares, crida a pinta-arestes amb la llista
; d'arestes d'aquesta cara.
(defun pinta-cares(cares f)
    (cond
        ((null cares) nil)
        (t (pinta-arestes (car cares) f) (pinta-cares (cdr cares) f))
    )
)

;===============================================================================
; Per a cada aresta de la llista d'arestes, crida a pinta-aresta amb la llista de
; punts de l'aresta (la llista realment conté els índexos dels punts que formen
; l'aresta de la llista de punts de la figura).
(defun pinta-arestes(cara f)
    (cond
        ((null cara) nil)
        (t (pinta-aresta (agafa (car cara) (get (get f 'patro) 'arestes)) f) (pinta-arestes (cdr cara) f))
    )
)

;===============================================================================
; Ens passen una llista d'índexos de 2 punts de la figura, i per a cada punt
; calculam la seva posició multiplicant la coordenada per a la matriu de
; transformació de la figura. Movem el punter i dibuixam.
; Com que varem implementar multVecMatrix per a que la matriu d'entrada estigués
; transposada (per més facilitat alhora de multiplicar matriu per matriu), també
; hem de transposar la matriu perque el resultat de la multiplicació sigui
; correcte.
(defun pinta-aresta (aresta f)
   (move (+ 320 (realpart (round (car (multVecMatrix (snoc 1 (agafa (car aresta) (get (get f 'patro) 'punts))) (transpose (get f 'matriu)))))))
        (+ 187 (realpart (round (cadr (multVecMatrix (snoc 1 (agafa (car aresta) (get (get f 'patro) 'punts))) (transpose (get f 'matriu))))))))

    (draw (+ 320 (realpart (round (car (multVecMatrix (snoc 1 (agafa (cadr aresta) (get (get f 'patro) 'punts))) (transpose (get f 'matriu)))))))
            (+ 187 (realpart (round (cadr (multVecMatrix (snoc 1 (agafa (cadr aresta) (get (get f 'patro) 'punts))) (transpose (get f 'matriu)))))))
    )
)

;===============================================================================
; Fa el mateix que pinta figura però amb el color del fons enlloc del color de
; la figura.
(defun cls-figura (f)
    (color 255 255 255)
    (pinta-cares (get (get f 'patro) 'cares) f)
    (color 0 0 0)
)

;===============================================================================
; S'encarrega de esborrar la figura de la pantalla i de l'escena. Després
; d'esborrar un element de l'escena no es podrà tornar a animar, pintar,
; transformar, etc
(defun borra-figura (f)
    (cls-figura f)
    (putprop f nil 'matriu)
    (putprop f nil 'color)
    (putprop f nil 'patro)
    (putprop 'escena (borra-element f (get 'escena 'figura)) 'figura)
)


;===============================================================================
;======================     FUNCIONS ANIMACIÓ       ============================
;===============================================================================

;===============================================================================
;Funció que anima una figura
(defun animacio (f)
    (print 'animacio)
    (setq key (get-key))
    (cond
        ((equal key 114) (anima-rotacio f))
        ((equal key 116) (anima-translacio f))
        ((equal key 101) (anima-escalat f))
        ((equal key 113) (pinta-figura f))
        (t (anima-rotacio f))
    )
)


;===============================================================================
;Funció que anima la translació d'una figura depenent de l'input de l'usuari
(defun anima-translacio (f)
    (print 'translacio)
    (setq key (get-key))
    (cond
        ((equal key 331) (cls) (trasllada-figura f -5 0 0) (pinta-figura f) (anima-translacio f))
        ((equal key 333) (cls) (trasllada-figura f 5 0 0) (pinta-figura f) (anima-translacio f))
        ((equal key 328) (cls) (trasllada-figura f 0 5 0) (pinta-figura f) (anima-translacio f))
        ((equal key 336) (cls) (trasllada-figura f 0 -5 0 ) (pinta-figura f) (anima-translacio f))
        ((equal key 113) (cls) (animacio f))
        (t  (anima-translacio f))
    )
)

;===============================================================================
;Funció que anima l'escalat d'una figura depenent de l'input de l'usuari
(defun anima-escalat (f)
    (print 'escalat)
    (setq key (get-key))
    (cond
        ((equal key 331) (cls) (escala-figura f 2 2 2) (pinta-figura f) (anima-escalat f))
        ((equal key 333) (cls) (escala-figura f 0.5 0.5 0.5) (pinta-figura f) (anima-escalat f))
        ((equal key 113) (animacio f))
        (t (anima-escalat f))
    )
)

;===============================================================================
;Funció que anima la rotació d'una figura depenent de l'input de l'usuari
(defun anima-rotacio (f)
    (print 'rotacio)
    (setq key (get-key))
    (cond
        ((equal key 331) (cls) (rota-figura f 0 -5 0) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix X (esquerra)
        ((equal key 333) (cls) (rota-figura f 0 5 0)  (pinta-figura f) (anima-rotacio f)) ;rotacio en eix X (dreta)
        ((equal key 328) (cls) (rota-figura f 5 0 0)  (pinta-figura f) (anima-rotacio f)) ;rotacio en eix Y (amunt)
        ((equal key 336) (cls) (rota-figura f -5 0 0) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix Y (abaix)
        ((equal key 119) (cls) (rota-figura f 0 0 5)  (pinta-figura f) (anima-rotacio f)) ;rotacio en eix Z (w)
        ((equal key 115) (cls) (rota-figura f 0 0 -5) (pinta-figura f) (anima-rotacio f)) ;rotacio en eix Z (s)
        ((equal key 113) (animacio f))
        (t (anima-rotacio f))
    )
)

;===============================================================================
;====================       FUNCIONS DE TRANSFORMACIONS     ====================
;===============================================================================

;===============================================================================
; Trasllada la figura f en x, y i z unitats
(defun trasllada-figura(f x y z)
    (putprop f (mult (get f 'matriu) (translacio x y z)) 'matriu)
)

;===============================================================================
; Escla la figura f en x, y i z unitats
(defun escala-figura (f x y z)
    (putprop f (mult (get f 'matriu) (escalat x y z)) 'matriu)
)

;===============================================================================
; Rota la figura f en x, y i z unitats
(defun rota-figura(f x y z)
    (putprop f (mult (mult (mult (get f 'matriu) (rotax x)) (rotay y)) (rotaz z)) 'matriu)
)

;===============================================================================
; Retorna la matriu identitat amb els valors passats com a paràmetres, per a
; traslladar la figura en x, y i z unitats.
(defun translacio(dx dy dz)
    (list (list 1 0 0 0) (list 0 1 0 0) (list 0 0 1 0) (list dx dy dz 1))
)

;===============================================================================
; Retorna la matriu identitat amb els valors passats com a paràmetres, per a
; traslladar la figura en x, y i z unitats.
(defun escalat(ex ey ez)
    (list (list ex 0 0 0) (list 0 ey 0 0) (list 0 0 ez 0) (list 0 0 0 1))
)

;===============================================================================
; Retorna la matriu identitat amb els valors passats com a paràmetres, per a
; traslladar la figura en a unitats sobre l'eix x.
(defun rotax(a)
    (list (list 1 0 0 0) (list 0 (cos (radians a)) (- 0 (sin (radians a))) 0)
     (list 0 (sin (radians a)) (cos (radians a)) 0) (list 0 0 0 1))
)

;===============================================================================
; Retorna la matriu identitat amb els valors passats com a paràmetres, per a
; traslladar la figura en a unitats sobre l'eix y.
(defun rotay(a)
    (list (list (cos (radians a)) 0 (- 0 (sin (radians a))) 0) (list 0 1 0 0)
     (list (sin (radians a)) 0 (cos (radians a)) 0) (list 0 0 0 1))
)

;===============================================================================
; Retorna la matriu identitat amb els valors passats com a paràmetres, per a
; traslladar la figura en a unitats sobre l'eix z.
(defun rotaz(a)
    (list (list (cos (radians a)) (- 0 (sin (radians a))) 0 0)
     (list (sin (radians a)) (cos (radians a)) 0 0) (list 0 0 1 0) (list 0 0 0 1))
)


;===============================================================================
;======================     FUNCIONS AUXILIARS      ============================
;===============================================================================

;===============================================================================
; Crea una matriu identitat 4x4
(defun identitat()
    '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))
)

;===============================================================================
; Borra la primera instància de l'element passat a la llista passada.
(defun borra-element(x l)
    (cond
        ((null l) nil)
        ((equal x (car l))  (cdr l))
        (t (cons (car l) (borra-element x (cdr l))))
    )
)

;===============================================================================
; Agafa l'element de la llista amb l'índex indicat
(defun agafa (n list)
    (cond
        ((null list) nil)
        ((= n 1) (car list))
        (t (agafa (- n 1) (cdr list)))
    )
)

;===============================================================================
;convert from degrees to radians
(defun radians(a)
    (* a (/ pi 180))
)

;===============================================================================
;Funció que retorna la transposada d'una matriu
(defun transpose (matrix)
    (cond
        ((null matrix) nil)
        ((null (car matrix)) nil)
        (t (cons (cars matrix) (transpose (cdrs matrix))))
    )
)

;===============================================================================
; Multiplica la matriu m1 per la matriu transposada m2.
(defun mult(m1 m2)
    (multMatrix m1 (transpose m2))
)

;===============================================================================
; Multiplica la matriu m1 per la matriu m2. La matriu m2 ha d'estar transposada
; per a que funcioni correctament.
(defun multMatrix(m1 m2)
    (cond
        ((null m1) nil)
        (t (cons (multVecMatrix (car m1) m2) (multMatrix (cdr m1) m2)))
    )
)

;===============================================================================
; Multiplica la matriu m per el vector v.
(defun multVecMatrix(v m)
    (cond
        ((null m) nil)
        (t (cons (pescalar v (car m)) (multVecMatrix v (cdr m))))
    )
)

;===============================================================================
; Producte escalar de dos vectors.
(defun pescalar (v1 v2)
    (cond
        ((null v1) 0)
        (t (+ (* (car v1) (car v2)) (pescalar (cdr v1) (cdr v2))))
    )
)

;===============================================================================
; Donada una llista de llistes (matriu), retorna una llista amb el primer
; element de cada subllista
(defun cars (matrix)
    (cond
        ((null matrix) nil)
        (t (cons (car (car matrix)) (cars (cdr matrix))))
    )
)

;===============================================================================
; Donada una llista de llistes (matriu), retorna una llista de llistes on cada
; subllista conté tots els elements menys el primer.
(defun cdrs (matrix)
    (cond
        ((null matrix) nil)
        (t (cons (cdr (car matrix)) (cdrs (cdr matrix))))
    )
)

;===============================================================================
; Fa el cons al reves (Fica un element al final de la llista)
(defun snoc(x l)
    (cond
        ((null l) (cons x l))
        (t (cons (car l) (snoc x (cdr l))))
    )
)


(inicia-patrons)