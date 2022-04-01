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
)
; create 3d figure with color and inicia patrons
(defun crea-figura(nom patro color)
    (putprop nom patro 'patro)
    (putprop nom color 'color)
    (putprop nom (identitat) 'matriu)
    (putprop 'escena (cons nom (get 'escena 'figura)) 'figura)
)



;sera una copia de pintar-figura pero con color = background color
(defun cls-figura (f)

)

(defun pinta-figura(f)

)

(defun borra-figura (f)
    (cls-figura f)
    (putprop 'escena (borra-element f (get 'escena 'figura)) 'figura)
)

;defun paint each element of a list
(defun pinta-figures ()
    (if (null (get 'escena 'figura))
        nil
        (pinta-figura (car (get 'escena 'figura)))
        (pinta-figures (cdr (get 'escena 'figura)))
    )
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

(inicia-patrons)