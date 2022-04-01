;11/03/2022
(defun pertanyer(x l)
	(cond 	((null l) nil)
		((equal x (car l) ) t)
		(t (pertanyer x (cdr l)))
	)
)

(defun exp(m n)
	(cond 	((= n 0) 1)
		(t (* m (exp m (- n 1))))
	)
)

(defun fib(x)
	(cond 	((= x 0) 0)
		((= x 1) 1)
		(t (+ (fib(- x 1)) (fib(- x 2))))
	)
)

(defun dividir(m n)
	(cond 	((> n m) 0)
		(t (+ 1 (dividir(- m n) n)))
	)
)

(defun parell_vostro(n)
	(= (dividir n 2) (dividir (+ n 1) 2))
)

(defun parell(n)
	(= n (* (dividir n 2) 2))	
)

;18/03/2022
(defun senars(l)
    (cond   ((null l) nil)
            ((evenp (car l)) (senars (cdr l)))
            (t (cons (car l) (senars (cdr l))))
    )
)

(defun borra(x l)
    (cond   ((null l) nil)
            ((equal x (car l))  (cdr l))
            (t (cons (car l) (borra x (cdr l))))
    )
)

; Contrario rdc
(defun rdc(l)
    (cond   ((null (cdr l)) nil)
            (t (cons (car l) (rdc(cdr l))))       
    )
)

; Contrario de cons
(defun snoc(x l)
    (cond   ((null l) (cons x l))
            (t (cons (car l) (snoc x (cdr l))))
    )
)

(defun escala(x l)
    (cond   ((null l) nil)
            (t (cons (* x (car l)) (escala x (cdr l))))
    )
)

(defun maxim(l) ;Meu
    (cond   ((null (cdr l)) (car l))
            ((< (car l) (cadr l)) (maxim (cdr l)))
            (t (maxim (cons (car l) (cddr l))))
    )
)

(defun minim(l) ;Meu
    (cond   ((null (cdr l)) (car l))
            ((> (car l) (cadr l)) (minim (cdr l)))
            (t (minim (cons (car l) (cddr l))))
    )
)

(defun maxim(l) ;Andreu Sureda
    (cond   ((null (cdr l)) (car l))
            ((>= (car l) (maxim (cdr l)) (car l)) )
            (t (maxim (cdr l)))
    )
)

(defun minim(l) ;Andreu Sureda
    (cond   ((null (cdr l)) (car l))
            ((<= (car l) (minim (cdr l)) (car l)) )
            (t (minim (cdr l)))
    )
)

(defun ordena(l) ; es borra només ha d'esborrar la primera aparició del numero
    (cond   ((null l) nil)
            ((null (cdr l)) l)
            (t (cons (minim l) (ordena(borra (minim l) l))))
    )
)

