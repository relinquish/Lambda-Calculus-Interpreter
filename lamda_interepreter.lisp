
;;;;;;;;;;;;;;;;;;;;;Front end
(defun lambda-frontend ()
    (terpri)(terpri)(princ "? ")
    (start-counter)
    (print (lameval (read)))
    (format t "~% Computed in ~D steps"  c)
    (lambda-frontend) )

(defun lameval (e) (de-alpha-convert (beta-reduce (alpha-convert (expand e)))))

;;;;;;;;;;;;;;;;;;;;;Substitution
(defun sub (lamexp x e) 
    (cond 
        ((atom lamexp) (if (equal lamexp x) e lamexp))
        ((equal (first lamexp) 'lam) 
            (if (member x (second lamexp)) 
                lamexp 
                (list 'lam (second lamexp) (sub (third lamexp) x e))
            )
        )
        (T (mapcar (lambda (y) (sub y x e)) lamexp))
    )
)


;;;;;;;;;;;;;;;;;;;;;Free Variables
(defun fv (e) 
    (cond
        ((atom e) (list e))
        ((equal (first e) 'lam) (list-subtract (second e) (fv (third e))))
        (T (reduce #'union (mapcar #'fv e)))
    )
)

(defun filter (test arg) 
    (if (atom arg) arg
        (if (funcall test (first arg)) 
            (cons (first arg) (filter test (cdr arg))) 
            (filter test (cdr arg))
        )
    )
)

(defun list-subtract (filterList bigList)
    (filter (lambda (x) (not (member x filterList))) bigList)
)


;;;;;;;;;;;;;;;;;;;;;Alpha-Conversion

(defun alpha-convert-worker (e)
    (cond
        ((atom e) e)
        ((equal (first e) 'lam)
            (
                (lambda (pairList)
                    (list 'lam
                          (mapcar #'cdr pairlist)
                          (reduce (lambda (wExpr pair) (sub wExpr (car pair) (cdr pair)))
                                  pairList
                                  :initial-value (alpha-convert-worker (third e))
                          )
                    )
                )
                (mapcar (lambda (x)
                            (
                             (lambda (y) (progn (setf (get y 'name) x) (cons x y)))
                             (gentemp)
                            )
                        )
                        (second e)
                )
            )
        )
        (T (mapcar #'alpha-convert-worker e))
    )
)

(defun pre-alpha-convert (e)
    (reduce (lambda (wExpr pair) (sub wExpr (car pair) (cdr pair)))
            (mapcar (lambda (x)
                        (
                         (lambda (y) (progn (setf (get y 'name) x) (cons x y)))
                         (gentemp)
                        )
                    )
                    (fv e)
            )
            :initial-value e
    )
)

(defun alpha-convert (e) (alpha-convert-worker (pre-alpha-convert e)))


(defun de-alpha-convert (e)
    (cond
        ((atom e) (get e 'name))
        ((equal (first e) 'lam)
            (list 'lam (mapcar #'de-alpha-convert (second e))
(de-alpha-convert (third e))))
        (T (mapcar #'de-alpha-convert e))
    )
)

;;;;;;;;;;;;;;;;;;;;;CHURCHIFICATION
(DEFUN CHURCH (Z)
    (LIST 'LAM '(f x) (CHURCH-WORKER Z)))

(DEFUN CHURCH-WORKER (Z)
    (COND
        ((ZEROP Z) 'x)
        ((EQ Z 1) '(f x))
        (T (LIST 'f (CHURCH-WORKER (1- Z))))
       )    )

;;;;;;;;;;;;;;;;;;;;;EXPAND

(defun expand (e)
  (cond
    ((atom e) (cond ((eq e 'true)  '(lam (t f) t))
                    ((eq e 'false) '(lam (t f) f))
                    ((eq e 'and)   '(lam (x y) (x y x)))
                    ((eq e 'or)    '(lam (x y) (x x y)))
                    ((eq e 'not)   '(lam (x y z) (x z y)))                    
                    ((eq e 'identity)     '(lam (x)   x))
                    ((eq e 'Comb_K)     '(lam (x) (lam (y) x)))
                    ((eq e 'Comb_S)     '(lam (x y z) (x z (y z))))
                    ((eq e 'Comb_B)     '(lam (x y z) (x (y z))))
                    ((eq e 'Comb_C)     '(lam (x y z) (x z y)))
                    ((eq e 'Comb_W)     '(lam (x y) (x y y)))
                    ((eq e 'Comb_Y)     '(lam (f) ((lam (x) (f (x x))) (lam (x) (f (x x))))) )
                    ((eq e 'omega)     '(lam (x) (x x)))
                    ((eq e '+)     '(lam (m n f x) (m f (n f x))))
                    ((eq e '1+)    '(lam (n f x) (f (n f x))))
                    ((eq e '1-)    '(lam (n f x) (n (lam (g h) (h (g f))) (lam(u) x)(lam(u) u))) )
                    ((eq e '*)     '(lam (m n f) (m (n f))))
                    ((eq e '^)     '(lam (m n) (n m)))
                    ((eq e 'ISZERO)     '(lam (n) (n (lam(v) (lam (t f) f)) (lam (t f) t)) ) )
                    ((eq e 'ifthenelse)     '(lam (a b c) (((a)b)c)) )
                    ((eq e 'facthelper)     `(lam (f n) (  (,(expand 'ifthenelse) (,(expand 'iszero) n)   ,(expand 1)   (,(expand '*) n (f (,(expand '1-) n))))   )) )
                    ((eq e 'fact)     `( ,(expand 'Comb_Y) ,(expand 'facthelper) ) )
                    ((numberp e)    (church e))
                    (T              e)) )

    ((equal (first e) 'lam)  (list 'lam (second e) (expand (third e))))
    (T  (mapcar #'expand e))
  )
)


(defun start-counter () (setq c -1))
(defun increment-counter () (setq c (1+ c)))


;;;;;;;;;;;;;;;;;;;;;Beta-Reduce
(defun isvar (x)  (atom x) )
(defun isfunc (x) (and (not (isvar x)) (equal (length x) 3) (equal (car x) 'lam)))

(defun variables (x) (second x))

(defun body (x) (third x))


(defun beta-reduce (E)
    (increment-counter)
    (cond ((atom E) E)
          ((eq (first E) 'lam) (list 'lam (variables E) (beta-reduce (body E))))
          ((eq (length E) 1) (beta-reduce (car E)))
          (T ((lambda (firstTerm)
                (cond
                    ((not (isfunc firstTerm)) (cons firstTerm (cdr E)))
                    ;First case: x1 is used in the body of the function, but is only parameter
                    ((and (member (first (variables firstTerm)) (fv (body firstTerm)))
                          (eq (length (variables firstTerm)) 1))
                     (beta-reduce (cons (sub (body firstTerm)
                                             (first (variables firstTerm))
                                             (beta-reduce (cadr E)))
                                        (cddr E))))
                    ;Second case: x1 is used in body of the function, and there are more parameters
                    ((and (member (first (variables firstTerm)) (fv (body firstTerm)))
                          (> (length (variables firstTerm)) 1))
                     (beta-reduce
                        (cons (list 'lam
                                    (cdr (variables firstTerm))
                                    (sub (body firstTerm)
                                         (first (variables firstTerm))
                                         (beta-reduce (cadr E)))
                              )
                              (cddr E))))
                    ;Third case: x1 not used in body, is the only parameter
                    ((eq (length (variables firstTerm)) 1);already know first var not in fv of body
                     (beta-reduce (cons (body firstTerm) (cddr e)))
                    )
                    ;Fourth case: x1 not used in body, and there are more parameters
                    ((> (length (variables firstTerm)) 1)
                     (beta-reduce
                        (cons (list 'lam
                                    (cdr (variables firstTerm))
                                    (body firstTerm)
                              )
                              (cddr E))))
                )
              )
              (beta-reduce (first E))
             )
          )
    )
)
