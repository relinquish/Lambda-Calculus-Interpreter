Lambda-Calculus-Interpreter
===========================

This is a lambda calculus interpreter written in LISP (Common Lisp).  
It uses an evaluation strategy that is neither call-by-value nor call-by-name but lies somewhere in between.
The idea behind this type of evaluation strategy is that if an expression is found that will never be evaluated, there is no need to compute it and it can be skipped over.

Many common data types are already encoded within this interpreter such as booleans, boolean operations (if-then-else, is-zero, etc), church numerals, arithmetic operations (plus, minus, factorial, etc.)
A complete list of all built in shortcuts is found below:

+ true 
+ false
+ and
+ or
+ not
+ identity
+ Comb_K
+ Comb_S
+ Comb_B
+ Comb_C
+ Comb_W
+ Comb_Y,
+ Omega
+ +
+ 1+
+ 1-
+ *
+ ^
+ IsZero
+ IfThenElse
+ Facthelper
+ Fact

The interpreter is started by calling (lambda-frontend).

Some example executions are found below:

? (fact 3)
(LAM (F) (LAM (X) (F (F (F (F (F (F X))))))))

? (fact 2)
(LAM (F) (LAM (X) (F (F X))))

? (+ 1 1)
(LAM (F X) (F ((LAM (F X) (F X)) F X)))

? (+ 2 2)
(LAM (F X) (F (F ((LAM (F X) (F (F X))) F X))))
