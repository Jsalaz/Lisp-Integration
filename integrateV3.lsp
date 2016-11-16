;;;  Jorge Salazar
;;;  CSC345: Paragidms 
;;;  Professor R. Wyatt
;;;  Project 1: Integration

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Common Variables
;;		F  = function
;;		V  = variable
;;		hi = high interval range
;;		lo = logh interval range
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Main Functions
;;		INTEGRATE: 	Takes in a function (F), variable (V), and optional (hi) and (low) 
;;					intervals. Determines if the funtion is a definite or indefinite 
;;					integral and send it to the appropriate aux-function.
;;		1	Checks if value for hi or lo are nil 
;;		2	If true then continue to INDEF-INTEGRAL function
;;		3 	Otherwise continue to DEF-INTEGRAL

(defun integrate (F V &optional lo hi)
  (cond ((or (equal hi nil) (equal lo nil))				;; 1
         (indef-integral F V))						;; 2 
        (t (def-integral (indef-integral F V) V lo hi))))		;; 3 
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 		INDEF-INTEGRAL: Takes in a function (F), variable (V) and constructs 
;;						the indefinite integral.
;;		1 	Checks if the function is a number, if true constructs the integral
;;				with the make-mult function.
;;		2 	Checks if the function is a variable, if true constructs the integral
;;				with the make-div and make-pow functions.
;;		3	Checks if the function is a negation lists such as (- x) and (- - x) 
;;				If true then it recursively calls indef-integral with a negated
;;				function. Then negates the result of the integral. 
;;		4 	Checks if the function is a sum. If true then it calls the make-sum
;;				function with a recursible call of indef-integral with each operands
;;				of the function. 
;;		5 	Checks if the function is a difference. If true then it calls the 
;;				make-diff function with a recursible call of indef-integral with 
;;				each operand of the function.
;;		6	Checks if the function has an exponent and if the power is greater or 
;;				equal to 0. If true then it constructs the integral with the make-div
;;				and make-pow functions. Making sure to increase the second operand 
;;				by 1 in both the make-pow and make-div functions.
;;		7	Checks if the function has an exponent and if the power is -1. If true
;;				then it constructs the integral with the make-log function.

(defun indef-integral (F V)
  (cond ((numberp F)(make-mult F V))   							;; 1
        ((var-pred F)(make-div (make-pow F 2) 2))				;; 2
        ((negation-pred F) 										;; 3
         (make-negation (indef-integral (make-negation F) V)))
        ((sum-pred F) 											;; 4 
         (make-sum (indef-integral (sum-operand-1 F) V)
                   (indef-integral (sum-operand-2 F) V)))
        ((diff-pred F) 											;; 5
         (make-diff (integrate (diff-operand-1 F) V)
                    (integrate (diff-operand-2 F) V)))
        ((and (pow-pred F)										;; 6
              (>= (pow-operand-2 F) 0)) 
         (make-div (make-pow (pow-operand-1 F)(1+ (pow-operand-2 F)))
                   (1+ (pow-operand-2 F))))
        ((and (pow-pred F)										;; 7
              (= (pow-operand-2 F) -1))
         (make-log (pow-operand-1 F)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 		DEF-INTEGRAL:	Takes the function constructed by INDEF-INTEGRAL (F), 
;;						the variable (V), and the hi and lo intervals. Then 
;;						evaluates the result by replacing the variables with
;;						the hi and lo interval values and takes the difference.

(defun def-integral (F V lo hi)
  (eval (make-diff (my-replace V hi F)
                   (my-replace V lo F))))

;;;===========================================================================
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; MY-REPLACE: switches element e1 with e2 if it in list L
        ;; 1    Checks if L is a list, wasn't necessary for this project
        ;; 2    Checks if L is at the end of the list
        ;; 3    Equal checks if the first element of L is the same as e1
        ;;         If true the e2 replaces the first element and recursively calls
        ;;         my-replace with e1, e2 and the rest of L
        ;; 4    Checks if the first element of L is a list
        ;;         If true it concatenates the recursive call of e1, e2 with
        ;;         the first of L
        ;; 5       with the recursive call of e1, e2 with the rest of L
        ;; 6    If no condition is true then the function concatenates the first of L
        ;;         and recursively calls e1, e2 with the rest of L
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
(defun my-replace (e1 e2 L)
  (cond ;((not (listp L)) nil)                                          ;; 1
        ((var-pred L) e2)
        ((endp L) nil)                                                  ;; 2
        ((equal (first L) e1) (cons e2 (my-replace e1 e2 (rest L))))    ;; 3
        ((listp (first L)) (cons (my-replace e1 e2 (first L))           ;; 4
                                 (my-replace e1 e2 (rest L))))          ;; 5
        (t (cons (first L) (my-replace e1 e2 (rest L))))))              ;; 6

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 		SYMBOLS
(defconstant var-symbols '(U V W X Y Z)) 	;defining variables
(defconstant sum-symbols '+)    			;defining addition
(defconstant dif-symbols '-)    			;defining subtraction
(defconstant mult-symbols '*)    			;defining multiplication
(defconstant div-symbols '/)    			;defining division
(defconstant negation-sym '-)   			;defining negation
(defconstant pow-symbols 'expt) 			;defining power
(defconstant log-symbols 'log)  			;defining log

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		OPERATORS: Get the First symbol of a list
(defun sum-operator (F) (first F))
(defun diff-operator (F) (first F))
(defun mult-operator (F) (first F))
(defun div-operator (F) (first F))
(defun pow-operator (F) (first F))
(defun negation-operator (F) (first F))
(defun log-operator (F) (first F))

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		OPERANDS: Get the Second and Thirds symbols of a list
(defun sum-operand-1 (F) (second F))
(defun sum-operand-2 (F) (third F))

(defun diff-operand-1 (F) (second F))
(defun diff-operand-2 (F) (third F))

(defun mult-operand-1 (F) (second F))
(defun mult-operand-2 (F) (third F))

(defun div-operand-1 (F) (second F))
(defun div-operand-2 (F) (third F))

(defun negation-operand-1 (F) (second F))
(defun negation-operand-2 (F) (third F))

(defun pow-operand-1 (F) (second F))
(defun pow-operand-2 (F) (third F))

(defun log-operand-1 (F) (second F))
(defun log-operand-2 (F) (third F))

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		PREDICATES: Tests the list for conditions
(defun var-pred (F)(member F var-symbols)) 		;;Tests if operator is a variable

(defun sum-pred (F)								;;Tests if operator is a sum-symbol
  (equal (sum-operator F) sum-symbols))

(defun diff-pred (F)							
  (and (equal (diff-operator F) dif-symbols) 	;;Tests if operator is a dif-symbol	
       (not (negation-pred F))))				;;Checks if function is a negation

(defun mult-pred (F)							;;Tests if operator is a mult-symbol
  (equal (mult-operator F) mult-symbols))

(defun div-pred (F)								;;Test if operator is a div-symbol
  (equal (div-operator F) div-symbols))

(defun pow-pred (F)								;;Test if operator is a pow-symbol
  (equal (pow-operator F) pow-symbols))

(defun log-pred (F)								;;Test if operator is a log-symbol
  (equal (log-operator F) log-symbols))			;;	was not used in this project

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		NEGATION-PRED:	Checks if the function is a negation. Meaning if the number
;;						is negative, or if the list contains multiple negation-symbols
;;						or if the list contains a single negation-symbol and is size 2  
;;		1 	Checks if function (F) is a number and if its lower than 0
;;		2 	Checks if F is a single variable
;;		3	Checks if F is a number
;;		4	Checks if the operator is the negation-symbol and if the list length is 2 
;;		5	Checks if the operator and the first operand are equal
;;				and checks that the operator is the negation-symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun negation-pred (F)
  (cond ((and (numberp F)  											;; 1
              (< F 0)) T)  
        ((var-pred F) nil) 											;; 2 
        ((numberp F) nil)  											;; 3
        ((and (equal (negation-operator F) negation-sym)			;; 4 
              (equal (length F) 2)) T)
        ((and (equal (negation-operator F) (negation-operand-1 F))	;; 5
              (equal (negation-operator F) negation-sym)) T)))

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		CONSTRUCTORS:	Constructs prefix functions based on defined cases and tests
;;	Division:
;;	Case : F = F(x)	: G = G(x)	: Represents: Returns	:
;;	1	 : 	0		:	G		: 	0/G		: 	0		: Zero numerator
;;	2	 : 	G = F	:	F = G	:   F/G 	: 	1		: Equal operands
;;	3	 :	F		:	1		:	F/1		:	F		: Division by 1
;;	4 	 :	F 		: 	0		: 	F/0		:	nil		: Undefined Division by zero
;;	5	 :	-F		:	-G		:	-F/-G	:	F/G		: Recursive call on negations
;;	6	 : <number>	: <number>	:	F/G		: <number>	: Perfoms calculation on numbers
;;	7	 :Either F, G or both don't meet the above cases: Creates List (/ F G) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-div (F G)
  (cond ((equal F 0) 0)      			;; 1
        ((equal F G) 1)      			;; 2
        ((equal G 1) F)      			;; 3
        ((equal G 0) nil)    			;; 4
        ((and (negation-pred F)  		;; 5
		      (negation-pred G))
         (make-div (make-negation F)
                   (make-negation G)))
        ((and (numberp F)				;; 6 
              (numberp G))
         (/ F G))
        (t (list div-symbols F G))))	;; 7

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Multiplication: 
;;	Case : F = F(x)	: G = G(x)	: Represents: Returns	:
;;	1	 :	Either F, G or both equal 0		:	0		: Multiplication by 0
;;	2	 :	F = 1 	:	G 		:	1*G		:	G 		: Multiplication by 1
;;	3	 :	F		:	G = 1 	:	F*1		:	F		: Multiplication by 1
;;	4	 :	F = -1	:	G		:	-1*G	:	-G		: Multiplication by -1
;;	5	 :	F		:	G = -1	:	F*-1	:	-F 		: Multiplication by -1
;;	6	 :	-F		:	-G		:	-F*-G	:	F*G		: Recursive call on negations
;;	7	 : 	G = F 	:	F = G 	:	F*G		:	F^2 	: Creates List (expt F 2)
;;	8	 : <number> : <number>	:	F*G		: <number>	: Performs multiplication
;;	9	 :Either F, G or both don't meet the above cases: Creates List (* F G)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-mult (F G)
  (cond ((or (equal F 0)						;; 1
             (equal G 0)) 0) 
        ((equal F 1) G)                 		;; 2
        ((equal G 1) F)                 		;; 3
        ((equal F -1)                   		;; 4
         (make-negation G))
        ((equal G -1)     	  	            	;; 5
         (make-negation F))
        ((and (negation-pred F)             	;; 6 
              (negation-pred G))            
         (make-mult (make-negation F) 
                    (make-negation G)))
        ((equal F G) (list pow-symbols F 2)) 	;; 7
        ((and (numberp F)               		;; 8
              (numberp G))              
         (* F G))
        (t (list mult-symbols F G))))			;; 9

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addition:
;;	Case : F = F(x)	: G = G(x)	: Represents: Returns	:
;;	1	 :	0		:	G		:	0+G		:	G		: Addition to 0
;;	2	 :	F		:	0		:	F+0		:	F		: Addition to 0
;;	3	 :	F		:	-F		:	F+(-F)	:	0		: Addition of opposites
;;	4	 :	G		:	G		:	(-G)+G	:	0		: Addition of opposites
;;	5	 :	<number>: <number>	:	F+G		: <number>	: Performs addition
;;	6	 :Either F, G or both don't meet the above cases: Creates List (+ F G)

(defun make-sum (F G)
  (cond ((equal F 0) G)					;; 1
        ((equal G 0) F)					;; 2
        ((equal F (make-negation G)) 0) ;; 3
        ((equal G (make-negation F)) 0) ;; 4
        ((and (numberp F)               ;; 5 
              (numberp G))              
         (+ F G))
        (t (list sum-symbols F G))))	;; 6
		
;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtraction:
;;	Case : F = F(x)	: G = G(x)	: Represents: Returns	:
;;	1	 :	0		:	G		:	0-G		:	-G		: Subtraction to 0
;;	2	 :	F		:	0		:	F-0		:	-F		: Subtraction by 0
;;	3	 :	F		:	G		:	F-G		:	0		: Subtraction of equals
;;	4	 :	<number>: <number>	:	F-G		: <number>	: Performs subtraction
;;	5	 :Either F, G or both don't meet the above cases: Creates List (- F G)

(defun make-diff (F G)
  (cond ((equal F 0) (make-negation G)) ;; 1
        ((equal G 0) F)                 ;; 2
        ((equal F G) 0)                 ;; 3
        ((and (numberp F)               ;; 4
              (numberp G))              
         (- F G))
        (t (list dif-symbols F G))))	;; 5

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exponent
;;	Case : F = F(x)	: G = G(x)	: Represents: Returns	:
;;	1	 :	F		:	1		:	F^1		:	F		: Power of 1
;;	2	 :	F		:	0		:	F^0		:	1		: Power of 0
;;	3	 :	<number>: <number>	:	F^G		: <number>	: Performs exponent calculation
;;	4	 : The above cases are not met, then: Creates List (expt F G)

(defun make-pow (F G)
  (cond ((equal G 1) F)
        ((equal G 0) 1)
        ((and (numberp F)
              (numberp G))
         (expt F G))
        (t (list pow-symbols F G))))

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Log Cases
;;	Creates a list (log F)

(defun make-log (F)
  (list log-symbols F))
  
;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Negation: 	Creates and simplifes the negation of a function.
;;				If input is F returns (- F). If input is (- F) returns F
;;	1 Checks if F is a number and returns the negative of the number (- F).
;;	2 Checks if F is a variable and returns its negation (- F)
;;	3 Checks if F is a negation, and checks if the number of negation symbols
;;		is odd such as (- x) (- - - x) and returns it's simplified negation x
;;	4 Checks if F is a negation, and checks if the number of negation symbols
;;		is even such as (- - x) (- - - - x) and returns it's simplified negation
;;		(- x)
;;	5 Default case returns a list (- F)

(defun make-negation (F)
  (cond ((numberp F) (* -1 F))  					;; 1
        ((var-pred F) (list negation-sym F))  		;; 2
        ((and (negation-pred F)						;; 3
              (equal (mod (negation-count F) 2) 1)) 
         (first (last F)))
        ((and (negation-pred F)						;; 4
              (equal (mod (negation-count F) 2) 0))
         (cons negation-sym (last F)))
        (t (list negation-sym F))))					;; 5

;;;===========================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Negation-Count:	Counts the number of negation-symbols in a list.
;;	1 Checks if F is a variable member. If true then returns 0
;;	2 Checks if F is an empty list. If true then returns 0
;;	3 Checks if the first symbol in the list is a negation symbol. If
;;		true then it adds 1 to the count and recursively calls the
;;		Negation-Count with the rest of the list.
;;	4 Default case doesn't add 1 to the count and recursively calls the
;;		Negation-Count with the rest of the list.

(defun negation-count (F)
  (cond ((var-pred F) 0)
        ((null F) 0)
        ((equal negation-sym (first F)) 
         (1+ (negation-count (rest F))))
        (t (negation-count (rest F)))))


;;Test Functions
(defun t1() (integrate '1 'x))
(defun t2() (integrate '1 'y 1 4))
(defun t3() (integrate 'z 'z)) 
(defun t4() (integrate '(+ x 0) 'x))
(defun t5() (integrate '(- x) 'x 1 3))
(defun t6() (integrate '(- - x) 'x 1 4))
(defun t7() (integrate '(- x) 'x))
(defun t8() (integrate '(- - x) 'x))
(defun t9() (integrate '(- - - x) 'x))
(defun t10() (integrate '(+ x (- x)) 'x))
(defun t11() (integrate '(- (+ (- - x) x)) 'x 1 4))
(defun t12() (integrate '(+ (+ (- - x) (+ x 3)) 2) 'x 2 6))
(defun t13() (integrate '(- x (expt x 3)) 'x))
(defun t14() (integrate '(- x (expt x 3)) 'x 2 5))
(defun t15() (integrate '(+ (+ x (- - - x)) (expt x 3)) 'x))
(defun t16() (integrate '(+ (- x (- x)) (expt x 3)) 'x 2 3))
(defun t17() (integrate '(expt x -1) 'x))
(defun t18() (integrate '(expt x -1) 'x 3 45))
(defun t19() (integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x))
(defun t20() (integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x 5 217))

;;Testing Function
(defun integral-test ()
  (print "(integrate '1 'x)") (print (t1))
  (print "(integrate '1 'y 1 4)") (print (t2))
  (print "(integrate 'z 'z)") (print (t3))
  (print "(integrate '(+ x 0) 'x") (print (t4))
  (print "(integrate '(- x) 'x 1 3)") (print (t5))
  (print "(integrate '(- - x) 'x 1 4)") (print (t6))
  (print "(integrate '(- x) 'x)") (print (t7))
  (print "(integrate '(- - x) 'x)") (print (t8))
  (print "(integrate '(- - - x)") (print (t9))
  (print "(integrate '(+ x (- x)) 'x)") (print (t10))
  (print "(integrate '(- (+ (- - x) x)) 'x 1 4)") (print (t11))
  (print "(integrate '(+ (+ (- - x) (+ x 3)) 2) 'x 2 6)") (print (t12))
  (print "(integrate '(- x (expt x 3)) 'x)") (print (t13))
  (print "(integrate '(- x (expt x 3)) 'x 2 5)") (print (t14))
  (print "(integrate '(+ (+ x (- - - x)) (expt x 3)) 'x)") (print (t15))
  (print "(integrate '(+ (- x (- x)) (expt x 3)) 'x 2 3)") (print (t16))
  (print "(integrate '(expt x -1) 'x)") (print (t17))
  (print "(integrate '(expt x -1) 'x 3 45)") (print (t18))
  (print "(integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x)") (print (t19))
  (print "(integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x 5 217)") (t20))
