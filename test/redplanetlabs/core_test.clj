(ns redplanetlabs.core-test
  (:require [clojure.test :refer :all]
            [redplanetlabs.core :as sut]))

(sut/defstackfn* example [!a !b !c] ; example uses input: 1 2 4. Stack starts empty.
  !a ; 1
  !b ; 1 2
  (invoke> + 2) ; 3
  !v1+ ; 3
  !c ; 3 4
  !c ; 3 4 4
  <pop> ; 3 4
  2 ; 3 4 2
  (invoke> * 2) ; 3 8
  !v2+ ; 3 8
  (invoke> = 2) ; false
  (if> ; stack empty
      !v1
    !v2
    (invoke> - 2)
    else>
    "false!!" ;; "false!!"
    (invoke> println 1) ; nil
    <pop> ; stack empty
    !v1 ; 3
    !v2 ; 3 8
    (invoke> * 2) ; 24
    )
  )

(sut/defstackfn example-tos [!a !b !c] ; example uses input: 1 2 4. Stack starts empty.
  !a ; 1
  !b ; 1 2
  (invoke> + 2) ; 3
  !v1+ ; 3
  !c ; 3 4
  !c ; 3 4 4
  <pop> ; 3 4
  2 ; 3 4 2
  (invoke> * 2) ; 3 8
  !v2+ ; 3 8
  (invoke> = 2) ; false
  (if> ; stack empty
      !v1
    !v2
    (invoke> - 2)
    else>
    "false!!" ;; "false!!"
    (invoke> println 1) ; nil
    <pop> ; stack empty
    !v1 ; 3
    !v2 ; 3 8
    (invoke> * 2) ; 24
    )
  )



(sut/defstackfn* nested-if [!y]
  !y !y
  (invoke> pos? 1)
  (if> !y
    (invoke> even? 1)
    (if> 3
      (invoke> + 2)
      else>
      11
      (invoke> * 2))
    else> -99))

(sut/defstackfn* shadow [!x !y] !x !y !x+ !x)

(sut/defstackfn* looping []
  1 true
  (loop> !x+ !x ;; hacky dup
         (invoke> + 2)
         !x+ !x 
         10000 (invoke> < 2)))

(sut/defstackfn* fib [!start !max]
  !start !start true
  (loop> !a+ <pop> !b+ <pop> !a !a !b
         (invoke> + 2) ;; 1 2
         !b !b 
         !max (invoke> < 2)))

(sut/defstackfn* loop-break1 [!start]
  !start true
  (loop> !x+ !x (invoke> dec 1)
         !x+ !x 5 (invoke> < 2)
         (if> break>
           else>
           !x+ !x)
         (invoke> pos? 1)))

(sut/defstackfn* make-square [!len]
  1 !rows+ <pop> [] true
  (loop>
   [] 1 !x+ <pop> true ;; [] true
   (loop> !x ;; [] 1
          (invoke> conj 2) ;; [1]
          !x ;; [1] 1
          (invoke> inc 1) ;; [1] 2
          !x+
          !len ;; [1] 2 5
          (invoke> <= 2))
   (invoke> conj 2)
   !rows
   (invoke> inc 1)
   !rows+
   !len
   (invoke> <= 2)))

(sut/defstackfn* loop-continue [!x]
  !x
  2 (invoke> > 2) ;; if > 2 loop
  (loop>
   !x 3 (invoke> mod 2) 0 (invoke> = 2) ;; divisible by 3
   (if> !x 2 (invoke> - 2) !x+ true continue> ;; subtract 2 from x, leave item on stack
        else>
        !x 10 (invoke> - 2)) ;; subtract 10
   (invoke> dec 1) !x+ ;; dec and set x
   2 (invoke> > 2)))

(sut/defstackfn* closure []
  4 !x+

  (fn> ([!z] !x 4 (invoke> = 2)
        (if> !z :yes
             27 !x+ <pop> ;; change x to 27 in inner scope
             else> :no))) 
  5 !x+ <pop> ;; change x in outer scope
  (call> :fnarg)

  !x)

(sut/defstackfn* varargs [!x !a]
  (fn> ([!x] !x) ([!x & !y] !x !y))

  !a
  !x+
  <pop>
  (call> 1 2 3 4))

(sut/defstackfn* multiple-call [!x]
  (fn> ([] 7) ([!y] !x !y (invoke> + 2)))
  !f+
  (call>)
  !f
  (call> 5))

(sut/defstackfn* java [!x !y]
  !x (invoke> Math/abs 1) (invoke> .toString 1) !y (invoke> .contains 2)
  (invoke> String. 0) (invoke> .length 1) 0 (invoke> = 2))

(deftest example-test
  (is (= (example 1 2 4) '(24)))
  (is (= (example-tos 1 2 4) 24)))

(deftest nested-if-test
  (are [y exp] (= (nested-if y) exp)
    4 '(7)
    5 '(55)
    -4  '(-99 -4)))

(deftest shadow-test
  (is (= (shadow 1 2) '(2 2 1))))

(deftest loop-test
  (is (= (fib 1 10) '(13 21 8 5 3 2 1 1)))
  (is (= (loop-break1 7 '(7 6 5 4)))))

(deftest nested-loop-test
  (is (= (make-square 3) '([[1 2 3] [1 2 3] [1 2 3]]))))

(deftest lexical-closure-test
  (is (= (closure) '(5 :yes :fnarg 4))))

(deftest multiple-call-test
  (is (= (multiple-call 11) '(16 7))))

(deftest varargs-test
  (is (= (varargs 12 19) '((2 3 4) 1))))

(deftest java-test
  (is (= (java -140 "4") '(true true)))
  (is (= (java -140 "5") '(true false))))

(deftest continue-test
  (is (= (loop-continue 30) '(4 28))))

(deftest compiler-error-tests
  (are [body] (thrown-with-msg? clojure.lang.Compiler$CompilerException
                                #"Syntax error"
                                (eval `(sut/defstackfn foo [] ~@body)))
    `[(unknownFn 1)]
    `[unknownFn]
    `[(invoke> +)] ;; no arity
    `[(invoke> + :badarity)] 
    `[(invoke> :unknownFn 2)] ;; unresolvable 
    `[(fn> ([]) ([]))] ;; two bodies with same arity
    `[(loop> (if> 1 break> 2 else> 3))] ;; break> must be tail position
    `[break>] ;; break> not allowed outside loop
    `[(loop> break>)] ;; unconditional break> not allowed
    `[continue>] ;; continue not allowed outside loop
    `[()]))
