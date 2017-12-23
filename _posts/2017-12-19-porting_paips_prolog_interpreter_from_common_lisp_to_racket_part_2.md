---
layout:     post
comments:   true
title:      "Porting PAIP's Prolog interpreter from Common Lisp to Racket - Part 2"
subtitle:   "Porting interpreter's second version shown in PAIP"
date:       2017-12-19
author:     "Promesante"
header-img: "img/f6d9753217.jpg"
---

# Porting PAIP's Prolog interpreter from Common Lisp to Racket - Part 2#



As laid out in my [previous post]({% post_url 2017-10-12-porting_paips_prolog_interpreter_from_common_lisp_to_racket %}), I am porting from Common Lisp to Racket the Prolog interpreter exposed in Peter Norvig's classic text, [Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp 1st Edition](https://www.amazon.com/Paradigms-Artificial-Intelligence-Programming-Studies/dp/1558601910/ref=sr_1_1?s=books&ie=UTF8&qid=1506786797&sr=1-1&keywords=paradigms+of+artificial+intelligence+programming), also known as PAIP, in its chapter 11.

This second post covers interpreter's second version, exposed in sections 11.3 though 11.5. And, as the first post, it focuses on its port, from its original implementation in Common Lisp, to Racket, in the main differences between its implementation in each of these Lisp dialects.

Source code available in my [GitHub](https://github.com/) [repo](https://github.com/promesante/paip-racket), branch `section-11.3`.


---

## Automatic Backtracking ##

The first version of the interpreter, exposed in the previous post, for every query, returns all the solutions at once, in a manner PAIP calls "batch approach". On the other hand, its second version, exposed in this post, for every query returns one solution at a time, as they are found, as usual in real Prolog interpreters: PAIP calls this approach "incremental".

In order to implement it, PAIP uses a specific goal which "extends every query to print out the variables, and ask the user if the computation should be continued". This is a new type of goal, which is independent of the database but "causes a procedure to take action. In Prolog, such procedures are called `primitives`, because they are built-in to the language".

The first, actually unique `primitive` procedure included in the interpreter's new version exposed in section 11.3, `show-prolog-vars`, appears in `top-level-prove` function, which is directly by query operator.

For every modification made to the interpreter's new version, we will compare below the initial version, shown in PAIP's 11.2 section, with the new one, shown in turn in section 11.3, in their original implementation, in Common Lisp:

---

### Section 11.2 ###

```cl
(defun top-level-prove (goals)
  "Prove the goals, and print variables readably."
  (show-prolog-solutions
    (variables-in goals)
    (prove-all goals no-bindings)))
``` 

---

### Section 11.3 ###

```cl
(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (format t "~&No.")
  (values))
```

As mentioned above, primitive `show-prolog-vars` is left to the very end of the list of goals.

And then, its port to Racket:

```scheme
;; Prove the goals, and print variables readably
(define (top-level-prove goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (printf "No.")
  (values))
``` 

---

Then, we approach `prove-all` function. As explained above, in this new version, with "incremental approach", for every query, just one solution at a time is returned, as they are found:

---

### Section 11.2 ###

```cl
(defun prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) (list bindings))
        (t (mapcan #'(lambda (goal1-solution)
                       (prove-all (rest goals) goal1-solution))
                   (prove (first goals) bindings)))))
``` 

---

### Section 11.3 ###

```cl
(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))
``` 

And then, its port to Racket:

```scheme
;; Return a list of solutions to the conjunction of goals
(define (prove-all goals bindings)
  (cond ((equal? bindings fail) fail)
        ((null? goals) (list bindings))
        (else (prove (car goals) bindings (cdr goals)))))
``` 

Then, we go on with `prove` function.

---

### Section 11.2 ###

```cl
(defun prove (goal bindings)
  "Return a list of possible solutions to goal."  
  (mapcan #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all (clause-body new-clause)
                           (unify goal (clause-head new-clause) bindings))))
          (get-clauses (predicate goal))))
``` 

---

### Section 11.3 ###

```cl
(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
        (some
          #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all
                  (append (clause-body new-clause) other-goals)
                  (unify goal (clause-head new-clause) bindings))))
          clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (funcall clauses (rest goal) bindings
                 other-goals))))
``` 

This last `funcall` invokes `primitives`.

And then, its port to Racket:

```scheme
;; Return a 1ist of possible solutions to goal
(define (prove goal bindings other-goals)
  (let* ((pred (predicate goal))
         (clauses (get-clauses pred)))
    (if (null? clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (eval `(,pred ',(rest goal) ',bindings ',other-goals))
        (for/or ((clause clauses))
          (let ((new-clause (rename-variables clause)))
            (prove-all
             (append (clause-body new-clause) other-goals)
             (unify goal (clause-head new-clause) bindings)))))))
``` 

For `primitives` to be picked by the `if` condition in previous snippet, the corresponding symbol had to be inserted in the clauses dictionary. This is performed in a slightly different manner in Racket than in Common Lisp:

```cl
(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)
``` 

This last `funcall` invokes `primitives`.

And then, its port to Racket:

```scheme
(hash-set! db-predicates 'show-prolog-vars null)
``` 

Then, we go on with `primitive show-prolog-vars` function implementation:

---

### Section 11.2 ###

```cl
(defun show-prolog-vars (vars bindings)
  "Print each variable with its binding."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (princ ";"))
``` 

---

### Section 11.3 ###

```cl
(defun show-prolog-vars (vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (if (null vars)
      (format t "~&Yes")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))
``` 

And then, its port to Racket:

```scheme
;; Print each variable with its binding
(define (show-prolog-vars vars bindings other-goals)
  (if (null? vars)
      (printf "Yes~%")
      (for ((var vars))
        (printf "~a = ~a~%"
                var
                (subst-bindings bindings var))))
  (if (continue?)
      fail
      (prove-all other-goals bindings)))
``` 

And the following predicate, referenced from `show-prolog-vars` function, was added in section 11.3:

```cl
(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise 
      (format t " Type ; to see more or . to stop")
      (continue-p))))
``` 

And then, its port to Racket:

```scheme
;; Ask user if we should continue looking for solutions
(define (continue?)
  (case (read-char)
    ((#\;) #t)
    ((#\.) #f)
    ((#\newline) (continue?))
    (else
     (printf "Type ; to see more or . to stop")
     (continue?))))
``` 

### Test Cases ###

```scheme
> (<- (member ?item (?item . ?rest)))
> (<- (member ?item (?x . ?rest)) (member ?item ?rest))

> (?- (member 2 (1 2 3)))
Yes;
No.

> (?- (member 2 (1 2 3 2 1)))
Yes;
Yes;
No.

> (<- (length () 0))
> (<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))

> (?- (length (a b c d) ?n))
?n = (1+ (1+ (1+ (1+ 0))));
No.
```

---

## Anonymous Variables ##

This kind of variable denote the ones which are not relevant, denoted by `?`. In the following example: `?x`.

```scheme
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (member ?item (?item . ?)))
(<- (member ?item (? . ?rest)) (member ?item ?rest))
```

They are implemented by the following functions:

### Section 11.2 ###

```cl
(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',clause))

(defmacro ?- (&rest goals) `(top-level-prove ',goals))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable-p exp))
``` 

---

### Section 11.3 ###

```cl
(defmacro <- (&rest clause)
  "add a clause to the data base."
  `(add-clause ',(replace-?-vars clause)))

(defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))

(defun replace-?-vars (exp)
    "Replace any ? within exp with a var of the form ?123."
    (cond ((eq exp '?) (gensym "?"))
	  ((atom exp) exp)
	  (t (reuse-cons (replace-?-vars (first exp))
			 (replace-?-vars (rest exp))
			 exp))))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'non-anon-variable-p exp))

(defun non-anon-variable-p (x)
  (and (variable-p x) (not (eq x '?))))
``` 

And then, its port to Racket:

```scheme
;; Add a clause to the data base
(define-syntax-rule (<- fact1 ...)
  (add-clause (replace-?-vars '(fact1 ...))))

(define-syntax-rule (?- goal1 ...)
  (top-level-prove (replace-?-vars '(goal1 ...))))

;; Replace any ? within exp with a var of the form ?123
(define (replace-?-vars exp)
  (cond ((equal? exp '?) (gensym "?"))
        ((not (cons? exp)) exp)
        (else (reuse-cons (replace-?-vars (car exp))
                          (replace-?-vars (cdr exp))
                          exp))))

;; Return a list of all the variables in EXP
(define (variables-in exp)
  (unique-find-anywhere-if non-anon-variable? exp))

(define (non-anon-variable? x)
  (and (variable? x) (not (eq? x '?))))
``` 

---

## The Zebra Puzzle ##

This is the final example given PAIP: a logic game.

```scheme
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) (member ?item ?rest))

(<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest))
    (iright ?left ?right ?rest))

(<- (= ?x ?x))

(<- (zebra ?h ?w ?z)
  ;; Each house is of the form:
  ;; (house nationality pet cigarette drink house-color)
  (= ?h ((house norwegian ? ? ? ?)                    ; 1, 10
         ?
         (house ? ? ? milk ?) ? ?))                   ; 9
  (member (house englishman ? ? ? red) ?h)            ; 2
  (member (house spaniard dog ? ? ?) ?h)              ; 3
  (member (house ? ? ? coffee green) ?h)              ; 4
  (member (house ukrainian ? ? tea ?) ?h)             ; 5
  (iright (house ? ? ? ? ivory)                       ; 6
          (house ? ? ? ? green) ?h)
  (member (house ? snails winston ? ?) ?h)            ; 7
  (member (house ? ? kools ? ye1low) ?h)              ; 8
  (nextto (house ? ? chesterfield ? ?)                ; 11
          (house ? fox ? ? ?) ?h)
  (nextto (house ? ? kools ? ?)                       ; 12
          (house ? horse ? ? ?) ?h)
  (member (house ? ? luckystrike orange-juice ?) ?h)  ; 13
  (member (house japanese ? parliaments ? ?) ?h)      ; 14
  (nextto (house norwegian ? ? ? ?)                   ; 15
          (house ? ? ? ? blue) ?h)
  ;; Now for the questions:
  (member (house ?w ? ? water ?) ?h)                  ; Q1
  (member (house ?z zebra ? ? ?) ?h))                 ; Q2

(?- (zebra ?houses ?water-drinker ?zebra-owner))

> (?- (zebra ?houses ?water-drinker ?zebra-owner))
?zebra-owner = japanese
?water-drinker = norwegian
?houses = ((house norwegian fox kools water ye1low) (house ukrainian horse chesterfield tea blue) (house englishman snails winston milk red) (house spaniard dog luckystrike orange-juice ivory) (house japanese zebra parliaments coffee green));
No.
```

And that's all by now...

Source code available in my [repo](https://github.com/promesante/paip-racket), at [GitHub](https://github.com/), branch `section-11.3`.

---

## Next steps ##

From this base, I plan to go on, with the steps listed below:

* **Chapter 11**: current chapter, porting the rest of the sections, each in its own branch.
* **Chapter 12**: Compiling Logic Programs
* **Chapter 22**: Scheme: An Uncommon Lisp
* **Chapter 23**: Compiling Lisp

I hope this information to be useful for you ! And till next post !
