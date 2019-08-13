---
layout:     post
comments:   true
title:      "Porting PAIP's Prolog interpreter from Common Lisp to Racket"
subtitle:   "Porting interpreter's first version shown in PAIP"
date:       2017-10-12
author:     "Promesante"
background: "/img/posts/07.jpg"
---

# Porting PAIP's Prolog interpreter from Common Lisp to Racket #

I'm learning Racket. After having studied about it, I am beginning to go on learning, but in a more practical, hands-on way. As Racket's main goal is to be a "language development lab", I guess the most thorough way to learn it is... coding an interpreter.

Among what I studied so far on this subject, I've chosen the Prolog interpreter implemented and explained in Peter Norvig's classic text on AI, [Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp 1st Edition](https://www.amazon.com/Paradigms-Artificial-Intelligence-Programming-Studies/dp/1558601910/ref=sr_1_1?s=books&ie=UTF8&qid=1506786797&sr=1-1&keywords=paradigms+of+artificial+intelligence+programming) (also known as PAIP), in its chapter 11. Furthermore, many recommend this text as an excellent entry point into Lisp and its dialects. Book's source code is available in the [author's Web site ](http://norvig.com/paip/README.html).

This first post covers interpreter's first version, exposed in section 11.2. And focuses on its port, from its original implementation in Common Lisp, to Racket, in the main differences between its implementation in each of these Lisp dialects. So, familiarity with Prolog programming language, PAIP's explanation about the interpreter, or at least its source code, is assumed.

In order to learn Prolog, I'd suggest the following tutorials:

* [Prolog Tutorial](https://www.cpp.edu/~jrfisher/www/prolog_tutorial/pt_framer.html): very organized, straighforward tutorial
* [Adventure in Prolog](http://www.amzi.com/AdventureInProlog/): learning Prolog by coding a text adventure game. Fun !
* [SWI-Prolog](http://www.swi-prolog.org/): SWI-Prolog is this language's interpreters ecosystem leader, and its documentation is among the most complete and thorough

Main online resouces used:

* [The Racket Guide](https://docs.racket-lang.org/guide/)
* [The Racket Reference](http://docs.racket-lang.org/reference/)
* [Common Lisp HyperSpec](http://www.lispworks.com/documentation/lw61/CLHS/Front/)
* [Hyperpolyglot - Lisp: Common Lisp, Racket, Clojure, Emacs Lisp](http://hyperpolyglot.org/lisp)

Source code available in my [repo](https://github.com/promesante/paip-racket), at [GitHub](https://github.com/), branch `section-11.2`.

---

## Prolog operators ##

Insert, <-, and query, ?- operators are implemented, by means of macros.

In PAIP (Common Lisp):

```cl
(defmacro <- (&rest clause)
  "add a clause to the data base."
  `(add-clause ',clause))
  
(defmacro ?- (&rest goals) `(top-level-prove ',goals))
``` 

These macros ported to Racket:

```scheme
(define-syntax-rule (<- fact1 ...)
  (add-clause (list 'fact1 ...)))
  
(define-syntax-rule (?- goal1 ...)
  (top-level-prove (list 'goal1 ...)))
``` 

---

## Predicates database ##

Driven by insert operator, in PAIP interpreter, predicates are stored in memory as a list, and clauses are bound to each predicate as a property:

```cl
(defvar *db-predicates* nil
  "A list of all predicates stored in the database.")

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))

(defun get-clauses (pred) (get pred 'clauses))

(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))
  
(defun clear-predicate (predicate)
  "remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))
``` 

On the other hand, in Racket, I used a dictionary, indexed by predicate as symbol:

```scheme
; A list of all predicates stored in the database
(define db-predicates (make-hash))

(define (get-clauses pred)
  (hash-ref db-predicates pred))

; Add a clause to the data base, indexed by head's predicate
(define (add-clause clause)
  (let ((pred (predicate (clause-head clause))))
    ;; The predicate must be a non-variable symbol
    (when (and (symbol? pred) (not (variable? pred)))
      (if (hash-has-key? db-predicates pred)
          (hash-update! db-predicates pred
                        (lambda (value)
                          (append value (list clause))))
          (hash-set! db-predicates pred (list clause))))
    pred))

; Remove all clauses (for all predicates) from the data base
(define (clear-db)
  (hash-clear! db-predicates))

; Remove the clauses for a single predicate
(define (clear-predicate predicate)
  (hash-remove! db-predicates predicate))
``` 

---

## Proving queries ##

On the other side, driven by query operator, queries are handled primarily by the following two functions, `prove-all` and `prove`:

```cl
(defun prove (goal bindings)
  "Return a list of possible solutions to goal."
  (mapcan #'(lambda (clause)
              (let ((new-clause (rename-variables clause)))
                (prove-all (clause-body new-clause)
                           (unify goal (clause-head new-clause) bindings))))
          (get-clauses (predicate goal))))

(defun prove-all (goals bindings)
  "Return a list of solutions to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) (list bindings))
        (t (mapcan #'(lambda (goal1-solution)
                       (prove-all (rest goals) goal1-solution))
                   (prove (first goals) bindings)))))
``` 

These two functions, ported to Racket, are shown below:

```scheme
; Return a 1ist of possible solutions to goal
(define (prove goal bindings)
  (append-map (lambda (clause)
                (let ((new-clause (rename-variables clause)))
                  (prove-all (clause-body new-clause)
                             (unify goal (clause-head new-clause) bindings))))
              (get-clauses (predicate goal))))

; Return a list of solutions to the conjunction of goals
(define (prove-all goals bindings)
  (cond ((equal? bindings fail) fail)
        ((null? goals) (list bindings))
        (else (append-map (lambda (goal1-solution)
                            (prove-all (rest goals) goal1-solution))
                          (prove (first goals) 
``` 

This part was by far the hardest one, at least for me: devicing how to port to Racket Common Lisp mapcan function. I have finally been able to perform it thanks to [to_the_crux's](https://stackoverflow.com/users/2013341/to-the-crux) reply to [this question](https://stackoverflow.com/questions/14617095/how-to-use-append-map-in-racket-scheme) in [StackOverflow](https://stackoverflow.com/).

---

## Porting Common Lisp primitive functions to Racket ##

Furthermore, a couple Common Lisp primitive functions have to be re-implemented in Racket, in order to alter the least PAIP's functions using them:

* [sublis](http://www.lispworks.com/documentation/lw61/CLHS/Body/f_sublis.htm#sublis)
* [adjoin](http://www.lispworks.com/documentation/lw61/CLHS/Body/f_adjoin.htm#adjoin)


Porting the `rename-variables` function shown below:

```cl
(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))
``` 

That function, ported to Racket:

```scheme
; Replace all variables in x with new ones
(define (rename-variables x)
  (sublis (map (lambda (var) (cons var (gensym (symbol->string var))))
               (variables-in x))
          x))
``` 

Both versions of this function are almost equal as Common Lisp `sublis` function has been re-implemented in Racket as shown below:

```scheme
(define (sublis pairs lst)
  (map (lambda (elem)
         (if (list? elem)
             (sublis pairs elem)
             (let ((mem-pairs (memf (lambda (pair)
                                      (equal? elem (car pair)))
                                    pairs)))
               (if (not mem-pairs)
                   elem
                   (cdr (car mem-pairs))))))
         lst))
```

On the other hand, porting `unique-find-anywhere-if` function shown below:

```cl
(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))
``` 

That function, ported to Racket:

```scheme
; Return a list of leaves of tree satisfying predicate, with duplicates removed
(define (unique-find-anywhere-if predicate tree (found-so-far '()))
  (if (cons? tree)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree) found-so-far))
      (if (apply predicate (list tree))
          (adjoin tree found-so-far)
          found-so-far)))
``` 

Both versions of this function are almost equal as Common Lisp `adjoin` function has been re-implemented in Racket as shown below:

```scheme
(define (adjoin item lst)
  (if (member item lst)
      lst
      (append lst (list item))))
```

---

## Test cases ##

In order to test the interpreter, Prolog sessions given in PAIP for that purpose has been used. In all of them, the results finally got are the same shown in the book.

First, the ones regarding the `unify` function:

```scheme
> (unify '(f ?x) '(f ?y))
'((?x . ?y)) 

> (unify '(?a + ?a = 0) '(?x + ?y = ?y))
'((?y . 0) (?x . ?y) (?a . ?x))

> (unify '(?x + 1) '(2 + ?y))
'((?y . 1) (?x . 2))

> (unify '?x '?y)
'((?x . ?y))

> (unify '(?x ?x) '(?y ?y))
'((?x . ?y))

> (unify '(?x ?y) '(?y ?x))
'((?x . ?y))

> (unify '(?x ?y a) '(?y ?x ?x))
'((?y . a) (?x . ?y))

> (unify '(?x ?y a) '(?y ?x ?x))
'((?y . a) (?x . ?y))

> (unify '(?x ?y a) '(?y ?x ?x))
'((?y . a) (?x . ?y))

> (unify '?x '(f ?x))
'()

> (unify '(?x ?y) '((f ?y) (f ?x)))
'()

> (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y)))
'()

> (unify 'a 'a)
'((#t . #t))
```

Then, the ones also involving the `unify` function, but with `set-occurs-check` set to `#f`:

```scheme
> (unify '?x '(f ?x))
'((?x f ?x))

> (unify '(?x ?y) '((f ?y) (f ?x)))
'((?y f ?x) (?x f ?y))

> (unify '(?x ?y ?z) '((?y ?z) (?x ?z) (?x ?y)))
'((?z ?x ?y) (?y ?x ?z) (?x ?y ?z))
```

Then, the ones involving the `unifier` function:

```scheme
> (unifier '(?a + ?a = 0) '(?x + ?y = ?y))
'(0 + 0 = 0)

> (unifier '(?a + ?a = 2) '(?x + ?y = ?y))
'(2 + 2 = 2)

> (unifier '(?x ?y a) '(?y ?x ?x))
'(a a a)

> (unifier '((?a * ?x ^ 2) + (?b * ?x) + ?c)
           '(?z + (4 * 5) + 3))
'((?a * 5 ^ 2) + (4 * 5) + 3)

> (unifier '(?x ?y a) '(?y ?x ?x))
'(a a a)

> (unifier '(?x ?y a) '(?y ?x ?x))
'(a a a)

> (unifier '(?x ?y a) '(?y ?x ?x))
'(a a a)
```

Then, the ones involving insert and query operations:

```scheme
> (<- (likes Kim Robin))
'likes

> (<- (likes Sandy Lee))
'likes

> (<- (likes Sandy Kim))
'likes

> (<- (likes Robin cats))
'likes

> (<- (likes Sandy ?x) (likes ?x cats))
'likes

> (<- (likes Kim ?x) (likes ?x Lee) (likes ?x Kim))
'likes

> (<- (likes ?x ?x))
'likes

> (?- (likes Sandy ?who))
?who = Lee;
?who = Kim;
?who = Robin;
?who = Sandy;
?who = cats;
?who = Sandy;
```

And tha's all by now...

Source code available in my [repo](https://github.com/promesante/paip-racket), at [GitHub](https://github.com/), branch `section-11.2`.

---

## Next steps ##

From this base, I plan to go on, with the steps listed below:

* **Chapter 11**: current chapter, porting the rest of the sections, each in its own branch.
* **Chapter 12**: Compiling Logic Programs
* **Chapter 22**: Scheme: An Uncommon Lisp
* **Chapter 23**: Compiling Lisp

I hope this information to be useful for you ! And till next post !
