;;lein repl
;;(load-file "prolog.clj")
(def memory (ref {}))

(defn in? 
	"true if coll contains elm"
	[coll elm]  
	(some
		#(= elm %)
		coll
		)
	)

(defn add_rule
	[head body]
	(dosync
		(ref-set
			memory
			(merge-with
				concat
				(deref memory)
				{(keyword (first head)) (list (cons (rest head) body))}
				)
			)
		)
	)

(defmacro <- 
	"Define new rules and facts.
	The first clause after is the head, the rest is the body (nothing for simple facts).
	The head may contain any kind of structure (functors, atoms, ...), not only variables."
	[head & body]
	(add_rule
		head
		body
		)
	)

(defn unify
	[clause]
	(get memory (keyword (first clause)))
	)

(defn resolution
	[r s]
	(unify (first r))
	) 

(defmacro ?-
	"Runs queries against the knowledge base.
	It takes a variable number of goal clauses.
	The effect is the same as separating the clauses with a comma in Prolog"
	[& clauses]
	(if
		(=(count clauses) 0)
		'()
		(resolution clauses '())
		)
	)


(<- (male george))
(<- (parent Parent Child) (father Parent Child))
(<- (parent Parent Child) (mother Parent Child))
(<- (append (list) T T))
(<- (append (list H T) L2 (list H TR)) (append T L2 TR))
(pprint memory)

; {
; 	:male
; 	(
; 		(
; 			(georges)
; 			()
; 		)
; 	)
; 	:parent
; 	(
; 		(
; 			(Parent Child)
; 			(
; 				pather
; 				Parent
; 				Child
; 			)
; 		)
; 		(
; 			(Parent Child)
; 			(
; 				mother
; 				Parent
; 				Child
; 			)
; 		)
; 	)
; 	:append
; 	(
; 		[
; 			((list) T T)
; 			()
; 		)
; 		(
; 			((list H T) L2 (list H TR))
; 			(
; 				append
; 				T
; 				L2
; 				TR
; 			)
; 		)
; 	)
; }







