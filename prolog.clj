;;lein repl
;;(load-file "prolog.clj")
(def memory (ref {}))
(def r (ref []))
(def s (ref []))

(defn in? 
	"true if coll contains elm"
	[coll elm]  
	(some
		#(= elm %)
		coll
		)
	)

(defn concat_vec
	[list1 list2]
	(into [] (concat list1 list2))
	)

(defn add_rule
	[head body]
	(dosync
		(ref-set
			memory
			(merge-with
				concat_vec
				(deref memory)
				{(keyword (first head)) (into [] (list (cons (into [] (rest head)) (into [] body))))}
				)
			)
		)
	nil
	)

(defn to_vec
	[body]
	(if (= (count body) 0)
		[]
		(into [] (cons (into [] (first body)) (to_vec (rest body))))
		)
	)

(defmacro <- 
	"Define new rules and facts.
	The first clause after is the head, the rest is the body (nothing for simple facts).
	The head may contain any kind of structure (functors, atoms, ...), not only variables."
	[head & body]
	(add_rule
		head
		(into [] (to_vec body))
		)
	)

(defn rules
	[clause]
	(get (deref memory) (keyword (first clause)))
	)

(defn resolution
	[]
	(rules (first (deref r)))
	) 

(defmacro ?-
	"Runs queries against the knowledge base.
	It takes a variable number of goal clauses.
	The effect is the same as separating the clauses with a comma in Prolog"
	[& clauses]
	(print clauses)
	(dosync(ref-set r (into [] (concat (deref r) (into [] (to_vec clauses))))))
	(dosync(ref-set s []))
	(if
		(=(count clauses) 0)
		'()
		(resolution)
		)
		nil
	)


(<- (male george))
(<- (parent Parent Child) (father Parent Child))
(<- (parent Parent Child) (mother Parent Child))
(<- (append (list) T T))
(<- (append (list H T) L2 (list H TR)) (append T L2 TR))

;(?- (male george))

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

(defn tester
	[w]
	(keyword w))
