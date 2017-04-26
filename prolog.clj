;;lein repl
;;(load-file "prolog.clj")
(def memory (ref {}))
(def r (ref []))
(def s (ref {}))
(def var_known (ref []))
(def ren (ref 0))


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

(defn match
	[var1 var2]
	(dosync
		(ref-set s
			(merge
				(deref s)
				{(keyword var1) var2}
				)
			)
		)
	true
	)

(defn create_match
	[var1 var2]
	 (if (and (Character/isLowerCase (first (str var1))) (Character/isLowerCase (first (str var2))))
	 	(if (= var1 var2)
	 		true
	 		false
	 		)
	 	(if (and (Character/isUpperCase (first (str var1))) (Character/isUpperCase (first (str var2))))
	 		(if (= var1 var2)
	 			true
	 			)
	 		(if (and (Character/isUpperCase (first (str var1))) (Character/isLowerCase (first (str var2))))
	 			(match var1 var2)
	 			(if (and (Character/isLowerCase (first (str var1))) (Character/isUpperCase (first (str var2))))
	 				(match var2 var1)
	 				false
	 				)
	 			)
	 		)
	 	)
	)

(defn create_match_list
	[list1 list2]
	(if (= (count list1) 0)
		true
		(if
			(create_match
				(first list1)
				(first list2)
				)
			(create_match_list
				(rest list1)
				(rest list2)
				)
			false
			)
		)
	)

(defn unify
	[clause rule]
	(if (not (= (count clause) (count (first rule))))
		false
		(if (create_match_list (into [] clause) (first rule))
			true
			false
			)
		)
	)

(defn backtrack
	[]
	(pick_first_clauses_group)
	)

(defn unify_all
	[clause rules]
	(if
		(= (count rules) 0)
		false
		(if
			(unify
				clause
				(first rules)
				)
			(backtrack)
			(unify_all
				clause
				(rest rules)
				)
			)
		)
	)

(defn proof_clauses
	[clauses]
	(if (= (count clauses) 0)
		true
		(if (unify_all
				(rest (first clauses))
				(rules (first clauses))
				)
			(proof_clauses (rest clauses))
			false
			)
		)
	)

(defn pick_first_clauses_group
	[]
	(if (= 0 (count (deref r)))
		true
		(let [clause (first (deref r))]
			(dosync
				(ref-set
					r
					(rest (deref r))
					)
				)
			(proof_clauses clause)
			)
		)
	)

(defn rename_args
	[l]
	(if (= 0(count l))
		[]
		(concat_vec
			(str (keyword (first l)) (deref ren))
			(rename_args (rest l))
			)
		)
	nil
	)

(defn rename_clause
	[clause]
	(concat_vec
		(first clause)
		(rename_args (rest clause))
		)
	)

(defn rename_clauses
	[clauses]
	(dosync(ref-set ren
		(+ (deref ren) 1)))
	(if (= 0(count clauses))
		[]
		(concat_vec
			(rename_clause (first clauses))
			(rename_clauses(rest clauses))
			)
		)
	)

(defn add_clauses_to_r
	[clauses]
	(dosync
		(ref-set r
			(into []
				(reverse
					(conj
						(deref r)
						(to_vec clauses)
						)
					)
				)
			)
		)
	)

(defmacro ?-
	"Runs queries against the knowledge base.
	It takes a variable number of goal clauses.
	The effect is the same as separating the clauses with a comma in Prolog"
	[& clauses]
	(add_clauses_to_r clauses)
	(dosync(ref-set s []))
	(if
		(=(count clauses) 0)
		'()
		(let [result (pick_first_clauses_group)]
			(println (deref s))
			result
			)
		)
	)


(<- (male george))
(<- (parent Parent Child) (father Parent Child))
(<- (parent Parent Child) (mother Parent Child))
(<- (append (list) T T))
(<- (append (list H T) L2 (list H TR)) (append T L2 TR))

(<- (parent a b))
(<- (parent b c))

(<- (gp A C) (parent A B) (parent B C))

;(?- (male george))
;(keyword (str (first (quote (parent Parent Child)))) "1")

;(symbol "parent")

; {
; 	:male
; 	(
; 		(
; 			(george)
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






