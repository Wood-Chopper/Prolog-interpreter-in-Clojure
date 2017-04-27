;;lein repl
;;(load-file "prolog.clj")
(def memory (ref {}))
(def r (ref []))
(def s (ref {}))
(def var_known (ref []))
(def ren (ref 0))



(defn concat_vec
	[list1 list2]
	(into [] (concat list1 list2))
	)

(defn rename_arg
	[arg]
	(if (= 0 (deref ren))
		arg
		(symbol (str arg (deref ren)))
		)
	)

(defn rename_args
	[args]
	(if (= 0(count args))
		[]
		(concat_vec
			(rename_arg (first args))
			(rename_args (rest args))
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

(defn get_rules
	[clause]
	(get (deref memory) (keyword (first clause)))
	)

(defn no_conflict
	[var1 var2]
	(if (contains? (deref s) (keyword var1))
		(if (and (Character/isUpperCase (first (str var1))) (Character/isUpperCase (first (str var2))))
			true
			false
			)
		(if (in? (vals (deref s)) var2)
			false
			true
			)
		)
	)

(defn match
	[var1 var2]
	(if (= (get (deref s) (keyword var1)) var2)
		true
		(if (no_conflict var1 var2)
			(do
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
			false
			)
		)
	)

(defn create_match
	[var1 var2]
	;(println (symbol (str var1 (deref ren))))
	;(match (symbol (str var1 (deref ren))) (symbol (str var2 (deref ren))))
	(if (and (Character/isLowerCase (first (str var1))) (Character/isLowerCase (first (str var2))))
	 	(if (= var1 var2)
	 		true
	 		false
	 		)
	 	(if (and (Character/isUpperCase (first (str var1))) (Character/isUpperCase (first (str var2))))
	 		(match (rename_arg var1) (rename_arg var2))
	 		(if (and (Character/isUpperCase (first (str var1))) (Character/isLowerCase (first (str var2))))
	 			(match (rename_arg var1) var2)
	 			(if (and (Character/isLowerCase (first (str var1))) (Character/isUpperCase (first (str var2))))
	 				(match (rename_arg var2) var1)
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
	(println "unify " clause rule)
	(if (not (= (count clause) (count (first rule))))
		false
		(let [saved_s (deref s)]
			(if (create_match_list (into [] clause) (first rule))
				true
				(do
					(dosync
						(ref-set s
							saved_s
							)
						)
					false
					)
				)
			)
		)
	)

(defn unify_and
	[clauses]
	(println "and " clauses)
	(if (or (= 0 (count clauses)) (= nil clauses))
		true
		(let [saved_s (deref s)]
			(if
				(and
					(unify_or
						(first clauses)
						(get_rules (first clauses))
						)
					(unify_and (rest clauses))
					)
				true
				(do
					(dosync
						(ref-set s
							saved_s
							)
						)
					false
					)
				)
			)
		)
	)

(defn unify_or
	[clause rules]
	(println "or  " clause " with " (first rules))
	(if
		(= 0 (count rules))
		false
		(let [saved_s (deref s)]
			(if
				(unify
					(rest clause)
					(first rules)
					)
				(if (unify_and (rest (first rules)))
					true
					(do
						(println (first rules) " pas bon choix")
						(dosync
							(ref-set s
								saved_s
								)
							)
						(unify_or
							clause
							(rest rules)
							)
						)
					)
				(do
					(println (first rules) " pas bon choix")
					(dosync
						(ref-set s
							saved_s
							)
						)
					(unify_or
						clause
						(rest rules)
						)
					)
				)
			)
		)
	)

(defn proof_clauses
	[clauses]
	(if (= (count clauses) 0)
		true
		(if (unify_or
				(rest (first clauses))
				(get_rules (first clauses))
				)
			(proof_clauses (rest clauses))
			false
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
	(dosync(ref-set s {}))
	(if
		(=(count clauses) 0)
		'()
		(let [result (unify_and clauses)]
			(println (deref s))
			result
			)
		)
	)


(<- (male george))
(<- (append (list) T T))
(<- (append (list H T) L2 (list H TR)) (append T L2 TR))
;(<- (parent Parent Child) (father Parent Child))
;(<- (parent Parent Child) (mother Parent Child))
(<- (parent b c))
(<- (parent a b))

(<- (gp A C) (parent A B) (parent B C))

(println "")
(println '(?- (parent X c)))
(?- (parent X c))

(println "")
(println '(?- (gp X c)))
(?- (gp X c))

(println "")
(println '(?- (gp X Y)))
(?- (gp X Y))


;(println (symbol (str var1 1)))

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




