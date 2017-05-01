;;lein repl
;;(load-file "prolog.clj")
(def memory (ref {}))
(def r (ref []))
(def s (ref {}))
(def var_known (ref []))
(def ren (ref 0))

(declare
	<-
	?-
	unify_or
	unify_and
	unify
	match
	)


(defn concat_vec
	[list1 list2]
	(into [] (concat list1 list2))
	)

(defn rename_arg
	[arg]
	(if (Character/isLowerCase (first (str arg)))
		arg
		(if (in? (vals (deref s)) arg)
			(rename_arg (symbol (str (str arg) "*")))
			arg
			)
		)
	)

(defn rename_args
	[args]
	(if (= 0(count args))
		[]
		(concat_vec
			[(rename_arg (first args))]
			(rename_args (rest args))
			)
		)
	)

(defn rename_clauses
	[clauses]
	(if (= 0(count clauses))
		[]
		(concat_vec
			[(rename_args (first clauses))]
			(rename_clauses(rest clauses))
			)
		)
	)

(defn rename_rules
	[rules]
	(if (= 0(count rules))
		[]
		(concat_vec
			[(into [] (rename_clauses (first rules)))]
			(rename_rules(rest rules))
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
	(rename_rules (get (deref memory) (keyword (first clause))))
	)

(defn can_be_bound
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

(defn rename_key
	[key]
	(if (contains? (deref s) key)
		(rename_key (symbol (str (str key) "*")))
		key
		)
	)

(defn rename_value
	[value]
	(if (Character/isLowerCase (first (str value)))
		value
		(if (in? (vals (deref s)) value)
			(rename_key (symbol (str (str value) "*")))
			value
			)
		)
	)

(defn get_last_value
	[value]
	(if (contains? (deref s) (keyword value))
		(let [value2 (get (deref s) (keyword value))]
			(dosync
				(ref-set
					s
					(dissoc (deref s) (keyword value))
					)
				)
			(get_last_value value2)
			)
		value
		)
	)

(defn clean_key
	[key]
	(let [value (get (deref s) key)]
		(if (or (= nil value) (Character/isLowerCase (first (str value))))
			nil
			(let [new_value (get_last_value value)]
				(dosync
					(ref-set
						s
						(merge
							(deref s)
							{key new_value}
							)
						)
					)
				nil
				)
			)
		)
	)

(defn clean_keys
	[keys]
	(if (or (= nil keys) (= 0 (count keys)))
		nil
		(do
			(clean_key (first keys))
			(clean_keys (rest keys))
			)
		)
	)

(defn match
	[var1 var2]
	(if (= (get (deref s) (keyword var1)) var2)
		true
		(if (can_be_bound var1 var2)
			(do
				(dosync
					(ref-set s
						(merge
							(deref s)
							{(keyword var1) var2}
							)
						)
					)
				(clean_keys (keys (deref s)))
				true
				)
			false
			)
		)
	)

(defn create_match
	[var1 var2]
	(if (and (Character/isLowerCase (first (str var1))) (Character/isLowerCase (first (str var2))))
	 	(if (= var1 var2)
	 		true
	 		false
	 		)
	 	(if (and (Character/isUpperCase (first (str var1))) (Character/isUpperCase (first (str var2))))
	 		(match var1 var2)
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
	(if (or (= 0 (count clauses)) (= nil clauses))
		true
		(let [saved_s (deref s) saved_ren (deref ren)]
			(if
				(unify_or
					(first clauses)
					(get_rules (first clauses))
					(rest clauses)
				)
				true
				(do
					(dosync (ref-set ren saved_ren))
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
	[clause rules nexts]
	(if
		(= 0 (count rules))
		false
		(let [saved_s (deref s) saved_ren (deref ren)]
			(if
				(and
					(unify
						(rest clause)
						(first rules)
						)
					(unify_and
						(rest (first rules))
						)
					(unify_and
						nexts
						)
					)
					true
					(do
						(dosync (ref-set ren saved_ren))
						(dosync
							(ref-set s
								saved_s
								)
							)
						(unify_or
							clause
							(rest rules)
							nexts
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

(<- (parent Parent Child) (father Parent Child))
(<- (parent Parent Child) (mother Parent Child))
(<- (parent b c))
(<- (parent a b))

(<- (gp A C) (parent A B) (parent B C))
(<- (gp s f))

(<- (ami a b))
(<- (ami b c))
(<- (ami c d))
(<- (ami d e))
(<- (ami e f))
(<- (ami A C) (ami A B) (ami B C))

(<- (c a b))
(<- (b A B) (c B A))
(<- (a A B) (b B A))

(println "")
(println '(?- (parent X c)))
(println(?- (parent X c)))

(println "")
(println '(?- (gp X c)))
(println(?- (gp X c)))

(println "")
(println '(?- (gp X Y)))
(println(?- (gp X Y)))

(println "")
(println '(?- (a X Y)))
(println(?- (a X Y)))

(println "")
(println '(?- (ami a d)))
(println(?- (ami a d)))


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




