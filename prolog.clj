;;lein repl
;;(load-file "prolog.clj")

(ns prolog)

(def memory (ref {}))
(def bindings (ref {}))

(declare
	<-
	?-
	test_rules
	prolog_and
	unify
	match
	in?
	)

(defn bindings_add
	[])

(defn vari?
	[word]
	(Character/isUpperCase (first word)))

(defn val?
	[word]
	(Character/isLowerCase (first word)))

(defn concat_vec
	[list1 list2]
	(vec (concat list1 list2)))

(defn rename_arg
	[arg]
	(if (val? (str arg))
		arg
		(if (or (in? (vals (deref bindings)) arg)
				(contains? (deref bindings) (keyword arg)))
			(rename_arg (symbol (str (str arg) "*")))
			arg)))

(defn rename_args
	[args]
	(if (= 0(count args))
		[]
		(concat_vec
			[(rename_arg (first args))]
			(rename_args (rest args)))))

(defn rename_clauses
	[clauses]
	(if (= 0(count clauses))
		[]
		(concat_vec
			[(rename_args (first clauses))]
			(rename_clauses(rest clauses)))))

(defn rename_rules
	[rules]
	(if (= 0(count rules))
		[]
		(concat
			[(rename_clauses (first rules))]
			(rename_rules(rest rules)))))

(defn in? 
	"true if coll contains elm"
	[coll elm]  
	(some
		#(= elm %)
		coll))

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
	nil
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

(defn get_rules
	[clause]
	(rename_rules (get (deref memory) (keyword (first clause))))
	)

(defn key_of_val
	[val keys]
	(if (or (= keys nil) (= 0 (count keys)))
		nil
		(if (= (get (deref bindings) (first keys)) val)
			(first keys)
			(key_of_val val (rest keys))
			)
		)
	)

(defn can_be_bound
	[var1 var2]
	(if (contains? (deref bindings) (keyword var1))
		(if (and (vari? (str var1)) (vari? (str var2)))
			true
			false
			)
		(if (in? (vals (deref bindings)) var2)
			false
			true
			)
		)
	)

(defn rename_key
	[key]
	(if (contains? (deref bindings) key)
		(rename_key (symbol (str (str key) "*")))
		key
		)
	)

(defn rename_value
	[value]
	(if (val? (str value))
		value
		(if (in? (vals (deref bindings)) value)
			(rename_key (symbol (str (str value) "*")))
			value
			)
		)
	)

(defn get_last_value
	[value]
	(if (contains? (deref bindings) (keyword value))
		(let [value2 (get (deref bindings) (keyword value))]
			(dosync
				(ref-set
					bindings
					(dissoc (deref bindings) (keyword value))
					)
				)
			(get_last_value value2)
			)
		value
		)
	)

(defn clean_key
	[key]
	(let [value (get (deref bindings) key)]
		(if (or (= nil value) (val? (str value)))
			nil
			(let [new_value (get_last_value value)]
				(dosync
					(ref-set
						bindings
						(merge
							(deref bindings)
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

(defn consistent
	[keys]
	(if (or (= nil keys) (= 0 (count keys)))
		true
		(if (and (val? (rest (str (first keys)))) (val? (str (get (deref bindings) (first keys)))) (not= (first keys) (keyword (get (deref bindings) (first keys)))))
			false
			(consistent (rest keys))
			)
		)
	)

(defn match
	[var1 var2]
	(if (= (get (deref bindings) (keyword var1)) var2)
		true
		(if (can_be_bound var1 var2)
			(do
				(dosync
					(ref-set bindings
						(merge
							(deref bindings)
							{(keyword var1) var2}
							)
						)
					)
				(clean_keys (keys (deref bindings)))
				(consistent (keys (deref bindings)))
				)
			false
			)
		)
	)

(defn create_match
	[var1 var2]
	(if (and (val? (str var1)) (val? (str var2)))
	 	(if (= var1 var2)
	 		true
	 		false
	 		)
	 	(if (and (vari? (str var1)) (vari? (str var2)))
	 		(match var1 var2)
	 		(if (and (vari? (str var1)) (val? (str var2)))
	 			(match var1 var2)
	 			(if (and (val? (str var1)) (vari? (str var2)))
	 				(match var1 var2)
	 				false
	 				)
	 			)
	 		)
	 	)
	)

(defn create_match_list
	"Create the bindings between list1 and list2.
	* true if it can be bound
	* false otherwise"
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
	"Test if the clause can be unified with the rule."
	[clause rule]
	(if-not (= (count clause) (count (first rule)))
		false
		(let [saved_bindings (deref bindings)]
			(if (create_match_list clause (first rule))
				true
				(do
					(dosync
						(ref-set bindings
							saved_bindings
							)
						)
					false
					)
				)
			)
		)
	)

(defn prolog_and
	"The clauses must all be satisfied, there are tested one by one:
	* If there is no more clause to test, this return true
	* To satisfy a clause, it must find a rule in the database that satisfy:
		1- the first clause itself
		2- the rest of the clauses
	* If there are no rules that satisfy at least one clause, this return false."
	[clauses]
	(if (or (= 0 (count clauses)) (= nil clauses))
		true
		(let [saved_bindings (deref bindings)]
			(if
				(test_rules
					(first clauses)
					(get_rules (first clauses))
					(rest clauses)
				)
				true
				(do
					(dosync
						(ref-set bindings
							saved_bindings
							)
						)
					false
					)
				)
			)
		)
	)

(defn test_rules
	"The clause clause is tested with the rules one by one:
	* When a unification is possible between a rule and the clause,
		1- the bindings are modified in the variable `bindings'
		2- the conditions of the rule must be satisfied (prolog_and)
		3- and the nexts clauses must also be satisfied (prolog_and).
	* Or else, the `bindings' is reset to his previous value
	and the next rule is tested.
	* If there is no more rule to test, this return false."
	[clause rules nexts]
	(if
		(= 0 (count rules))
		false
		(let [saved_bindings (deref bindings)]
			(if
				(and
					(unify
						(rest clause)
						(first rules)
						)
					(prolog_and
						(rest (first rules))
						)
					(prolog_and
						nexts
						)
					)
					true
					(do
						(dosync
							(ref-set bindings
								saved_bindings
								)
							)
						(test_rules
							clause
							(rest rules)
							nexts
						)
					)
				)
			)
		)
	)

(defn to_keys
	[vars]
	(if (or (= nil vars) (= 0 (count vars)))
		nil
		(if (vari? (str (first vars)))
			(concat
				[(keyword (first vars))]
				(to_keys (rest vars))
				)
			(to_keys (rest vars))
			)
		)
	)

(defn concat_in
	[lists]
	(if (or (= nil lists) (= 0 (count lists)))
		nil
		(concat
			(to_keys (first lists))
			(concat_in (rest lists))
			)
		)
	)

(defn keep_unknow
	[vars key_s]
	(if (or (= nil key_s) (= 0 (count key_s)))
		nil
		(if-not (in? vars (first key_s))
			(do
				(dosync
					(ref-set
						bindings
						(dissoc (deref bindings) (first key_s))
						)
					)
				(keep_unknow vars (rest key_s))
				)
			(keep_unknow vars (rest key_s))
			)
		)
	)

(defmacro ?-
	"Runs queries against the knowledge base.
	It takes a variable number of goal clauses.
	The effect is the same as separating the clauses with a comma in Prolog"
	[& clauses]
	(dosync(ref-set bindings {}))
	(if
		(=(count clauses) 0)
		'()
		(let [result (prolog_and clauses) unknow (distinct (concat_in clauses))]
			(keep_unknow unknow (keys (deref bindings)))
			(printf "Call:     %s\n" clauses)
			(printf "Result:   %s\n" result)
			(printf "Bindings: %s\n" (deref bindings))
			(println)
			(let [binds (deref bindings)]
				(dosync(ref-set bindings {}))
				[result (str binds)]
				)
			)
		)
	)

