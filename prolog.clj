(ns prolog)

(def database (ref {}))
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


(defn set_bindings
	"Resets the bindings with the `values'."
	[values]
	(dosync
		(ref-set
			bindings
			values)))

(defn add_bindings
	"Adds a binding."
	[key value]
	(set_bindings
		(merge
			(deref bindings)
				{key value})))

(defn remove_bindings
	"Removes a binding."
	[key]
	(set_bindings
		(dissoc (deref bindings) key)))

(defn vari?
	"True if word is an variable in prolog."
	[word]
	(Character/isUpperCase (first (str word))))

(defn val?
	"True if word is an atom in prolog."
	[word]
	(Character/isLowerCase (first (str word))))

(defn rename_arg
	[arg]
	(if (val? arg)
		arg
		(if (or (in? (vals (deref bindings)) arg)
				(contains? (deref bindings) (keyword arg)))
			(rename_arg (symbol (str (str arg) "*")))
			arg)))

(defn rename_args
	[args]
	(if (= 0 (count args))
		'()
		(concat
			(list (rename_arg (first args)))
			(rename_args (rest args)))))

(defn rename_clauses
	[clauses]
	(if (= 0 (count clauses))
		'()
		(concat
			(list (rename_args (first clauses)))
			(rename_clauses(rest clauses)))))

(defn rename_rules
	[rules]
	(if (= 0 (count rules))
		'()
		(concat
			(list (rename_clauses (first rules)))
			(rename_rules(rest rules)))))

(defn in? 
	"True if coll contains elm.
	Source: http://stackoverflow.com/a/3249777/4203437"
	[coll elm]  
	(some
		#(= elm %) coll))

(defn add_rule
	"Adds a new rule to the database."
	[head body]
	(dosync
		(ref-set
			database
			(merge-with concat
				(deref database)
				{(keyword (first head))
					(list
						(cons
							(rest head)
							body))})))
	nil)

(defmacro <- 
	"Defines new rules and facts.
	The first clause after is the head, the rest is the body (nothing for simple facts).
	The head may contain any kind of structure (functors, atoms, ...), not only variables."
	[head & body]
	(add_rule
		head
		body))

(defn get_rules
	"Returns the rules associated to the `clause'."
	[clause]
	(rename_rules
		(get
			(deref database)
			(keyword (first clause)))))

(defn key_of_val
	"Returns the key related to the value `val' in the `bindings'."
	[val keys]
	(if (or (= keys nil) (= 0 (count keys)))
		nil
		(if (= (get (deref bindings) (first keys))
				val)
			(first keys)
			(key_of_val val (rest keys)))))

(defn get_last_value
	"Returns the last value of a key by removing the intermediate bindings.
	Example: in {:A X, :X F, :F R}, the last value of X is R.
			{:A X, :X F, :F R} became {:A X} and R is returned."
	[value]
	(if (contains? (deref bindings) (keyword value))
		(let [value2 (get (deref bindings) (keyword value))]
			(remove_bindings (keyword value))
			(get_last_value value2))
		value))

(defn clean_key
	"Cleans a specific `key'."
	[key]
	(let [value (get (deref bindings) key)]
		(if (or (= nil value) (val? value))
			nil
			(let [new_value (get_last_value value)]
				(add_bindings key new_value)
				nil))))

(defn clean_keys
	"Cleans the bindings to eliminate the intermediate variables.
	Example: {:A X, :X F, :F R} becomes {:A R}.
	Apply the cleaning on all the `keys'."
	[keys]
	(if (or (= nil keys) (= 0 (count keys)))
		nil
		(do
			(clean_key (first keys))
			(clean_keys (rest keys)))))

(defn consistent
	"True if the bindings are consistent.
		* {:a b, :X Y} is not consistent because a is not equal to b?
		* {:A a, :b B} is consistent."
	[keys]
	(if (or (= nil keys)
			(= 0 (count keys)))
		true
		(if (and
				(val? (.substring (str (first keys)) 1))
				(val? (get (deref bindings) (first keys)))
				(not=
					(first keys)
					(keyword (get (deref bindings) (first keys)))))
			false
			(consistent (rest keys)))))

(defn can_be_bound
	"True if the binding of var1 and var2 do not create duplicate values in the `bindings'."
	[var1 var2]
	(if (contains? (deref bindings) (keyword var1))
		(if (and
				(vari? var1)
				(vari?  var2))
			true
			false)
		(if (in? (vals (deref bindings)) var2)
			false
			true)))

(defn match
	"Adds the binding {:var1 var2}
	True only if the bindings are consistent."
	[var1 var2]
	(if (= (get (deref bindings) (keyword var1)) var2)
		true
		(if (can_be_bound var1 var2)
			(do
				(add_bindings (keyword var1) var2)
				(clean_keys (keys (deref bindings)))
				(consistent (keys (deref bindings))))
			false)))

(defn create_match
	"True if var1 and var2 have been match.
	False if var1 and var2 cannot be match."
	[var1 var2]
	(if (and (val? var1) (val? var2))
	 	(if (= var1 var2)
	 		true
	 		false)
	 	(if (and (vari? var1) (vari? var2))
	 		(match var1 var2)
	 		(if (and (vari? var1) (val? var2))
	 			(match var1 var2)
	 			(if (and (val? var1) (vari? var2))
	 				(match var1 var2)
	 				false)))))

(defn create_match_list
	"Creates the bindings between list1 and list2.
	* true if it can be bound
	* false otherwise"
	[list1 list2]
	(if (= (count list1) 0)
		true
		(if
			(create_match
				(first list1)
				(first list2))
			(create_match_list
				(rest list1)
				(rest list2))
			false)))

(defn unify
	"Tests if the clause can be unified with the rule."
	[clause rule]
	(if-not (= (count clause)
				(count (first rule)))
		false
		(let [saved_bindings (deref bindings)]
			(if (create_match_list clause (first rule))
				true
				(do
					(set_bindings saved_bindings)
					false)))))

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
					(rest clauses))
				true
				(do
					(set_bindings saved_bindings)
					false)))))

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
						(first rules))
					(prolog_and
						(rest (first rules)))
					(prolog_and
						nexts))
				true
				(do
					(set_bindings saved_bindings)
					(test_rules
						clause
						(rest rules)
						nexts))))))

(defn to_keys
	"Transform the list of `vars' in list of keys and remove the vars that are considered as atoms in prolog."
	[vars]
	(if (or (= nil vars) (= 0 (count vars)))
		nil
		(if (vari? (first vars))
			(concat
				(list (keyword (first vars)))
				(to_keys (rest vars)))
			(to_keys (rest vars)))))

(defn unknow_var
	"Return the keys of the unknowns variables off the list of `clauses'."
	[clauses]
	(if (or (= nil clauses)
			(= 0 (count clauses)))
		nil
		(distinct (concat
			(to_keys (first clauses))
			(unknow_var (rest clauses))))))

(defn filter_unknow
	"Removes the unwanted `bindings' to keep only the relevant ones."
	[vars key_s]
	(if (or (= nil key_s) (= 0 (count key_s)))
		nil
		(if-not (in? vars (first key_s))
			(do
				(remove_bindings (first key_s))
				(filter_unknow vars (rest key_s)))
			(filter_unknow vars (rest key_s)))))

(defmacro ?-
	"Runs queries against the knowledge base.
	It takes a variable number of goal clauses.
	The effect is the same as separating the clauses with a comma in Prolog"
	[& clauses]
	(set_bindings {})
	(if
		(=(count clauses) 0)
		'()
		(let [result (prolog_and clauses) unknow (unknow_var clauses)]
			(filter_unknow unknow (keys (deref bindings)))
			(printf "Call:     %s\n" clauses)
			(printf "Result:   %s\n" result)
			(printf "Bindings: %s\n" (deref bindings))
			(println)
			(let [binds (deref bindings)]
				(set_bindings {})
				[result (str binds)]))))

