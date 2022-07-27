(ns redplanetlabs.core)

(defrecord State [stack vars])

(defn new-state []
  (State. (list) {}))

(defn parse-var-reference
  "Returns a pair or nil. In the pair, the first item is a symbol for
   the variable being referred to (either a get '!v' or set '!v+'
   operation). The second item is a boolean, true for a 'set'
   operation and false for a 'get' operation. Returns nil if it's not
   a proper reference.

   eg !v => [v false], !v+ => [v true], v => nil"
  [sym]
  (let [[_ var set?] (re-find #"^!([^+]+)(\+?)$" (name sym))
        set? (boolean (seq set?))]
    (and var [(symbol var) set?])))

(defn push
  "Push val onto the stack, return the state"
  [state val]
  (update state :stack conj val))

(defn compile-constant
  "Pushes the constant onto the stack"
  [constant]
  `(push ~constant))

(defn throw-error
  "Throws an exception with the format args applied to the given
   format-string as the message"
  [fmt-str & fmt-args]
  (throw (Exception. (apply format fmt-str fmt-args))))

(defn var-lookup
  "Returns the value of a variable or throws exception if it doesn't
  exist"
  [state v]
  (-> state
      :vars
      (get v)
      (or (throw-error "Variable does not exist: %s" (name v)))))

(defn set-var
  "Sets the variable var with the value currently on top of
  stack (tos). Does not change the stack."
  [{:keys [stack] :as state} var]
  (if-let [tos (first stack)]
    (update state :vars assoc var tos)
    (throw-error "Stack underflow setting variable: %s" (name var))))

(defn get-var
  "Gets the variable var and pushes it onto the stack."
  [{:keys [vars] :as state} var]
  (->> var
       (var-lookup state)
       (update state :stack conj)))

(defn pop
  [{:keys [stack] :as state}]
  (if (seq stack)
    (update state :stack rest)
    (throw-error "Stack underflow")))

(defn compile-symbol
  "Emits code that handles symbols (variable get/set, pop)"
  [sym]
  (if (= sym '<pop>)
    `(pop)
    (if-let [var (parse-var-reference sym)]
      (let [[var set?] var]
        `(~(if set? `set-var `get-var) (quote ~var)))
      (throw-error "Unknown variable or symbol: %s" (name sym)))))

(defn invoke
  "Calls f with args taken from the top of the stack. Arity sets the
  number of args to take from the stack. Throws an exception if there
  aren't enough args on the stack."
  [{:keys [stack] :as state} f arity]
  (let [[args stack] (split-at arity stack)]
    (when (not= (count args) arity)
      (throw-error "Stack underflow calling %s" f))
    (->> args
         reverse ;; make the ToS the last argument to the function, so

         (apply f)
         (conj stack)
         (assoc state :stack))))

(defn compile-invoke
  "Emits code for invoke>"
  [[f arity]]
  (when (or (not (integer? arity)) (neg? arity))
    (throw-error "Invalid arity: %s" arity))
  (when-not (symbol? f)
    (throw-error "Invalid invocation, function must be a symbol"
                 (str f)))
  `(invoke ~f ~arity))

(defmacro threaded-if
  "Expands from an expression that fits into the threading macro ->
   (in other words, takes the state as the first arg), to a let/if
   that returns the new state"
  [state if-clause else-clause]
  `(let [tos# (-> ~state :stack first)
         newstate# (update ~state :stack rest)]
     (if tos#
       (-> newstate# ~@if-clause)
       (-> newstate# ~@else-clause))))

(declare compile-item)

(defn compile-if
  "Emits code for if> else>"
  [items]
  (let [[if-clause [_ & else-clause]] (split-with #(not= % 'else>) items)
        compiled-if (map compile-item if-clause)
        compiled-else (map compile-item else-clause)]
    `(threaded-if ~compiled-if ~compiled-else)))

(defn compile-list
  "Emits code for a list item (if or invoke)"
  [[function & args]]
  (case function
    if> (compile-if args)
    invoke> (compile-invoke args)
    (throw-error "Unknown function: %s" function)))

(defn compile-item
  "Returns a code snippet that executes the given item inside a
  defstackfn. the form is always one that fits into the -> macro. In
  other words, the first argument to whatever function is called is
  left out. It will be filled in inside the -> macro with a form that
  returns the state that this item needs."
  [item]
  (cond
    (list? item) (compile-list item)
    (symbol? item) (compile-symbol item)
    true (compile-constant item)))

(defn assign-initial-vars
  "Assigns a value to the initial variables specified in the first
  argument to defstackfn. If there are more variables specified than
  arguments, throw an error. If there are more arguments than
  variables, discard the extra arguments."
  [state vars values]
  (when (> (count vars) (count values))
    (throw-error "Not enought arguments to assign all variables"))
  (let [;; discard the leading ! from var names
        vars (map (comp first parse-var-reference) vars)

        kvs (map vector vars values)]
    (update state :vars merge (into {} kvs))))

(defmacro defstackfn
  "Compile a function with the name name-sym, and a list of variable
  names to assign at runtime to the arguments passed to the
  function. The function accepts a variable number of args."
  [name-sym initial-vars & program]
  `(defn ~name-sym [& args#]
     (-> (new-state)
         (assign-initial-vars (quote ~initial-vars) args#)
         ~@(map compile-item program)
         :stack)))
