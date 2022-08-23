(ns redplanetlabs.core
  (:require [clojure.pprint]
            [clojure.string :as string])
  (:refer-clojure :exclude [pop]))

(defrecord State [stack vars])

(defn new-state []
  (State. (list) {}))

(def if-sym 'if>)
(def else-sym 'else>)
(def loop-sym 'loop>)
(def break-sym 'break>)
(def pop-sym '<pop>)
(def invoke-sym 'invoke>)
(def call-sym 'call>)
(def fn-sym 'fn>)
(def continue-sym 'continue>)

(defn debug
  "Prints out the state before returning it"
  [state]
  (doto state println))

(def ^:dynamic *debug* false)

(defmacro -debug>
  "Optionally compile in debug statements to print state during each
   step of execution. *debug* must be set to true before
   macroexpansion, so can either redef it or eg (binding [*debug*
   true] (eval '(defstackfn ...)))"
  [& body]
  (let [body (if *debug*
               (interpose `debug body)
               body)]
    `(-> ~@body)))

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

(defn pop-item
  "Returns pair of [state, ToS] where state's stack has ToS popped
   off. Throws error on empty stack."
  [{:keys [stack] :as state}]
  (if (seq stack)
    (let [tos (-> state :stack first)
          newstate (update state :stack rest)]
      [newstate tos])
    (throw-error "Stack underflow")))

(defn pop
  "Drops the top item on the stack"
  [state]
  (first (pop-item state)))

(defn compile-symbol
  "Emits code that handles symbols (variable get/set, pop)"
  [sym]
  (condp = (-> sym name symbol) ;; remove namespacing
    pop-sym `(pop)
    continue-sym '(recur) ;; normalization guarantees this will be only

    (if-let [var (parse-var-reference sym)]
      (let [[var set?] var]
        `(~(if set? `set-var `get-var) (quote ~var)))
      (if (= sym break-sym)
        (throw-error "break> is only allowed in conditionals inside a loop")
        (throw-error "Unknown variable or symbol: %s" (name sym))))))

(defn invoke-instance-method
  "Returns a function that invokes the given method on the first of
  its args and passes in the rest of the args to that method."
  [method-name]
  (fn [& args]
    (clojure.lang.Reflector/invokeInstanceMethod
     (first args)
     method-name
     (into-array Object (rest args)))))

(defn invoke-static-method
  "Returns a function that invokes the given static method (in the
  form 'Class/staticMethod')"
  [method-name]
  (let [[clazz method] (string/split method-name #"/")
        clazz (-> clazz symbol resolve)]
    (fn [& args]
      (clojure.lang.Reflector/invokeStaticMethod
       clazz
       method
       (into-array Object args)))))

(defn invoke-constructor
  "Returns a function that invokes the given constructor"
  [clazzname]
  (fn [& args]
    (clojure.lang.Reflector/invokeConstructor
     (class clazzname)
     (into-array Object args))))

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

(defn compile-java-interop
  [sym]
  (let [s (str sym)]
    (cond
      (.startsWith s ".") `(invoke-instance-method (subs ~s 1))
      (some #{\/} s) `(invoke-static-method ~s)
      (.endsWith s ".") `(invoke-constructor (->> ~s butlast (apply str)))
      :else (throw-error "Unknown symbol: %s" (str sym)))))

(defn compile-invoke
  "Emits code for invoke>"
  [[f arity]]
  (when (or (not (integer? arity)) (neg? arity))
    (throw-error "Invalid arity: %s" arity))
  (when-not (symbol? f)
    (throw-error
     "Invalid invocation, function or method must be a symbol"
     (str f)))
  `(invoke ~(if (some-> f resolve var?)
              f
              (compile-java-interop f))
           ~arity))

(defmacro threaded-if
  "Expands from an expression that fits into the threading macro ->
   (in other words, takes the state as the first arg), to a let/if
   that returns the new state"
  [state if-clause else-clause]
  `(let [[newstate# tos#] (pop-item ~state)]
     (if tos#
       (-debug> newstate# ~@if-clause)
       (-debug> newstate# ~@else-clause))))

(declare compile-item)

(defn split-if
  "Splits an if> body into a pair, [if-clause, else-clause]"
  [items]
  (let [[if-clause [_ & else-clause]] (split-with #(not= % else-sym) items)]
    [if-clause else-clause]))

(defn compile-if
  "Emits code for if> else>"
  [items]
  (let [[if-clause else-clause] (split-if items)]
    `(threaded-if ~(map compile-item if-clause)
                  ~(map compile-item else-clause))))

(defn check-break-if-clause
  "Checks if the clause has break/continue. Returns true if it is
  last, false if not present, and throws an exception if present but
  not last"
  [clause]
  (let [kws #{'break> 'continue>}]
    (if (some kws clause)
      (if (kws (last clause))
        true
        (throw-error "Break and continue must be last in clause"))
      false)))

(defn if?
  "Returns true if the stack item is an if> expression"
  [item]
  (and (seq? item)
       (-> item first (= if-sym))))

(defn add-loop-continue
  "Adds a loop continue if it's not already there"
  [body]
  (cond-> body
    (-> body last (not= 'continue>)) (-> vec (conj 'continue>) list*)))

(defn shift-loop-breaks
  "If a loop body contains an `if` that contains a 'break/continue' in
   one branch, move the rest of the body following the `if` into the
   other branch (since that code is only reachable when that branch is
   chosen), then recurse into that joined branch. Throws exception if
   break/continue isn't in tail position"
  [body]
  (let [[up-to-if [[_ & the-if] & remaining]] (split-with #(not (if? %)) body)]
    (if the-if
      (let [[if-clause else-clause :as clauses] (split-if the-if)
            [break-cont-if? break-cont-else?] (map check-break-if-clause clauses)]

        (if (not= break-cont-if? break-cont-else?) 

          (list* (conj (vec up-to-if)
                       (remove #{break-sym}
                               (apply concat
                                      [if-sym]
                                      (if break-cont-if?
                                        [if-clause [else-sym]
                                         (shift-loop-breaks (concat else-clause remaining))]
                                        [(shift-loop-breaks (concat if-clause remaining))
                                         [else-sym] else-clause])))))
          (if break-cont-if?

            body

            (add-loop-continue body))))

      (add-loop-continue body))))

(defmacro threaded-loop
  "Expands from an expression that fits into the threading macro ->
   (in other words, takes the state as the first arg), to a let/loop
   that returns the new state"
  [state body]
  `(loop [state# ~state]
     (let [[newstate# tos#] (pop-item state#)]
       (if tos#
         (-debug> newstate# ~@body)
         newstate#))))

(defn compile-loop
  "Compiles a loop, which checks the ToS for truthy to
  continue. Supports 'continue' and 'break'"
  [body]
  `(threaded-loop ~(map compile-item (shift-loop-breaks body))))

(defn remove-ampersand
  "Removes the & from a list of values"
  [values]
  (remove #(= % '&) values))

(defmacro threaded-fn
  "Expands from an expression that fits into the threading macro ->
   (in other words, takes the state as the first arg), to a let/fn
   that returns the new state"
  [state bodies]
  (let [orig (gensym "orig")]
    `(let [~orig ~state]
       (push ~orig
             (fn
               ~@(for [[vars body] bodies]
                   `([state# ~@(vec vars)]
                     (let [captured# (update state# :vars merge (:vars ~orig))]
                       (assoc (-debug> captured#
                                       (assign-initial-vars

                                        (quote ~(remove-ampersand vars))


                                        ~(vec (remove-ampersand vars)))
                                       ~@body)
                              :vars

                              (:vars state#))))))))))

(defn compile-fn
  "Emits code for an anonymous function object"
  [bodies]
  `(threaded-fn ~(for [[vars & body] bodies]
                   [vars (map compile-item body)])))

(defn call
  "Calls anonymous function on ToS, puts return value onto
  stack. Function and its args are consumed."
  [state args]
  (let [[state tos] (pop-item state)]
    (if (fn? tos)
      (apply tos state args)
      (throw-error "Call attempted on value that isn't a function: %s", (pr-str tos)))))

(defn compile-list
  "Emits code for a list item (if or invoke)"
  [[function & args]]
  (condp = (-> function name symbol) ;; strip namespace
    if-sym (compile-if args)
    invoke-sym (compile-invoke args)
    loop-sym (compile-loop args)
    fn-sym (compile-fn args)
    call-sym `(call (quote ~args))
    (throw-error "Unknown function: %s" function)))

(defn compile-item
  "Returns a code snippet that executes the given item inside a
  defstackfn. the form is always one that fits into the -> macro. In
  other words, the first argument to whatever function is called is
  left out. It will be filled in inside the -> macro with a form that
  returns the state that this item needs."
  [item]
  (cond
    (and (sequential? item)
         (not (vector? item))) (compile-list item)
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
  (update state :vars merge
          (let [;; discard the leading ! from var names
                vars (map (comp first parse-var-reference) vars)]

            (into {} (map vector vars values)))))

(defn stackfn-body
  "Generates the body of a defn and places the compiled program in
   it. Args-sym is the symbol for the arguments to defstackfn in the
   defn's arglist."
  [args-sym initial-vars program]
  `(-debug> (new-state)
            (assign-initial-vars (quote ~initial-vars) ~args-sym)
            ~@(map compile-item program)
            :stack))

(defmacro defstackfn*
  "Compile a function with the name name-sym, and a list of variable
  names to assign at runtime to the arguments passed to the
  function. The function accepts a variable number of args and returns
  the entire stack (helpful for debugging)."
  [name-sym initial-vars & program]
  (let [args-sym (gensym "args")]
    `(defn ~(-> name-sym name symbol) [& ~args-sym]
       ~(stackfn-body args-sym initial-vars program))))

(defmacro defstackfn
  "Compile a function with the name name-sym, and a list of variable
  names to assign at runtime to the arguments passed to the
  function. The function accepts a variable number of args and returns
  the top stack item (or nil if empty)."
  [name-sym initial-vars & program]
  (let [args-sym (gensym "args")]
    `(defn ~(-> name-sym name symbol) [& ~args-sym]
       (first ~(stackfn-body args-sym initial-vars program)))))
