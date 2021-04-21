(ns weird.circuit
  (:require
   [weird.circuit.proto :as proto]))

(defrecord FunctionalChip [fun in-count out-count output]
  proto/Chip
  (stable? [_ next-input]
    (= output (fun next-input)))
  (step [this in]
    (assoc this
           :output (fun in)))
  (output [_] output)
  (num-inputs [_] in-count)
  (num-outputs [_] out-count))

(defn functional-chip
  "Constructs a [[proto/Chip]] that wraps a pure function.

  The function must take a single vector and return a single vector, both must
  contain only booleans."
  ([f in-count out-count]
   (functional-chip f in-count out-count (vec (repeat out-count false))))
  ([f in-count out-count init-output]
   (->FunctionalChip f in-count out-count init-output)))

(defn or-chip
  "Constructs a [[proto/Chip]] implementing logical or."
  ([] (or-chip 2))
  ([num-in]
   (or-chip num-in false))
  ([num-in init-out]
   (functional-chip (comp vector boolean (partial some true?))
                    num-in 1 [init-out])))

(defn and-chip
  "Constructs a [[proto/Chip]] implementing logical and."
  ([] (and-chip 2))
  ([num-in]
   (and-chip num-in false))
  ([num-in init-out]
   (functional-chip (comp vector boolean (partial every? true?))
                    num-in 1 [init-out])))

(defn not-chip
  "Constructs a [[proto/Chip]] implementing logical negation."
  ([] (not-chip true))
  ([init-out]
   (functional-chip (comp vector boolean (partial apply not))
                    1 1 [init-out])))

(defn xor-chip
  "Constructs a [[proto/Chip]] implementing logical xor.

  When more than two inputs are included, this chip will activate when exactly
  one input is true."
  ([] (xor-chip 2))
  ([num-in] (xor-chip num-in false))
  ([num-in init-out]
   (functional-chip (comp vector boolean (partial = 1) count (partial filter true?))
                    num-in 1 [init-out])))

(defn nor-chip
  "Constructs a [[proto/Chip]] implementing logical nor."
  ([] (nor-chip 2))
  ([num-in] (nor-chip num-in true))
  ([num-in init-out]
   (functional-chip (comp vector not (partial some true?))
                    num-in 1 [init-out])))

(defn nand-chip
  "Constructs a [[proto/Chip]] implementing logical nand."
  ([] (nand-chip 2))
  ([num-in] (nand-chip num-in false))
  ([num-in init-out]
   (functional-chip (comp vector not (partial every? true?))
                    num-in 1 [init-out])))

(defn- calculate-chip-inputs
  "Constructs a map from chip keys to input vectors for simulating a step."
  [circuit new-inputs]
  (reduce-kv
   (fn [m k v]
     (assoc m k (map :value (sort-by :index v))))
   {}
   (group-by :input
             (concat (for [[[out-chip out-port] [in-chip in-port]] (:connections circuit)]
                       {:input in-chip
                        :value (nth (proto/output (get (:chips circuit) out-chip)) out-port)
                        :index in-port})
                     (for [[in-chip out-port in-port] (:inputs circuit)]
                       {:input in-chip
                        :value (nth new-inputs out-port)
                        :index in-port})))))

(defrecord Circuit [chips connections inputs outputs stored-output]
  proto/Chip
  (stable? [this new-inputs]
    (let [chip-inputs (calculate-chip-inputs this new-inputs)]
      (every? #(proto/stable? (val %) (get chip-inputs (key %))) chips)))
  (step [this new-inputs]
    (let [chip-inputs (calculate-chip-inputs this new-inputs)
          new-chips (reduce-kv
                     (fn [m k v]
                       (assoc m k (if (proto/stable? v (get chip-inputs k))
                                    v
                                    (proto/step v (get chip-inputs k)))))
                     {}
                     chips)
          new-output (vec
                      (for [[out-chip out-port] outputs]
                        (nth (proto/output (get new-chips out-chip)) out-port)))]
      (assoc this
             :chips new-chips
             :stored-output new-output)))
  (output [_]
    stored-output)
  (num-inputs [_]
    (count (set (map second inputs))))
  (num-outputs [_]
    (count outputs)))

(defn add-chip
  "Adds the `chip` into the `circuit` with the given `key` id."
  [circuit chip key]
  (assoc-in circuit [:chips key] chip))

(defn remove-chip
  "Removes the `chip` from the `circuit`, removing any latent connections."
  [circuit key]
  (update
   (update
    (update
     (update circuit :chips dissoc key)
     :inputs
     (partial filter (comp #{key} first)))
    :connections
    (partial filter
             (fn [connection]
               (#{(get-in connection [0 0])
                  (get-in connection [1 0])}
                key))))
   :outputs
   (partial filter (comp #{key} first))))

(defn connect-chips
  "Adds a connection from the `source-chip` to the `target-chip`."
  [circuit source-chip source-port target-chip target-port]
  (update circuit :connections conj [[source-chip source-port] [target-chip target-port]]))

(defn disconnect-chips
  "Disconnects the port from the `source-chip` to the port in the `target-chip`."
  [circuit source-chip source-port target-chip target-port]
  (update circuit :connections (comp vec remove)
          #{[[source-chip source-port] [target-chip target-port]]}))

(defrecord Delay [chip ticks stored-inputs]
  proto/Chip
  (stable? [_ new-inputs]
    (and (apply = new-inputs stored-inputs)
         (proto/stable? chip (peek stored-inputs))))
  (step [this new-inputs]
    (assoc this
           :chip (cond-> chip
                   (>= (inc (count stored-inputs)) ticks) (proto/step (peek stored-inputs)))
           :stored-inputs (cond-> (conj stored-inputs new-inputs)
                            (>= (inc (count stored-inputs)) ticks) (pop))))
  (output [_]
    (proto/output chip))
  (num-inputs [_]
    (proto/num-inputs chip))
  (num-outputs [_]
    (proto/num-outputs chip)))

(defn make-delay
  "Returns a [[proto/Chip]] which reacts to the input after a delay.

  The `inputs` should be a collection of up to `ticks` - 1 input vectors
  representing the recent inputs."
  ([chip ticks] (make-delay chip ticks nil))
  ([chip ticks inputs]
   (->Delay chip ticks (into (PersistentQueue/EMPTY) inputs))))

(defrecord AcceleratedChip [chip ticks]
  proto/Chip
  (stable? [_ new-inputs]
    (proto/stable? chip new-inputs))
  (step [this new-inputs]
    (update this :chip (partial reduce proto/step) (repeat ticks new-inputs)))
  (output [_]
    (proto/output chip))
  (num-inputs [_]
    (proto/num-inputs chip))
  (num-outputs [_]
    (proto/num-outputs chip)))

(defn accelerate-chip
  "Returns a modified version of `chip` that simulates a number of `ticks` per step."
  [chip ticks]
  (->AcceleratedChip chip ticks))
