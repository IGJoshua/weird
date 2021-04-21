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
