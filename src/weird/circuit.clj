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
