(ns weird.circuit.proto
  "Protocols required to implement the circuit simulator.")

(defprotocol Chip
  "Protocol for pieces of circuits."
  :extend-via-metadata true
  (stable? [chip new-inputs]
    "Checks if the chip is stable given a set of new inputs.")
  (step [chip new-inputs]
    "Steps the chip with the given new inputs.
    Returns an updated version of the chip.")
  (output [chip]
    "Returns the current output vector for the chip.")
  (num-inputs [chip]
    "Returns the number of inputs expected by this chip.")
  (num-outputs [chip]
    "Returns the number of outputs produced by this chip."))
