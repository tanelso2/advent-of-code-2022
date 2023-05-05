open! Base
open Utils

module IntPairMap = struct
  type k = int * int

  type 'a t = (k, 'a, IntPair.comparator_witness) Map.t

  let empty () = Map.empty (module IntPair)
end

module StringPairMap = struct
  type k = string * string

  type 'a t = (k, 'a, StringPair.comparator_witness) Map.t
  
  let empty () = Map.empty (module StringPair)

end