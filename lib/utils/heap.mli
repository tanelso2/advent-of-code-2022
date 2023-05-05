module type HeapKey = sig
  type t
  val compare : t -> t -> int
  val sexp_of_t : t -> Base.Sexp.t
end

module type HeapType = sig
  type v
  type 'a t
  val create : ?size:int -> unit -> 'a t
  val size : 'a t -> int
  val pop : 'a t -> 'a option
  val peek : 'a t -> 'a option
  val add : 'a t -> 'a -> v -> unit
  val add_all : 'a t -> ('a * v) list -> unit
  val of_list : ('a * v) list -> 'a t
  val take_all : 'a t -> 'a list
end

module Make (K: HeapKey) : (HeapType with type v = K.t)