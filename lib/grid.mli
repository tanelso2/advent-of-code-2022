type 'a t

type loc = int * int

val empty_grid : int -> int -> 'a -> 'a t

val width : _ t -> int

val height : _ t -> int

val parse : string -> (char -> 'a) -> 'a t

val to_string : 'a t -> ('a -> char) -> string

val upper_right : _ t -> loc

val get_space : 'a t -> loc -> 'a option

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (loc -> 'a -> unit) -> 'a t -> unit

val copy : 'a t -> 'a t

val count : ('a -> bool) -> 'a t -> int

val collect : ('a -> bool) -> 'a t -> ('a * loc) list

val collecti : (loc -> 'a -> bool) -> 'a t -> ('a * loc) list