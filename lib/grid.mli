type 'a t

type loc = int * int

val empty_grid : int -> int -> 'a -> 'a t

val width : _ t -> int

val height : _ t -> int

val parse : string -> (char -> 'a) -> 'a t

val parsei: string -> (loc -> char -> 'a) -> 'a t

val to_string : 'a t -> ('a -> char) -> string

val upper_right : _ t -> loc

val lower_left : _ t -> loc

val lower_right : _ t -> loc

val get_space : 'a t -> loc -> 'a option

val get_space_exn : 'a t -> loc -> 'a

val set_space : 'a t -> 'a -> loc -> unit

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (loc -> 'a -> unit) -> 'a t -> unit

val copy : 'a t -> 'a t

val count : ('a -> bool) -> 'a t -> int

val collect : ('a -> bool) -> 'a t -> ('a * loc) list

val collecti : (loc -> 'a -> bool) -> 'a t -> ('a * loc) list

val rowsi : 'a t -> (loc * 'a) list list

val rows : 'a t -> 'a list list

val columnsi : 'a t -> (loc * 'a) list list

val columns : 'a t -> 'a list list

val to_left_of : 'a t -> loc -> (loc * 'a) list
val to_right_of : 'a t -> loc -> (loc * 'a) list
val above : 'a t -> loc -> (loc * 'a) list
val below : 'a t -> loc -> (loc * 'a) list

val line_from : 'a t -> loc -> loc -> (loc * 'a) list

val get_cardinal_neighbors : 'a t -> loc -> (loc * 'a) list