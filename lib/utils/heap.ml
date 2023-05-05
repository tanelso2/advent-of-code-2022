let child_indexes i =
  (2*i+1, 2*i+2)

let parent_index i =
  (i - 1) / 2

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

module Make (K: HeapKey) = struct
  type v = K.t
  type 'a t = {
    mutable data: (('a * v) option) array;
  }
  let create ?(size = 16) () =
    let a = Array.init size (fun _ -> None) in
    {
    data = a;
    }

  let resize (h: 'a t) : unit =
    let len = Array.length h.data in
    let len' = 2 * len + 1 in
    let data' = Array.init len' (fun i -> if i < len then h.data.(i) else None) in
    h.data <- data'

  let size (h: 'a t) : int =
    Base.Array.count ~f:Option.is_some h.data

  module KeyCompare = Base.Comparable.Make(K)

  module KeyOptionCompare = Base.Comparable.Make(struct
    open! Base
    type t = K.t option [@@deriving sexp_of]

    let compare a b =
      match (a,b) with
      | None, Some _ -> 1
      | Some _, None -> -1
      | Some x, Some y -> K.compare x y
      | None, None -> 0
  end)

  let compare_vals cv lv rv =
    let open KeyOptionCompare in
    if cv <= lv && cv <= rv
    then `Cv
    else if lv < cv && lv <= rv
    then `Lv
    else `Rv

  let get_v (x:('a * v) option) : v option = Option.map snd x

  let get_i h i = 
    let len = Array.length h.data in
    if i >= len
    then None
    else Array.get h.data i

  let rec balance_down (h: 'a t) (i: int) : unit =
    let li,ri = child_indexes i in
    let curr = get_i h i in
    let lc = get_i h li in
    let rc = get_i h ri in
    let lv = get_v lc in
    let rv = get_v rc in
    let cv = get_v curr in
    match compare_vals cv lv rv with
    | `Cv -> ()
    | `Rv -> begin
      Array.set h.data i rc;
      Array.set h.data ri curr;
      balance_down h ri 
    end
    | `Lv -> begin
      Array.set h.data i lc;
      Array.set h.data li curr;
      balance_down h li
    end

  let pop (h: 'a t) : 'a option =
    let x = Array.get h.data 0 in
    Array.set h.data 0 None;
    balance_down h 0;
    Option.map fst x

  let peek (h: 'a t) : 'a option =
    let x = Array.get h.data 0 in
    Option.map fst x

  let rec balance_up (h: 'a t) (i:int) : unit =
    if i = 0
    then ()
    else
      let should_replace cv pv = 
        let open KeyOptionCompare in
        cv < pv
      in
      let pi = parent_index i in
      let p = get_i h pi in
      let pv = get_v p in
      let c = get_i h i in
      let cv = get_v c in
      if should_replace cv pv
      then begin
        Array.set h.data i p;
        Array.set h.data pi c;
        balance_up h pi;
      end
      else ()

  let rec add (h: 'a t) (x:'a) (v: v) : unit =
    let insert_i = 
      Base.Array.findi ~f:(fun _ x -> Option.is_none x) h.data 
      |> Option.map fst
    in
    match insert_i with
    | Some i -> begin
      Array.set h.data i (Some (x,v));
      balance_up h i
    end
    | None -> begin
      resize h;
      add h x v
    end

  let add_all (h: 'a t) (vs: ('a * v) list) : unit =
    List.iter (fun (x,v) -> add h x v) vs


  let of_list (l: ('a * v) list) : 'a t =
    let h = create () in
    List.iter (fun (x,v) -> add h x v) l;
    h

  let take_all (h: 'a t) : 'a list =
    let l = ref [] in
    let rec helper () =
      match pop h with
      | None -> List.rev !l
      | Some x ->
        l := (x::!l);
        helper ()
    in
    helper ()

end