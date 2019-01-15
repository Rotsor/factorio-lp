open! Core
open! Types_nice

let (=) = Float.(=)

let ( * ) = ( *. )
let (+) = (+.)
let (/) = (/.)
let (-) = (-.)

let (~-) = (~-.)

let sum = List.fold ~f:(+) ~init:0.

let sort_by ~f =
    List.sort ~compare:(Comparable.lift Float.compare ~f)
    
let map_find_exn m k = match Map.find m k with
  | None ->
    let c = Map.comparator m in
    raise_s [%sexp "key missing", (c.sexp_of_t k : Sexp.t)]
  | Some v -> v
