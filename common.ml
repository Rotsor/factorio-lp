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
    
