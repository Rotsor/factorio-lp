open Base
open Types_nice

type t = float Item_name.Map.t [@@deriving sexp]

val zero : t
val scale : t -> float -> t
val (+) : t -> t -> t
val (-) : t -> t -> t

val sum : t list -> t

val utility : t -> item_price:(Item_name.t -> float) -> float
(* val suggestions : t -> desired_utility:float -> Suggestion.t list *)

val of_list : (float * Item_name.t) list -> t
val of_value : (float * Item_name.t) list -> t