open! Base
open Types_nice
type t = float Item_name.Map.t [@@deriving sexp]

let zero = Item_name.Map.empty
let scale t c = Map.map ~f:(( *. ) c) t

let utility t ~item_price = 
    List.fold 
        (List.map (Map.to_alist t) ~f:(fun (x,y) -> item_price x *. y))
        ~init:0.
        ~f:(+.)

let sexp_of_t t = [%sexp (t : t)]

(* let suggestions t ~desired_utility = 
    let desired_change = desired_utility - utility t in
    List.map (Map.to_alist t) ~f:(fun (item, contrib) -> 
        { Suggestion.item; suggestion = item_price item + desired_change / contrib })
    |> List.sort ~compare:(Comparable.lift [%compare: float] ~f:(Suggestion.bigness)) *)

let of_value l = Map.map ~f:(List.fold ~init:0. ~f:(+.)) (Item_name.Map.of_alist_multi (List.map l ~f:(fun (c, x) -> (x, c))))
let of_list = of_value

let (+) = Map.merge ~f:(fun ~key:_ -> function
    | `Left x -> Some x
    | `Right x -> Some x
    | `Both (a, b) -> Some (a +. b))

let sum values = Option.value ~default:zero (List.reduce_balanced ~f:(+) values)

let (-) a b = (+) a (scale b (-1.))

let singleton item quantity = of_list [quantity, item]
