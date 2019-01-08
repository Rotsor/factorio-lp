open! Base
type power =
    | Chemical of { power : float }
    | Electrical of { power : float; drain : float }
[@@deriving sexp]

type product_amount = 
    | Deterministic of float
    | Probabilistic of {
        amount_min : float option;
        amount_max : float option;
        probability : float option;
    }
[@@deriving sexp]

 type recipe = {
    name : string;
    inputs : (float * string) list;
    outputs : (product_amount * string) list;
    effort : float;
    category : string;
 } [@@deriving sexp]

type machine = {
    name : string;
    categories : string list;
    crafting_speed : float;
    power : power;
    ingredient_count : int option;
    size_x : float;
    size_y : float;
    (* pumps_fluid : (string * float) option; *)
} [@@deriving sexp]
