open! Base

open Float

type t = {
  output : Value.t;
  capital : Value.t;
} [@@deriving sexp_of]

let passive_capital capital = { output = Value.zero; capital; }

let free_output output = { output; capital = Value.zero; }

let pure_output ~growth t =
  Map.filter 
    ~f:(fun x -> Float.abs x > 1e-13)
    (Value.(+)
       (Value.scale t.capital (-growth/(3600.)))
       t.output)

let zero = { output = Value.zero; capital = Value.zero; }

let (+)
    { output = output1; capital = capital1; }
    { output = output2; capital = capital2; }
  =
  let (+) = Value.(+) in
  { output = output1 + output2; capital = capital1 + capital2; }

let scale { output; capital } c = { output = Value.scale output c; capital = Value.scale capital c; }

let linear_combination l =
  List.fold ~f:(+) ~init:zero (List.map ~f:(fun (c, v) -> scale v c) l)
