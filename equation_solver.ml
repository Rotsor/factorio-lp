open! Core
open! Common

let (=) = Rat.(=)

let ( * ) = Rat.( * )
let (+) = Rat. (+)
let (/) = Rat.(/)
let (-) = Rat.(-)

let (~-) = Rat.(~-)


module Make (A : sig 
    type t [@@deriving sexp_of]
    include Comparable.S with type t := t
    end) (B : Comparable.S) : sig 
    type equation = Rat.t A.Map.t * Rat.t B.Map.t (* equation: sum of them all = 0 *)
    [@@deriving sexp_of]
    
    (** brings as many equation as it can to the form:
     a_i = <linear-combination-of-bs> *)
    val good_form : elimination_order:A.t list -> equation list -> equation list
end = struct

    type equation = Rat.t A.Map.t * Rat.t B.Map.t (* equation: sum of them all = 0 *)
    [@@deriving sexp_of]

    let scale_equation (m1, m2) s =
        Map.map m1 ~f:(( * ) s), Map.map m2 ~f:(( * ) s);;

    let add_equation ~must_remove (a1, b1) (a2, b2) =
        let add_map a1 a2 =
            Map.merge a1 a2 ~f:(fun ~key:_ -> function
                | `Left a -> Some a
                | `Right b -> Some b
                | `Both (a, b) ->
                    Some (a + b))
        in
        let a, b = add_map a1 a2, add_map b1 b2 in
        let a = Map.filteri a ~f:(fun ~key ~data ->
            if A.(=) must_remove key then
            (if Rat.abs data = Rat.zero then false
            else raise_s [%sexp "tried to eliminate this, but it stayed", (must_remove : A.t), (a, b : equation)]
        )
            else (if Rat.(=) data Rat.zero then false else true))
        in
        if Map.is_empty a && Map.is_empty b then
        None
        else Some (a, b)

    let rec find_and_give_remaining ~f l = match l with
        | [] -> None
        | x :: xs -> if f x then Some (x, xs)
        else match find_and_give_remaining ~f xs with
        | None -> None
        | Some (res, resid) -> Some (res, x :: resid)

    (* invariant: equations have no zeroes *)
    let rec good_form ~elimination_order good_equations bad_equations =
        match elimination_order with
        | [] ->
            good_equations
        | a :: elimination_order ->
            match find_and_give_remaining bad_equations ~f:(fun equation -> 
                Option.is_some (Map.find (fst equation) a)
            ) with
            | None ->
                good_form ~elimination_order good_equations bad_equations
            | Some (eq1, bad_equations) ->
            
            match Map.find (fst eq1) a with
            | None ->
                assert false;
            | Some av ->
                assert (not (Rat.(=) av Rat.zero));
                let new_equation = (scale_equation eq1 (Rat.one / av)) in
                let remove_a equations =
                    List.filter_map equations ~f:(fun (e : equation) ->
                        match Map.find (fst e) a with
                        | None -> Some e
                        | Some av2 ->
                            add_equation ~must_remove:a e (scale_equation eq1 (Rat.one / av * (- (av2))))
                    )
                in
                good_form ~elimination_order (new_equation :: remove_a good_equations) (remove_a bad_equations)
    ;;
    let good_form ~elimination_order equations = good_form ~elimination_order [] equations
end