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
    end) (B : sig 
    type t [@@deriving sexp_of]
    include Comparable.S with type t := t
    end) : sig 
    type equation = Rat.t A.Map.t * Lazy_vector.Make(B).t (* equation: sum of them all = 0 *)
    [@@deriving sexp_of]
    
    (** brings as many equation as it can to the form:
     a_i = <linear-combination-of-bs> *)
    val good_form : elimination_order:A.t list -> equation list -> equation list
end = struct

    module Lazy_vector = Lazy_vector.Make(B)
    (* the function is guaranteed memoized *)
    type equation = Rat.t A.Map.t * Lazy_vector.t (* equation: sum of them all = 0 *)
    [@@deriving sexp_of]

    let scale_equation (m1, m2) s =
        Map.map m1 ~f:(( * ) s), Lazy_vector.scale m2 s;;

    let rec merge_skewed' ~combine m1 m2 =
        match Int.(<=) (Map.length m1) (Map.length m2) with
        | false -> merge_skewed' ~combine:(fun x y -> combine y x) m2 m1
        | true ->
            List.fold (Map.to_alist m1) ~init:m2 ~f:(fun m (k, v1) ->
                match Map.find m2 k with
                | None -> Map.add_exn m ~key:k ~data:v1
                | Some v2 ->
                    match combine v1 v2 with
                    | Some v -> Map.set m ~key:k ~data:v
                    | None -> Map.remove m k
            )

    let add_equation ~must_remove (a1, b1) (a2, b2) =
        let add_map a1 a2 =
            merge_skewed' a1 a2 ~combine:(fun x y -> 
                let res = x + y in
                if Rat.(=) res Rat.zero then None else Some res)
        in
        let a, b = add_map a1 a2, Lazy_vector.(+) b1 b2 in
        let () = match Map.find a must_remove with
            | Some _ -> 
                raise_s [%sexp "tried to eliminate this, but it stayed", (must_remove : A.t), (a, b : equation)]
            | None -> ()
        in
        Some (a, b)

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