open! Core
open Common

open! Types_nice

module G = Glpk

module Item_prices : sig
  type t
  val find : (String.t * Value.t) list -> [ `Too_easy | `Ok of t ]
  val lookup_exn : t -> (Item_name.t -> float)
  val report : extra:(String.t * Value.t) list -> t -> unit
end = struct

  type t = {
    prices : float Item_name.Map.t;
    recipes : (String.t * Value.t) list;
  }

  let lookup_exn { prices; _ } i = Option.value_exn (Map.find prices i)
  let lookup { prices; _ } i = Map.find prices i

  let find (recipes_list : (String.t * Value.t) list) =
    let item_array, (item_to_i : Item_name.t -> int) = 
      List.concat_map recipes_list ~f:(fun (_recipe, value) ->
          Map.keys value)
      |> Item_name.Set.of_list
      |> fun s ->
      let a = Array.of_list (Set.to_list s) in
      let m = 
        List.mapi (Array.to_list a) ~f:(fun i v -> (v, i))
        |> Item_name.Map.of_alist_exn
      in
      (a, map_find_exn m)
    in
    let constraints =
      List.map recipes_list ~f:(fun (_r, v) ->
          let res = Array.map item_array ~f:(fun _ -> 0.0) in
          let () =
            Map.to_alist v
            |> List.iter ~f:(fun (i, v) ->
                assert (Float.(=) (Array.get res (item_to_i i)) 0.0);
                Array.set res (item_to_i i) v)
          in
          (res, (-Float.infinity, 0.))
        )
    in
    let lp =
      G.make_problem Maximize
        (Array.map item_array ~f:(fun _ -> 1.0))
        (Array.of_list (List.map ~f:fst constraints))
        (Array.of_list (List.map ~f:snd constraints))
        (Array.map item_array ~f:(fun i -> 
             if (Item_name.(=) i Item_name.electrical_mj)
             then (1., 1.)
             else
             if Item_name.(=) (Item_name.of_string "building-size") i
             then (0.0, 0.0)
             else 
             if (Item_name.(=) i (Item_name.of_string "solid-lime"))
             then (-0.0, 10000.)
             else (-0.0, 1000000.)
           ))
    in
    G.set_message_level lp 0;
    G.scale_problem lp;
    G.use_presolver lp true;
    match G.simplex lp with
    | exception G.No_primal_feasible_solution -> `Too_easy
    | () ->
      let prim = G.get_col_primals lp in
      let _objective_val = (G.get_obj_val lp) in
      let prices =
        Array.mapi prim ~f:(fun i v ->
            (Array.get item_array i), v
          ) |>
        Array.to_list
        |> Item_name.Map.of_alist_exn
      in
      `Ok { prices; recipes = recipes_list }

  let report ~extra ({ prices; recipes } as t) =
    let () =
      prices
      |> Map.to_alist
      |> List.sort ~compare:(fun (_, x1) (_, x2) -> Float.compare x1 x2)
      |> List.iter ~f:(fun (name, v) -> printf "%80s: %20.4f\n"  (Item_name.to_string name) v)
    in
    let item_price item =
      match lookup t item with
      | None -> 1e7
      | Some x -> x
    in
    let () =
      List.map (recipes @ extra) ~f:(fun (recipe, (output : Value.t)) ->
          let output_utility =
            Value.utility ~item_price output
          in
          (recipe, output_utility)
        )
      |> List.sort ~compare:(Comparable.lift Float.compare ~f:(fun (_, x) -> x))
      |> List.iter ~f:(fun (r,v) -> printf "%s\n" (Sexp.to_string [%sexp (r, v : string * float)]))
    in
    ()
end

module Optimal_factory : sig 
  type t = {
    recipes : float String.Map.t;
    goal_item_output : float;
    problem : float String.Map.t Item_name.Map.t
  }

  val design :
    goal_item:Item_name.t
    -> recipes : (String.t * Value.t) list
    -> t option

  val report : t -> unit
end = struct

  type t = {
    recipes : float String.Map.t;
    goal_item_output : float;
    problem : float String.Map.t Item_name.Map.t
  }

  let report t =
    let () =
      t.recipes
      |> Map.to_alist
      |> List.sort ~compare:(fun (_, x1) (_, x2) -> Float.compare x1 x2)
      |> List.iter ~f:(fun (name, v) -> 
          if Float.abs v > 1e-5
          then printf "%80s: %20.4f\n"  name v)
    in
    let recipe_amount recipe =
      Option.value ~default:0.0 (Map.find t.recipes recipe)
    in
    let () =
      Map.mapi t.problem ~f:(fun ~key:_ ~data ->
          let net = 
            List.fold ~init:0. ~f:(+)
              (Map.data (Map.mapi data ~f:(fun ~key:recipe ~data:c -> c * recipe_amount recipe)))
          in
          let gross = 
            List.fold ~init:0. ~f:(+)
              (Map.data (Map.mapi data ~f:(fun ~key:recipe ~data:c -> 
                   let r = c * recipe_amount recipe in
                   (Float.abs r / 2.)
                 )))
          in
          let individuals =
            Array.filter_map 
              (Map.mapi data ~f:(fun ~key:recipe ~data:c -> c * recipe_amount recipe)
               |> Map.to_alist
               |> Array.of_list
              )
              ~f:(fun (r, v) ->
                  if Float.(=) gross 0. then None
                  else
                  if Float.abs (v / gross) < 1e-4 then None else 
                    Some (r, v, v / gross)
                )
          in
          (net, gross, individuals)
        )
      |> Map.to_alist
      |> List.sort ~compare:(fun (_, (_, x1, _)) (_, (_, x2, _)) -> Float.compare x1 x2)
      |> List.iter ~f:(fun (name, (net, gross, components)) -> 
          let components = 
            String.concat ~sep:"" (Array.to_list (
                Array.map components ~f:(fun (i, v, p) -> 
                    sprintf "           %02.1f%%: %s:%f\n" (p * 100.) i v
                  )))
          in
          if String.(=) components ""
          then ()
          else
            printf "\n%60s: %20.4f %20.4f\n%s" (Item_name.to_string name) gross net components)
    in
    ()


  let design ~goal_item ~recipes:(recipes_list : (String.t * Value.t) list) =
    let recipes = Array.of_list recipes_list in
    Core.printf "%s\n" (Sexp.to_string [%sexp [%here]]);
    let net_per_item =
      List.concat_mapi recipes_list ~f:(fun i -> (fun (_recipe, value) ->
          Map.map value ~f:(fun f -> (i, f))
          |> Map.to_alist
        ))
      |> Item_name.Map.of_alist_multi
      |> Map.map ~f:(fun l ->
          let arr = (Array.map recipes ~f:(fun _ -> 0.)) in
          List.iter l
            ~f:(fun (i, v) -> arr.(i) <- arr.(i) + v);
          arr)
    in
    let constraints =
      Map.to_alist net_per_item
      |> List.map ~f:(fun (k, expression) ->
          let constraint_ = 
            if Item_name.(=) (Item_name.of_string "building-size") k
            then (-1000.00, -0.0)
            else
              let bad_items = ["solid-lime"] in
              if List.exists bad_items ~f:(fun bad -> Item_name.(=) (Item_name.of_string bad) k)
              then (-1000., 0.00)
              else
              if Item_name.(=) k goal_item
              then (-0.00, Float.infinity)
              else (-0.00, Float.infinity)
          in
          (expression, constraint_)
        )
    in
    let lp =
      G.make_problem Maximize
        (map_find_exn net_per_item goal_item)
        (Array.of_list (List.map ~f:fst constraints))
        (Array.of_list (List.map ~f:snd constraints))
        (Array.map recipes ~f:(fun _ -> (0., Float.infinity)))
    in
    (*G.set_message_level lp 0;*)
    G.scale_problem lp;
    G.use_presolver lp true;
    G.simplex lp;
    let prim = G.get_col_primals lp in
    let z = (G.get_obj_val lp) in
    if z <= 1e-8 then None
    else
      let recipes_map =
        Array.mapi prim ~f:(fun i v ->
            (fst (Array.get recipes i)), v
          ) 
        |> Array.to_list
        |> String.Map.of_alist_exn
        |> Map.filter ~f:(fun x -> Float.(>) x 1e-8)
      in
      Some {
        goal_item_output = z;
        recipes = recipes_map;
        problem =
          Map.map net_per_item ~f:(fun arr ->
              String.Map.of_alist_exn (
                Array.mapi arr ~f:(fun i c ->
                    (fst (Array.get recipes i), c)
                  )
                |> Array.to_list)
            )
      }
end
