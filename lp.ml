open! Core
open Common

open! Types_nice

module G = Glpk

module Recipe_name = struct
  include String
end

module Item_prices : sig
  type t
  val find : (Recipe_name.t * Value.t) list -> [ `Too_easy | `Ok of t ]
  val lookup_exn : t -> (Item_name.t -> float)
  val report : extra:(Recipe_name.t * Value.t) list -> t -> unit
end = struct

  type t = {
      prices : float Item_name.Map.t;
      recipes : (Recipe_name.t * Value.t) list;
    }

  let lookup_exn { prices; _ } i = Option.value_exn (Map.find prices i)
  let lookup { prices; _ } i = Map.find prices i

  let find (recipes_list : (Recipe_name.t * Value.t) list) =
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
             then (1.0, 1.0)
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
    printf "%!";
    ()
end

module Optimal_factory : sig 
  type t = {
      recipes : float Recipe_name.Map.t;
      goal_item_output : float;
      problem : float Recipe_name.Map.t Item_name.Map.t;
      shadow_prices : float Item_name.Map.t;
    }

  val design :
    goal_item:Item_name.t
    -> recipes : (Recipe_name.t * Value.t) list
                 -> t option

  val report : t -> Html.t
end = struct

  type t = {
      recipes : float Recipe_name.Map.t;
      goal_item_output : float;
      problem : float Recipe_name.Map.t Item_name.Map.t;
      shadow_prices : float Item_name.Map.t;
    }
  

  module Report = struct

    let table ~columns ~rows =
      let open Html in
      table (
        tr (List.map columns ~f:(fun (name, _) ->
            th (text name)
          ))
        ::
        List.map rows ~f:(fun row ->
            tr (
              List.map columns ~f:(fun (_name, f) ->
                  td (f row)
                )
            )))

    let column to_html field =
      (Field.name field, (fun x -> to_html (Field.get field x)))

    module Anchor_namespace (M : sig
        type t
        val to_string : t -> string
        val namespace : string
      end) = struct

      let anchor_name name = M.namespace ^ "!" ^ name
      
      let link t =
        let name = M.to_string t in
        (* some url quoting is in order, I guess, but whatever *)
        Html.link (Html.text name) ~url:("#" ^ anchor_name name)

      let anchor t =
        let name = M.to_string t in
        Html.anchor (Html.text name) ~id:(anchor_name name)
        
    end

    module Recipe_name_anchor = Anchor_namespace (
      struct
        include Recipe_name
        let namespace = "recipe"
      end)

    module Item_name_anchor = Anchor_namespace (
      struct
        include Item_name
        let namespace = "item"
      end)
        
    let float_to_html precision float = Html.text (sprintf "%.*f" precision float)

    let option_to_html f = function
      | None -> Html.text "<none>"
      | Some x -> f x
    
    module Recipe_data = struct

      type material_entry = {
        item_name : Item_name.t;
        amount : float;
        value : float;
      }

      type t = {
        name : Recipe_name.t;
        amount : float;
        (* the rest is per second per crafter *)
        inputs : material_entry list;
        outputs : material_entry list;
        value_processed : float;
        profitability : float;
      } [@@deriving fields]

      let summary_table ts =
        let ts = List.sort ts ~compare:(Comparable.lift ~f:value_processed Float.compare) in
        table ~rows:ts ~columns:[
          column Recipe_name_anchor.link Fields.name;
          column (float_to_html 3) Fields.amount;
          column (float_to_html 3) Fields.profitability;
          column (float_to_html 3) Fields.value_processed;
        ]


      let details ts =
        Html.concat (
          List.map ts ~f:(fun recipe ->
              Html.div (
                let small_table name rows =
                  table
                    ~columns:[
                      name, (fun x -> Item_name_anchor.link x.item_name);
                      "amount", (fun x -> float_to_html 2 (x.amount * recipe.amount));
                      "value", (fun x -> float_to_html 2 (x.value * recipe.amount));
                      "fraction", (fun x ->
                          if Float.(=) recipe.value_processed 0.0 then Html.text "<n/a>"
                          else
                            (Html.concat [(float_to_html 2 (x.value * recipe.amount / recipe.value_processed * 100.)); Html.text "%"]));
                    ]
                    ~rows
                in
                Html.concat [
                  Recipe_name_anchor.anchor recipe.name;
                  small_table "input" recipe.inputs;
                  small_table "output" recipe.outputs;
                ]
              )
            ))

    end

    module Item_data = struct

      type recipe_entry = {
        recipe_name : Recipe_name.t;
        amount : float;
      }

      let (<<) f g x = f (g x)
      
      type t = {
        name : Item_name.t;
        shadow_price : float option;
        gross : float;
        net : float;
        producers : recipe_entry list;
        consumers : recipe_entry list;
      } [@@deriving fields]

      let summary_table ts =
        let gross_value t =
          match t.shadow_price with
          | None -> 0.
          | Some shadow_price ->
            Float.abs (t.gross *. shadow_price)
        in
        let ts = List.sort ts ~compare:(Comparable.lift ~f:gross_value Float.compare) in
        table ~rows:ts ~columns:[
          column Item_name_anchor.link Fields.name;
          column (option_to_html (float_to_html 4)) Fields.shadow_price;
          column (float_to_html 2) Fields.net;
          column (float_to_html 2) Fields.gross;
          ("gross-value", float_to_html 2 << gross_value);
        ]

      let details ts =
        Html.concat (
          List.map ts ~f:(fun item ->
              Html.div (
                let small_table name rows =
                  table
                    ~columns:[
                      name, (fun x -> Recipe_name_anchor.link x.recipe_name);
                      "amount", (fun x -> float_to_html 2 x.amount);
                      "fraction", (fun x ->
                          if Float.(=) item.gross 0.0 then Html.text "<n/a>"
                          else
                            (Html.concat [(float_to_html 2 (x.amount / item.gross * 100.)); Html.text "%"]));
                    ]
                    ~rows
                in
                Html.concat [
                  Item_name_anchor.anchor item.name;
                  small_table "producer" item.producers;
                  small_table "consumer" item.consumers;
                ]
              )
            ))
          
    end

    let gross_net list =
      let sum = List.fold ~init:0. ~f:(+) in
      let net = 
        sum list
      in
      let pos = sum (List.filter ~f:((<=) 0.) list) in
      let neg = sum (List.filter ~f:((>) 0.) list) in
      let gross = 
        Float.max (Float.abs pos) (Float.abs neg)
      in
      (gross, net)
    
  let report (t : t) =
    let recipe_data : Recipe_data.t list = 
      t.recipes
      |> Map.to_alist
      |> (
        List.map ~f:(fun (name, v) ->
            let items =
              Map.filter_map t.problem ~f:(fun m -> Map.find m name)
              |> Map.merge t.shadow_prices ~f:(fun ~key -> function
                  | `Left _price -> None
                  | `Right _amount -> failwith "no price"
                  | `Both (price, amount) -> Some {
                      Recipe_data.
                      item_name = key;
                      amount;
                      value = price *. amount;
                    })
              |> Map.data
            in
            let inputs =
              List.filter_map items ~f:(fun t ->
                  if t.value >= 0. then None else Some { t with value = -. t.value }
                )
            in
            let outputs = 
              List.filter_map items ~f:(fun t ->
                  if t.value < 0. then None else Some t
                )
            in
            let gross, net = gross_net (
                items
                |> List.map ~f:(fun x -> x.value))
            in
            { Recipe_data.
              name;
              amount = v;
              profitability = net * v;
              value_processed = gross * v;
              inputs;
              outputs;
            }
          ))
      |> List.sort ~compare:(fun a b -> Float.compare a.Recipe_data.amount b.amount)
    in
    let recipe_amount recipe =
      Option.value ~default:0.0 (Map.find t.recipes recipe)
    in
    let item_data =
      Map.mapi t.problem ~f:(fun ~key:item_name ~data:recipes ->
          let gross, net =
            gross_net (Map.data (Map.mapi recipes ~f:(fun ~key:recipe ~data:c -> c * recipe_amount recipe)))
          in
          let components =
            Map.filter_mapi recipes ~f:(fun ~key:recipe ~data:c ->
                let amount = c * recipe_amount recipe in
                if Float.(=) amount 0.
                then None
                else Some amount)
            |> Map.to_alist
          in
          { Item_data.
            name = item_name;
            shadow_price = Map.find t.shadow_prices item_name;
            gross;
            net;
            producers =
              List.filter_map components ~f:(fun (k, v) ->
                  if Float.(>=) v 0. then Some { Item_data.recipe_name = k; amount = v } else None
                );
            consumers =
              List.filter_map components ~f:(fun (k, v) ->
                  if Float.(<) v 0. then Some { Item_data.recipe_name = k; amount = -. v } else None
                );
          })
      |> Map.data
    in
    Html.concat [
      Recipe_data.summary_table recipe_data;
      Item_data.summary_table item_data;
      Recipe_data.details recipe_data;
      Item_data.details item_data;
    ]
    
end

let report = Report.report
  


  let design ~goal_item ~recipes:(recipes_list : (Recipe_name.t * Value.t) list) =
    let recipes_list = (recipes_list @ ["collect-goal-item", Value.of_list [1.0, Item_name.of_string "goal-item"; (-1.0), goal_item]]) in
    let recipes = Array.of_list recipes_list in
    let goal_item = Item_name.of_string "goal-item" in
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
             (k, (expression, constraint_))
           )
    in
    let lp =
      let constraints = List.map ~f:snd constraints in
      G.make_problem Maximize
        (map_find_exn net_per_item goal_item)
        (Array.of_list (List.map ~f:fst constraints))
        (Array.of_list (List.map ~f:snd constraints))
        (Array.map recipes ~f:(fun _ -> (0., Float.infinity)))
    in
    G.set_message_level lp 0;
    G.scale_problem lp;
    G.use_presolver lp true;
    G.simplex lp;
    let prim = G.get_col_primals lp in
    let shadow_prices =
      List.mapi constraints ~f:(fun i (item, _) ->
          (item, G.get_row_dual lp i))
      |> Item_name.Map.of_alist_exn
    in
    let z = (G.get_obj_val lp) in
    if z <= 1e-8 then None
    else
      let recipes_map =
        Array.mapi prim ~f:(fun i v ->
            (fst (Array.get recipes i)), v
          ) 
        |> Array.to_list
        |> Recipe_name.Map.of_alist_exn
        |> Map.filter ~f:(fun x -> Float.(>) x 1e-8)
      in
      let energy_price = match Map.find shadow_prices Item_name.electrical_mj with
        | None -> failwith "no energy price"
        | Some p -> p
      in
      let shadow_prices = Map.map shadow_prices ~f:(fun x -> x / energy_price) in
      Some {
          goal_item_output = z;
          recipes = recipes_map;
          shadow_prices;
          problem =
            Map.map net_per_item ~f:(fun arr ->
                Recipe_name.Map.of_alist_exn (
                    Array.filter_mapi arr ~f:(fun i c ->
                        if Float.(=) c 0.0
                        then None
                        else
                          Some
                        (fst (Array.get recipes i), c)
                      )
                    |> Array.to_list)
              )
        }
end
