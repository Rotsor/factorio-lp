open! Base
open! Core
open Game_data

open Common

let cmd f = Command.basic ~summary:"" (Command.Param.return f)
let cmd_async f = Async.Command.async ~extract_exn:true ~summary:"" (Command.Param.return f)

module Configuration = struct
    module V1 = struct
        type quality =
            | Expanding
            | Keeping
        [@@deriving sexp]
        type component = {
            name : string;
            blueprint : Blueprint.V1.t;
            quality : quality;
            quantity : float;
        } [@@deriving sexp]
        type t = component list
        [@@deriving sexp]
    end

    module Versioned = struct
        type t = {
            version : int;
            body : Sexp.t;
        }
        [@@deriving sexp]
    end

    module Latest = struct
        type t = V1.t
        let t_of_sexp sexp = 
            let { Versioned.version; body } = Versioned.t_of_sexp sexp in
            match version with
            | 1 -> V1.t_of_sexp body
            | v -> raise_s [%sexp "unsupported version", (v : int)]
        let sexp_of_t t = Versioned.sexp_of_t { version = 1; body = V1.sexp_of_t t }
    end
    
    type t = Latest.t [@@deriving sexp]
    let load () =
        Sexp.load_sexp_conv_exn "configuration.sexp" [%of_sexp: Latest.t]
    let save t =
        Out_channel.write_all "configuration.sexp" ~data:(Sexp.to_string_hum [%sexp (t : Latest.t)])
end

module Solver = Equation_solver.Make(Item_name)(String)

open Async

let print_s = Core.print_s
let printf = Core.printf

let rec binary_search ~f a b =
    let c = (a + b) / 2. in
    if Float.(<) (b - a) 1e-5 then
        (a, b)
    else
        if f c then
        binary_search ~f a c
        else
        binary_search ~f c b


module Lazy_vector = Lazy_vector.Make(String)

let can_produce ~have ~recipe_output =
    Map.merge have recipe_output ~f:(fun ~key:_ -> function
        | `Left _ -> None
        | `Right x -> (if x < 0. then Some () else None)
        | `Both (have, x) -> if x < 0. && have <= 0. then Some () else None
    )
    |> Map.to_alist
    |> function
    | [] -> true
    | _list -> false

let available_machines (game_data : Game_data.t) recipe_name =
  let recipe = map_find_exn game_data.recipes recipe_name in
  let ingredient_count =
    List.length (List.filter recipe.ingredients ~f:(fun input ->
        match input.type_ with
        | `item -> true
        | `fluid _ -> true (* CR: should be false *)
      ))
  in
  Map.filter game_data.accessible_entities ~f:(fun e ->
      match e.kind with
      | Offshore_pump _ -> false
      | Machine m ->
      Set.mem m.crafting_categories recipe.category
      && ((match m.ingredient_count with
          | limit ->
            Int.(>=) limit ingredient_count
        ) || 
          (
            (* hack: allow "handcrafting" of machines by making them constructible in grey factories *)
            match recipe.products with
            | [ { name = item; kind = Item; _ } ] ->
              (match Game_data.entity_by_item_name game_data item with
               | exception _ -> true
               | (_machine) -> true)
            | _ -> true
          )
        )
    )
  |> Map.keys

let warn_if_empty sexp l = match l with
  | [] -> print_s [%sexp "warning! no machine available!", (sexp : Sexp.t)]; []
  | l -> l

let all_trivial_blueprints (game_data : Game_data.t) =
  let recipes = 
    List.concat_map (Map.to_alist game_data.recipes) ~f:(fun (recipe_name, _) ->
        List.map (warn_if_empty [%sexp (recipe_name : Recipe_name.t)] (available_machines game_data recipe_name))
          ~f:(fun machine ->
              (`recipe (recipe_name, machine), (Blueprint.trivial
                                                  ~recipe:recipe_name
                                                  ~machine
                                                  ())))
      )
  in
  let offshore_pumps =
    List.filter_map (Map.to_alist game_data.accessible_entities)
      ~f:(function
            (item, { kind = Offshore_pump _; _ }) ->
            Some (`pump item, [ 1.0, Blueprint.V1.Offshore_pump item ])
          | _ -> None
        )
  in
  recipes @ offshore_pumps

let description_to_string = function
  | `recipe (recipe, machine) ->
    Recipe_name.to_string recipe ^ "@" ^ Item_name.to_string machine
  | `pump item ->
    "pumpage@" ^ Item_name.to_string item

let rec choose_many l n =
  match (l, n) with
  | _, 0 -> [[]]
  | [], _ -> []
  | (x :: xs), n ->
    List.concat (List.init (Int.(+) n 1) ~f:(fun k ->
        List.map ~f:(fun l -> (k, x) :: l)
          (choose_many xs (Int.(-) n k))))

let compress_module_name m =
  match String.split (Item_name.to_string m) ~on:'-' with
  | [ kind; "module"; rank ] ->
    let kind = match kind with
      | "productivity" -> "p"
      | "speed" -> "s"
      | "effectivity" -> "e"
      | _ -> kind
    in
    let rank = match rank with
      | "2" -> "0"
      | "4" -> "1"
      | "6" -> "2"
      | "8" -> "3"
      | _ -> rank
    in
    kind ^ rank
  | _ -> Item_name.to_string m


let module_names_to_string modules =
  String.concat ~sep:"+" (List.map modules ~f:(fun m -> compress_module_name m))

let crafting_recipes_with_modules game_data =
  let all_modules =
    Map.filter_map
      game_data.items
      ~f:(fun item -> item.as_module)
  in
  (List.concat_map (Map.to_alist game_data.recipes) ~f:(fun (recipe_name, _) ->
       let applicable_modules =
         Map.filter all_modules ~f:(fun m -> match m.limitations with
             | None -> true
             | Some limitations -> Set.mem limitations recipe_name)
       in
       List.concat_map (available_machines game_data recipe_name)
         ~f:(fun machine_name ->
             match Game_data.entity_by_item_name game_data machine_name with
             | None -> assert false
             | Some { kind = Machine machine; _ } ->
               List.map (choose_many (Map.to_alist applicable_modules) machine.module_inventory_size)
                 ~f:(fun modules ->
                     let modules = List.concat_map modules ~f:(fun (k, m) ->
                         List.init k ~f:(fun _ -> m))
                     in
                     let module_names, module_effects = List.unzip modules in
                     let module_effects =
                       List.map module_effects ~f:(fun x -> (Modules.Effects.of_raw x.effects))
                       |> List.fold ~init:Modules.Effects.zero ~f:(Modules.Effects.(+))
                     in
                     let investment =
                       Blueprint.trivial_recipe_output_and_capital
                         game_data ~recipe:recipe_name ~machine:machine_name ~module_effects ~module_names
                     in
                     recipe_name,
                     (sprintf "%s@%s*%s" (Recipe_name.to_string recipe_name) (Item_name.to_string machine_name) (module_names_to_string module_names))
                     ,
                     investment
                   )
             | _ -> assert false
           )
     ))


let recipes game_data =
  let mk_recipe name ~output ~capital =
    name,
    { Investment.output;
      capital;
    }
  in
  let conversion name output =
    mk_recipe name ~output ~capital:Value.zero
  in
  let crafting =
    (List.filter_map (all_trivial_blueprints game_data) ~f:(fun (description, blueprint) ->
       let drop =
         match description with
         | `pump _ ->
           false
         | `recipe (recipe, _machine) ->
            List.exists [
                (* "sb-water-mineralized-crystallization" *)
             (* "tree-arboretum-1" *)
             (* "swamp-5"; "desert-5"; "temperate-4";"temperate-5"; "desert-4"; "desert-3";
                    "bob-rubber"; "nutrients-refining-3"; "desert-tree-arboretum-1"; *)
           ] ~f:(fun s -> Recipe_name.(=) recipe (Recipe_name.of_string s))
       in
       if drop then None else
         Some (let name = description_to_string description in
                mk_recipe name ~output:(Blueprint.output game_data blueprint) ~capital:(Blueprint.capital game_data blueprint))
      ))
  in
  let burning =
    List.filter_map (Map.to_alist game_data.items) ~f:(fun (item_name, item) ->
         Option.map item.as_fuel ~f:(fun as_fuel ->
             conversion (sprintf "%s@burning" (Item_name.to_string item_name))
               (
                 Value.of_list [
                   (-1.), item_name;
                   as_fuel.fuel_value, Item_name.chemical_mj;
                 ]
               )
           )
      )
  in
  let generators =
    [mk_recipe "electrical-MJ@steam-engine"
      ~output:(Value.of_list [
          -3.1622776601683795, Item_name.of_string "building-size";
          -1.8, Item_name.chemical_mj;
          0.88640000000000008, Item_name.electrical_mj;
        ])
      ~capital:(Value.of_list [
          1.0, Item_name.of_string "inserter";
          30.0, Item_name.of_string "landfill-sand-3";
          1.0, Item_name.of_string "steam-engine";
        ])]
  in
  crafting @
   burning
  @
  generators
  @
        [
            (*"free-swamp-garden", Item_name.Map.singleton (Item_name.of_string "swamp-garden") 1.0;
            "free-desert-garden", Item_name.Map.singleton (Item_name.of_string "desert-garden") 1.0;
              "free-temperate-garden", Item_name.Map.singleton (Item_name.of_string "temperate-garden") 1.0; *)
          conversion "steam-conversion" (
            Item_name.Map.of_alist_exn [
              (Item_name.of_string "steam"), 1.0;
              (Item_name.of_string "electrical-MJ"), -(150. * 200e-6);
            ]);
          (* conversion "free-desert-tree" (Item_name.Map.singleton (Item_name.of_string "desert-tree") 1.0); *)
          (* conversion "free-electronic-circuit" (Item_name.Map.singleton (Item_name.of_string "electronic-circuit") 1.0); *)
          conversion "big-bottle" ( 
            Item_name.Map.of_alist_exn [
              (Item_name.of_string "big-bottle"), 1.0;
              (*              (Item_name.of_string "high-tech-science-pack"), -1.0; *)
              (Item_name.of_string "science-pack-1"), -1.0;
              (Item_name.of_string "science-pack-2"), -1.0;
(*              (Item_name.of_string "science-pack-3"), -1.0;
                (Item_name.of_string "productivity-module-4"), -0.1; *)
            ]);
          ]

let assume_growth ~growth = List.map ~f:(fun (name, recipe) -> (name, Investment.pure_output ~growth recipe))

let explain_lack_of_growth recipes =
  let by_name = String.Map.of_alist_exn recipes in
  let design goal_item = Lp.Optimal_factory.design ~goal_item ~recipes:(assume_growth ~growth:0.0000 recipes) in
  let rec go queue saw =
    match queue with
    | [] -> ()
    | item :: queue ->
      let report s =
        printf "%50s: %s\n" (Item_name.to_string item) s
      in
      match design item with
      | None ->
        report "can't produce";
        go queue saw
      | Some { goal_item_output; recipes; _ } ->
        report (Float.to_string goal_item_output);
        let new_items =
            List.concat_map (Map.to_alist recipes) ~f:(fun (recipe, _amount) ->
            let capital = (map_find_exn by_name recipe).capital in
            Map.to_alist capital
            |> List.filter_map ~f:(fun (item, _amount2) ->
            if not (Set.mem !saw item) then
              (saw := Set.add !saw item;
               Some item)
            else
              None)
            )
        in
        go (new_items @ queue) saw
  in
  go [Item_name.electrical_mj] (ref Item_name.Set.empty)

let _ = explain_lack_of_growth

let _sample_recipes = [
    "electrical-mj",
    ( 
      Item_name.Map.of_alist_exn [
          (Item_name.electrical_mj), 1.0;
          (Item_name.chemical_mj), -2.0;]);
    "chemical-mj",
    ( 
      Item_name.Map.of_alist_exn [
          (Item_name.chemical_mj), 1.0;
          (Item_name.of_string "building-size"), -4.0;
          (Item_name.of_string "algae"), -1.0;
        ]
    );
    "algae",
    ( 
      Item_name.Map.of_alist_exn [
          (Item_name.of_string "algae"), 1.0;
          (Item_name.electrical_mj), -0.4;
        ]
    );
  ]
      

let report ~growth ~goal_item (recipes : (string * Investment.t) list) (solution : Lp.Optimal_factory.t) =
  let capital_per_recipe =
    List.map recipes ~f:(fun (name, recipe) -> name, recipe.capital)
    |> String.Map.of_alist_exn
  in
  let merge_and_sum ~f a b =
    Map.merge a b ~f:(fun ~key:_ v -> Some (f v))
    |> Map.data
    |> List.fold ~init:0.0 ~f:(+)
  in
  let bottles_produced =
    Map.find_exn solution.recipes "collect-goal-item"
  in
  let space_usage =
    -1.0 * (Map.find_exn solution.problem (Item_name.of_string "building-size")
            |> merge_and_sum solution.recipes
                 ~f:(function
                   | `Left _amount -> 0.0 (* recipe does not use space *)
                   | `Right _ -> 0.0 (* recipe itself is not used? *)
                   | `Both (space_per_recipe, recipes_used) -> (recipes_used * space_per_recipe)))
  in
  let value_shadow_price =
    merge_and_sum solution.shadow_prices ~f:(function
        | `Left _price -> 0.0
        | `Right _amount -> failwith "price unknown"
        | `Both (price, amount) -> price * amount)
  in
  let capital_cost_per_recipe =
    Map.map capital_per_recipe ~f:value_shadow_price
  in
  let total_capital =
    merge_and_sum solution.recipes capital_cost_per_recipe
      ~f:(function
        | `Left _amount -> 0.0 (* must be "collect-goal-item" *)
        | `Right _ -> 0.0 (* unused *)
        | `Both (x, y) -> x * y)
  in
  let space_value =
    value_shadow_price (Value.of_list [ space_usage, Item_name.of_string "building-size" ])
  in
  let growth_value =
    total_capital * growth / 3600.
  in
  let html = Lp.Optimal_factory.report solution in
  let%map () = Writer.save "report.html" ~contents:(Html.render html) in
  (*       print_s [%sexp (capital_cost_per_recipe : float String.Map.t)]; *)
  printf "growth output: %f\n" growth_value;
  printf "building-size input: %f\n" space_value;
  printf "science output: %f\n%!" (value_shadow_price (Value.singleton goal_item 1.0) * bottles_produced)


let solve game_data () =
  let recipes = recipes game_data in
  (*  explain_lack_of_growth recipes; *)
  let%bind () =
    Writer.save
      ~contents:(Sexp.to_string_hum [%sexp (String.Map.of_alist_exn (assume_growth recipes ~growth:0.05) : Value.t String.Map.t)]) "recipes.sexp"
  in
  let goal_item = (Item_name.of_string "big-bottle") in
  (*  let design_factory recipes = Lp.Optimal_factory.design ~goal_item:(Item_name.electrical_mj) ~recipes in *)
  let growth = 0.1 in
  let design_factory recipes =
    Lp.Optimal_factory.design ~goal_item ~recipes in
  let solve recipes =
    design_factory (assume_growth recipes ~growth)
  in
  let report recipes solution = report ~growth ~goal_item recipes solution in
  let%bind () = report recipes (Option.value_exn (solve recipes)) in
  let crafting_recipes_with_modules = crafting_recipes_with_modules game_data in
  Core.printf "%d\n%!" (List.length crafting_recipes_with_modules);
  let rec improve recipes =
    let solution = (Option.value_exn (solve recipes)) in
    let%bind () = report recipes solution in
    let prices = solution.shadow_prices in
    List.map crafting_recipes_with_modules
      ~f:(fun (base_name, name, investment) ->
        base_name, (name, investment)
      )
    |> Recipe_name.Map.of_alist_multi
    |> Map.map ~f:(fun l ->
      List.map l ~f:(fun (name, investment) ->
          ((name, investment),
           let utility = 
             Value.utility 
               ~item_price:(fun item ->
                 match Map.find prices item with
                 | None -> 9999999999. (* raise_s [%sexp  "price not found", (item : Item_name.t)] *)
                 | Some price -> price
               )
           in
         let profit =
           utility (Investment.pure_output investment ~growth)
         in
         let capital = 
           utility investment.capital
         in
         (profit, (profit / capital))
          )
      )
      |> List.sort ~compare:(fun (_, (p, _x)) (_, (q, _y)) ->
           (* we should compare x and y, not p and q, but x and y currently depend on capital prices, which are less reliable *)
           Float.compare p q)
      |> List.rev
      |> List.hd_exn)
    |> Map.data
    |> List.sort ~compare:(fun (_, (p, _x)) (_, (q, _y)) ->
           (* we should compare x and y, not p and q, but x and y currently depend on capital prices, which are less reliable *)
           Float.compare p q)
    |> List.rev
    |> (fun l -> List.take l 20)
    |> List.filter ~f:(fun (_, (p, _)) -> Float.(>) p 1e-5)
    |> fun l ->
       List.iter l ~f:(fun ((i, _inv), (profit, payback)) -> print_s [%sexp (i : string), (profit : float), (payback : float)]);
       improve (recipes @ List.map l ~f:fst)
  in
  improve recipes
(*  let growth_via_prices = snd (
        binary_search 0.0 10.0 ~f:(fun growth ->
            match Lp.Item_prices.find (
                assume_growth recipes ~growth
            ) with
          | `Too_easy -> false
          | `Ok _ -> true
        ))
  in
  let () = 
    match Lp.Item_prices.find (assume_growth recipes ~growth:growth_via_prices) with
    | `Too_easy -> assert false
    | `Ok solution ->
    Lp.Item_prices.report solution
    ~extra:(
    List.map ~f:(fun (name, investment) -> (name, Investment.pure_output investment ~growth:growth_via_prices)) crafting_recipes_with_modules
    )
  in
  let growth_via_design = 
    fst (binary_search 0.0 10.0 ~f:(fun growth ->
        Core.printf "attempting: %f\n%!" growth;
        match design_factory (assume_growth recipes ~growth) with
        | None ->
          true
        | Some _ ->
          false
      ))
  in
  let () = 
    match design_factory (assume_growth recipes ~growth:growth_via_design) with
    | None -> assert false
    | Some solution ->
      Lp.Optimal_factory.report solution
  in
  Core.printf "growth: %f-%f\n%!" growth_via_design growth_via_prices;
  () *)
;;

let game_data_flag =
  Command.Param.map (
    Command.Param.
      (flag
         "game-data"
         ~doc:"game-data.sexp the file generated by export.lua script (defaults to ~/.factorio/script-output/game-data.sexp)"
         (optional string)))
    ~f:(fun path ->
        let path = match path with
        | Some path -> path
        | None ->
          match Sys.getenv "HOME" with
          | Some p -> p ^/ ".factorio/script-output/game-data.sexp"
          | None ->
            failwith "$HOME is unset: please pass -game-data"
        in
        (fun () -> Game_data.load ~path))

let () =
  Command.run (
    Command.group ~summary:""
      [
        "solve", (Command.async ~summary:"solve" (
            let open Command.Let_syntax in
            let%map
              game_data = game_data_flag
            in
            (fun () ->
               game_data ()
               >>= fun game_data ->
               solve game_data ())));
        "pp-game-data", (Command.async ~summary:"solve" (
            let open Command.Let_syntax in
            let%map
              game_data = game_data_flag
            in
            (fun () ->
               game_data ()
               >>| fun game_data ->
               print_s [%sexp (game_data : Game_data.t)]))
                        );
      ])
