open! Base
open! Core
open Game_data

open Common

let hide_recipe = 
    let hidden =
        [
            "bob-rubber";
            "angelsore5-crushed-smelting";
            "angelsore6-crushed-smelting";
            "angelsore3-crushed-smelting";
            "angelsore1-crushed-smelting";
            "lead-plate";
            "quartz-glass";
            "silver-plate";
            "tin-plate";
            "cellulose-fiber-raw-wood";
            "water-separation";
        ]
        |>  List.map ~f:(Recipe_name.of_string)
    in
    fun name -> 
    List.exists hidden ~f:(Recipe_name.(=) name)
    || String.is_prefix ~prefix:"alien-artifact-" (Recipe_name.to_string name)
    || List.exists [
        "diamond"; "amethyst"; "ruby"; "emerald"; "sapphire"; "topaz";
        "ore2"; "ore4"; "ore5"; "ore6";
        "-void-"] ~f:(fun forbidden_infix ->
        String.is_substring ~substring:forbidden_infix (Recipe_name.to_string name))

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
  let _mk_recipe name ~output ~capital =
    name,
    { Investment.output;
      capital;
    }
  in
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
       Core.printf "applicable modules: %d\n%!" (Map.length applicable_modules);
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
                     (sprintf "%s@%s*%s" (Recipe_name.to_string recipe_name) (Item_name.to_string machine_name) (module_names_to_string module_names))
                     ,
                     Crafting_recipes.crafting_recipe_output game_data ~machine:machine_name ~recipe:recipe_name ~module_effects ~utilization:1.0)
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
          conversion "free-desert-tree" (Item_name.Map.singleton (Item_name.of_string "desert-tree") 1.0);
          conversion "free-viscous-water" (Item_name.Map.singleton (Item_name.of_string "water-viscous-mud") 100.0);
          conversion "big-bottle" ( 
            Item_name.Map.of_alist_exn [
              (Item_name.of_string "big-bottle"), 1.0;
              (Item_name.of_string "high-tech-science-pack"), -1.0;
              (Item_name.of_string "science-pack-1"), -1.0;
              (Item_name.of_string "science-pack-2"), -1.0;
              (Item_name.of_string "science-pack-3"), -1.0;
              (Item_name.of_string "productivity-module-4"), -0.1;
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

let solve game_data () =
  let recipes = recipes game_data in
  (*  explain_lack_of_growth recipes; *)
  let%map () =
    Writer.save
      ~contents:(Sexp.to_string_hum [%sexp (String.Map.of_alist_exn (assume_growth recipes ~growth:0.05) : Value.t String.Map.t)]) "recipes.sexp"
  in
  let crafting_recipes_with_modules = crafting_recipes_with_modules game_data in
  Core.printf "%d\n%!" (List.length crafting_recipes_with_modules);
    let growth_via_prices = snd (
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
        ~extra:crafting_recipes_with_modules
  in
  let design_factory recipes = Lp.Optimal_factory.design ~goal_item:(Item_name.electrical_mj) ~recipes in
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
  ()
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
(*
        "recipe-summary", Command.basic ~summary:"" (Command.Param.return (fun () ->

    (List.bind Game_data.recipes ~f:(fun recipe ->
        List.filter_map Game_data.machines ~f:(fun machine ->
            if List.mem machine.categories ~equal:Category.equal recipe.category
            then
            Some ((recipe, machine.name),
                Blueprint.output (Blueprint.trivial ~recipe:recipe.name ~machine:machine.name))
            else(None)
        )
    ))
    |> List.sort ~compare:(Comparable.lift Float.compare ~f:(fun (_,x) -> Result.paybacks_per_hour x))
    |> fun l ->
    let report str l =
        printf "\n%s:\n" str;
        List.iter l ~f:(fun ((recipe, machine_name), r) -> Core_kernel.printf "%s\n" (Sexp.to_string_hum 
        [%sexp 
            (Result.paybacks_per_hour r : float),
            ((recipe : Investment.t).name : Recipe_name.t),
            (machine_name : Item_name.t),
            (r : Result.t)
(*            ,(List.take (Result.suggestions r) 6 : Suggestion.t list) *)
            ]
            )
            )
    in
    report "worst" (List.take (List.filter ~f:(fun i -> 
        not (hide_recipe ((fst (fst i) : Investment.t).name))) l) 6);
    report "best" (List.take (List.rev l) 6);
    )) *)
    
    ])
