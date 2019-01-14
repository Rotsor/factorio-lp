open! Base
open! Core
module GG = Game_data
open! Types_nice

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
let cmd_async f = Async.Command.async ~summary:"" (Command.Param.return f)

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

let trivial_blueprints : (string * Blueprint.V1.t) list =
    List.bind Game_data.recipes ~f:(fun recipe ->
        List.filter_map Game_data.machines ~f:(fun machine ->
            if List.mem machine.categories ~equal:Category.equal recipe.category
            then
                Some (
                    (Recipe_name.to_string recipe.name ^ "@" ^ Item_name.to_string machine.name), 
                    Blueprint.trivial ~recipe:recipe.name ~machine:machine.name ())
            else None
        ))



let print_report c =
    let elimination_order =
        List.rev (List.concat_map c ~f:(fun
            { Configuration.V1.blueprint; quality = _; quantity = _; name = _ } ->
                Map.filter 
                    ~f:(fun x -> x > 1e-13)
                    (Blueprint.output blueprint)
                |>  Map.keys
            ))
    in
    let equations ~growth : Solver.equation list =
        List.map c ~f:(fun
            { Configuration.V1.blueprint; quality = _; quantity = _; name } ->
                (
                    Map.filter 
                        ~f:(fun x -> Float.abs x > 1e-13)
                        (Value.(+) (Value.scale (Blueprint.capital blueprint) (-growth/(3600.))) (Blueprint.output blueprint)),
                    Lazy_vector.singleton name)
            )
        |> List.map ~f:(let conv x = Map.map ~f:Rat.of_float_dyadic x in fun (a, b) -> (conv a, b))
    in
    let solve growth = 
        Core.printf "solving for %.4f... %!" growth;
        let (solved : Solver.equation list) = Solver.good_form (equations ~growth) ~elimination_order in
        Core.printf "done\n%!";
        let (items, recipes) as equation =
            List.find_exn solved ~f:(fun ((items, _recipes)) ->
                Option.is_some (Map.find items (Item_name.electrical_mj))
            )
        in
        let sign =
            match Int.(=) (Map.length items) 1 with
            | false -> `Mixed
            | true ->
                let recipes = Lazy_vector.to_map recipes in
                if List.for_all (Map.data recipes) ~f:(fun x -> (Rat.(>=) x Rat.zero))
                then `Positive
                else 
                if List.for_all (Map.data recipes) ~f:(fun x -> (Rat.(<=) x Rat.zero))
                then `Negative
                else
                `Mixed
        in
        sign, (equation, solved)
    in
    let growth_per_hour =
        fst (binary_search ~f:(fun growth -> 
            match (solve growth) with
            | `Positive, _ -> false
            | `Negative, _ -> true
            | `Mixed, (equation, solution) -> raise_s [%sexp "Mixed", (equation : Solver.equation), (solution : Solver.equation list)]
        ) 0.0 1.0)
    in
    printf "growth rate: %.4f\n" growth_per_hour;
    let solution, equations = snd (solve (growth_per_hour *. 0.99)) in
    print_s [%sexp (solution : Solver.equation)];
    let electricity_recipe_name =
        match List.filter (Map.keys (Lazy_vector.to_map (snd solution))) ~f:(String.is_prefix ~prefix:"electrical-MJ@") with
        | [ key ] -> key
        | [] -> raise_s [%sexp "no electricity recipe?"]
        | _ :: _ :: _ -> raise_s [%sexp "multiple electricity recipes?"]
    in
    let prices = 
        let recipes_to_map = (fun (what, how_much) ->
                (what, Lazy_vector.to_map how_much)
            )
        in
        let solution = recipes_to_map solution in
        List.map equations ~f:recipes_to_map
        |>
        List.map ~f:(fun ((what, _) as eqn) ->
            let get (_, how_much) =
                Option.value ~default:Rat.zero (Map.find how_much electricity_recipe_name)
            in
            let price = 
                Rat.to_float (Rat.(/) 
                    (get eqn)
                    (get solution))
            in
            printf
                !"%60s: %12.3f\n"
                    (Sexp.to_string ([%sexp  (what : Rat.t Item_name.Map.t)]))
                    price;
            (what, price)
        )
        |> List.filter_map ~f:(fun (what, price) ->
            match Map.to_alist what with
            | [ (item, amount) ] -> 
                assert (Rat.(=) amount Rat.one);
                Some (item, price)
            | _ -> None)
        |> Item_name.Map.of_alist_exn
    in
    let basket_price (basket : Value.t) : float option =
        with_return (fun { return } -> 
            Some (Value.utility basket ~item_price:(fun item -> match Map.find prices item with
                | None -> return None
                | Some x -> x)))
    in
    let () =
        List.filter_map trivial_blueprints 
            ~f:(fun (name, x) ->
                let open Option.Let_syntax in
                let%bind output = basket_price (Blueprint.output x) in
                let%map capital = basket_price (Blueprint.capital x) in
                name, output, capital, (output / capital)
            )
        |> List.sort ~compare:(Comparable.lift Float.compare ~f:(fun (_, _, _, p) -> p))
        |> List.iter ~f:(fun (name, output, capital, payback) ->
            printf "%60s %10.3f %10.1f %6.2f\n" name output capital (payback * 3600.)
         )
    in
    ()
(*    let m = Map.merge net gross ~f:(fun ~key:_ -> function
    | `Left x -> Some (x, 0.)
    | `Right y -> Some (0., y)
    | `Both (x, y) -> Some (x, y))
    in
    printf "%25s: %10s %10s (rel)\n" "item-name" "gross production" "net production";
    List.iter (Map.to_alist m) ~f:(fun (k,(net, gross)) ->
        printf "%25s: %10.3f %10.3f (%06.2f%%)\n" (Item_name.to_string k) gross net (net / gross * 100.)
    ); *)
;;

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

let run_competition () = Exchange.run_competition (
    List.map trivial_blueprints ~f:(fun (name, x) ->
        let output = Blueprint.output x in
        let capital = Blueprint.capital x in
        { Exchange.Contestant. name; output; capital }))

let recipe_category recipe = Game_data.lookup_recipe recipe |> fun recipe -> recipe.Recipe.category

let available_machines recipe_name =
    let recipe = Game_data.lookup_recipe recipe_name in
    let ingredient_count =
        List.length recipe.inputs
    in    
    List.filter Game_data.machines ~f:(fun m -> 
        List.exists m.categories ~f:(Category.(=) recipe.Recipe.category)
        && ((match m.ingredient_count with
        | None -> true
        | Some limit ->
            Int.(>=) limit ingredient_count
        ) || 
        (
            (* hack: allow "handcrafting" of machines by making them constructible in grey factories *)
            match recipe.Recipe.outputs with
            | [ _, item ] ->
                (match Game_data.lookup_machine item with
                | exception _ -> false
                | (_machine : Machine.t) -> true)
            | _ -> false
        )
        )
        )
        |> List.map ~f:(fun machine -> machine.name)

let default_machine recipe_name = 
    let machines_first_better =
        [
            "liquifier";
        ]
        |> List.map ~f:(Item_name.of_string)
    in
    let available_machines = 
        available_machines recipe_name
        |> Item_name.Set.of_list
    in
    match List.find machines_first_better ~f:(Set.mem available_machines) with
    | Some m -> Some m
    | None ->
        match (Set.to_list available_machines) with
        | x :: _ -> Some x
        | [] -> None

let autoadd_machine recipe = 
    let category = recipe_category recipe in
    if Category.(=) (Category.of_string "angels-converter") category then None
    else
    default_machine recipe

let all_trivial_blueprints () =
    List.concat_map Game_data.recipes ~f:(fun recipe ->
        List.map (available_machines recipe.name)
        ~f:(fun machine ->
            ((recipe, machine), (Blueprint.trivial
                ?inserters:None
                ?land_:None
                ~recipe:recipe.name
                ~machine
                ())))
    )

let recipes growth =
    (List.filter_map (all_trivial_blueprints ()) ~f:(fun ((recipe, machine), blueprint) ->
            let recipe = recipe.name in
            if
                List.exists [
                    (* "tree-arboretum-1" *)
                    (* "swamp-5"; "desert-5"; "temperate-4";"temperate-5"; "desert-4"; "desert-3";
                    "bob-rubber"; "nutrients-refining-3"; "desert-tree-arboretum-1"; *)
                ] ~f:(fun s -> Recipe_name.(=) recipe (Recipe_name.of_string s))
            then None
            else
            Some (let name = Recipe_name.to_string recipe ^ "@" ^ Item_name.to_string machine in
                name, (Map.filter 
                    ~f:(fun x -> Float.abs x > 1e-13)
                    (Value.(+) (Value.scale (Blueprint.capital blueprint) (-growth/(3600.))) (Blueprint.output blueprint))))
        ) @
        [
            (*"free-swamp-garden", Item_name.Map.singleton (Item_name.of_string "swamp-garden") 1.0;
            "free-desert-garden", Item_name.Map.singleton (Item_name.of_string "desert-garden") 1.0;
            "free-temperate-garden", Item_name.Map.singleton (Item_name.of_string "temperate-garden") 1.0; *)
            "steam-conversion", 
                Item_name.Map.of_alist_exn [
                    (Item_name.of_string "steam"), 1.0;
                    (Item_name.of_string "electrical-MJ"), -(150. * 200e-6);
                    ];
            "free-desert-tree", Item_name.Map.singleton (Item_name.of_string "desert-tree") 1.0;
            "free-viscous-water", Item_name.Map.singleton (Item_name.of_string "water-viscous-mud") 100.0;
            "big-bottle", 
                Item_name.Map.of_alist_exn [
                    (Item_name.of_string "big-bottle"), 1.0;
                    (Item_name.of_string "high-tech-science-pack"), -1.0;
                    (Item_name.of_string "science-pack-1"), -1.0;
                    (Item_name.of_string "science-pack-2"), -1.0;
                    (Item_name.of_string "science-pack-3"), -1.0;
                    (Item_name.of_string "productivity-module-4"), -0.1;
                    ];
        ])

let borism () =
    Lp.design_optimal_factory ~goal_item:(Item_name.of_string "big-bottle") (recipes 0.)

let improve_configuration_hardcoded () =
  (* let _ = assert false in *)
  let%map () =
    Writer.save
      ~contents:(Sexp.to_string_hum [%sexp (recipes 0.05 : (string * Value.t) list)]) "recipes.sexp"
  in
    let growth_via_prices = snd (
        binary_search 0.05 4.0 ~f:(fun growth ->
            match Lp.Item_prices.find (
                recipes growth
            ) with
            | `Too_easy -> false
            | `Ok _ -> true
            ))
    in
    let () = 
        match Lp.Item_prices.find (recipes growth_via_prices) with
        | `Too_easy -> assert false
        | `Ok solution ->
            Lp.Item_prices.report solution
    in
    (* let _ = assert false in *)
    let design_factory = Lp.design_optimal_factory ~goal_item:(Item_name.of_string "desert-garden") in
    let growth_via_design = 
        fst (binary_search 0.05 4.0 ~f:(fun growth ->
            Core.printf "attempting: %f\n%!" growth;
            match design_factory (recipes growth) with
            | exception _ -> true
            | _ -> false
        ))
    in
    Core.printf "growth: %f-%f\n%!" growth_via_design growth_via_prices;
    ()
    (* print_report (c @ List.map ~f:(fun (name, (recipe, blueprint)) ->
        {
            Configuration.V1.
            blueprint;
            quality = Expanding;
            quantity = 0.0;
            name = "auto: " ^ (Recipe_name.to_string recipe) ^ " for " ^ (Item_name.to_string name) ;
        }
    ) unique_producer_recipes); *)
;;

let rec improve_configuration () =
    let c = Configuration.load () in
    print_report c;
    Async_interactive.ask_yn "add last to configuration?"
    >>= function
    | true ->
        let _ = raise_s [%sexp "blergh"] in
        (* Configuration.save ({name; blueprint; quantity = 1.0; quality = Expanding} :: c); *)
        improve_configuration ()
    | false -> improve_configuration ()

let () =
    Command.run (Command.group ~summary:""
    [
    "competition", cmd run_competition;
    "improve", cmd_async (fun () ->
        improve_configuration ()
    );
    "improve-hardcoded", cmd_async (fun () ->
        improve_configuration_hardcoded ());
    "pp-game-data", cmd_async (fun () ->
        GG.pp ~path:"/home/rotsor/.factorio/script-output/game-data.sexp"
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
            ((recipe : Recipe.t).name : Recipe_name.t),
            (machine_name : Item_name.t),
            (r : Result.t)
(*            ,(List.take (Result.suggestions r) 6 : Suggestion.t list) *)
            ]
            )
            )
    in
    report "worst" (List.take (List.filter ~f:(fun i -> 
        not (hide_recipe ((fst (fst i) : Recipe.t).name))) l) 6);
    report "best" (List.take (List.rev l) 6);
    )) *)
    
    ])
