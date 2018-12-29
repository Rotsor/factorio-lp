open! Base
open! Core
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
        c
    else
        if f c then
        binary_search ~f a c
        else
        binary_search ~f c b


module Lazy_vector = Lazy_vector.Make(String)

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
        binary_search ~f:(fun growth -> 
            match (solve growth) with
            | `Positive, _ -> false
            | `Negative, _ -> true
            | `Mixed, (equation, solution) -> raise_s [%sexp "Mixed", (equation : Solver.equation), (solution : Solver.equation list)]
        ) 0.0 1.0
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
    let () = 
        let recipes_to_map = (fun (what, how_much) ->
                (what, Lazy_vector.to_map how_much)
            )
        in
        let solution = recipes_to_map solution in
        List.map equations ~f:recipes_to_map
        |>
        List.iter ~f:(fun ((what, _) as eqn) ->
            let get (_, how_much) =
                Option.value ~default:Rat.zero (Map.find how_much electricity_recipe_name)
            in
            printf
                !"%60s: %12.3f\n"
                    (Sexp.to_string ([%sexp  (what : Rat.t Item_name.Map.t)]))
                    (Rat.to_float (Rat.(/) 
                        (get eqn)
                        (get solution)))
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

let default_machine category = 
    let machines_first_better =
        [
            "liquifier";
        ]
        |> List.map ~f:(Item_name.of_string)
    in
    let available_machines = List.filter Game_data.machines ~f:(fun m -> 
        List.exists m.categories ~f:(Category.(=) category))
        |> List.map ~f:(fun machine -> machine.name)
        |> Item_name.Set.of_list
    in
    match List.find machines_first_better ~f:(Set.mem available_machines) with
    | Some m -> Some m
    | None ->
        match (Set.to_list available_machines) with
        | x :: _ -> Some x
        | [] -> None

let autoadd_machine category = 
    if Category.(=) (Category.of_string "angels-converter") category then None
    else
    default_machine category

let recipe_category recipe = Game_data.lookup_recipe recipe |> fun recipe -> recipe.Recipe.category

let configuration =
    let trivial ?inserters ?area count recipe machine =
        let recipe = Recipe_name.of_string recipe in
        let machine = if String.(=) machine "" then 
            (match (default_machine (recipe_category recipe)) with
            | Some machine -> machine
            | None ->
            raise_s [%sexp "no default machine for", (recipe : Recipe_name.t), (recipe_category recipe : Category.t)]) else Item_name.of_string machine in
        { 
            Configuration.V1.
            blueprint = Blueprint.trivial
                ?inserters:(Option.map inserters ~f:(fun cnt -> (cnt, 1.0)))
                ?land_:area
                ~recipe
                ~machine
                ();
            name = Recipe_name.to_string recipe ^ "@" ^ Item_name.to_string machine;
            quality = Expanding;
            quantity = count;
        }
    in
    let stockpile ~quantity item =
        let item = Item_name.of_string item in
        {
            Configuration.V1.
            blueprint = [ 
                1.0, Free_output (item, -. quantity);
            ];
            name = (if quantity > 0. then "stockpile: " else "get: ") ^ (Item_name.to_string item);
            quality = Expanding;
            quantity = 1.0;
        }
    in
    let free item = stockpile ~quantity:(-1.) item in
    let _ = free in
    let stockpile item = stockpile ~quantity:(1.) item in
    let farms = 66. in
    let power_tech = `t1 in
    (match power_tech with 
    | `t0 -> [ 
        trivial (12. * 0.533333333) ~inserters:0. "water-mineralized" "liquifier";
    ]
    | `t1 -> [
        trivial 1. "tree-arboretum-1" "";
        trivial 1. "angels-bio-void-algae-brown" "";
        trivial 1. "cellulose-fiber-raw-wood" "";
        trivial 1. "tree-generator-1" "";
        trivial 1. "solid-soil" "";
        free "desert-tree";
        trivial 1. ~inserters:0. "angels-chemical-void-gas-hydrogen-sulfide" "";
        trivial 1. ~inserters:0. "angels-water-void-water-saline" "";
        trivial 1. "washing-1" "";
        trivial 1. "washing-2" "";
        trivial 1. "washing-3" "";
        trivial 1. "washing-4" "";
        trivial 1. "washing-5" "";
        free "water-viscous-mud";
    ]
    ) @
    [
        trivial farms "algae-green" "algae-farm";
        trivial (farms * 0.8) "cellulose-fiber-algae" "assembling-machine-1";
        trivial (farms / 2.) "wood-pellets" "assembling-machine-1";
        trivial 9. ~inserters:0. "carbon-separation-2" "liquifier";
        trivial 48. ~inserters:3. "slag-processing-stone" "burner-ore-crusher";
        trivial 96. "dirt-water-separation" "angels-electrolyser";
        trivial (farms / 12.) ~inserters:2. "wood-bricks" "assembling-machine-1";
        trivial 66. "electrical-MJ" "steam-engine";
        trivial 24. "sb-wood-bricks-charcoal" "stone-furnace";
        trivial 27. ~inserters:0. "chemical-MJ" "free-conversion-machine";
        trivial 6. ~inserters:0. "water-pumpage" "offshore-pump";
        trivial 6. ~inserters:0. "angels-chemical-void-gas-hydrogen" "angels-flare-stack";
        trivial 6. ~inserters:0. "angels-chemical-void-gas-oxygen" "angels-flare-stack";
        trivial (12. * 0.533333333) ~inserters:3. "coke-purification" "liquifier";
        trivial 1. ~inserters:1. "landfill" "assembling-machine-1";
    ] @ (match power_tech with
    | `t0 -> [
        trivial 1. ~inserters:0. "water-purification" ""; ] 
    | `t1 -> [])
    @ [
        trivial 1. "solder-alginic" "";
        stockpile "quartz"; (* silicon ore *)
        stockpile "nickel-ore";
        stockpile "copper-plate";
    ] @
    (* ore processing *)
    (match `t2 with
    | `t0 -> 
        [ 
            trivial 1. ~inserters:2. "sb-water-mineralized-crystallization" "crystallizer";
            trivial 1. ~inserters:1. "angelsore3-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore1-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore2-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore4-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore5-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore6-crushed" "burner-ore-crusher";
        ]
    | `t1 ->
        [ 
            trivial 1. ~inserters:2. "sb-water-mineralized-crystallization" "crystallizer";
            trivial 1. ~inserters:1. "angelsore3-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore1-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore2-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore4-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore5-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore6-crushed" "burner-ore-crusher";
            trivial 1. "angelsore1-crushed-processing" "ore-sorting-facility";
            trivial 1. "angelsore3-crushed-processing" "ore-sorting-facility";

            (* the part of t2 necessary for green circuits *)
            trivial 1. ~inserters:1. "slag-processing-5" "";
            trivial 1. ~inserters:1. "slag-processing-6" "";
            trivial 1. ~inserters:1. "angelsore5-crushed" "burner-ore-crusher"; (* rubyite *)
            trivial 1. ~inserters:1. "angelsore6-crushed" "burner-ore-crusher"; (* bobmonium *)
            trivial 1. "angelsore6-crushed-processing" "ore-sorting-facility";
            trivial 1. "angelsore5-crushed-processing" "ore-sorting-facility";
            trivial 1. "slag-processing-filtering-1" "";
            trivial 1. "slag-processing-dissolution" "";
            trivial 1. "liquid-sulfuric-acid" "";
            trivial 1. "yellow-waste-water-purification" "";
            stockpile "sulfur";
            trivial 1. "gas-sulfur-dioxide" "";
            trivial 1. "water-synthesis" "";
            trivial 1. "filter-coal" "";
        ]
    | `t2 ->
        [
            trivial 1. ~inserters:1. "slag-processing-1" ""; (* crystallization to saphirite *)
            trivial 1. ~inserters:1. "slag-processing-5" "";
            trivial 1. ~inserters:1. "slag-processing-6" "";
            trivial 1. ~inserters:1. "angelsore1-crushed" "burner-ore-crusher";
            trivial 1. ~inserters:1. "angelsore5-crushed" "burner-ore-crusher"; (* rubyite *)
            trivial 1. ~inserters:1. "angelsore6-crushed" "burner-ore-crusher"; (* bobmonium *)
            trivial 1. "angelsore1-crushed-processing" "ore-sorting-facility";
            trivial 1. "angelsore6-crushed-processing" "ore-sorting-facility";
            trivial 1. "angelsore5-crushed-processing" "ore-sorting-facility";
            trivial 1. "slag-processing-filtering-1" "";
            trivial 1. "slag-processing-dissolution" "";
            trivial 1. "liquid-sulfuric-acid" "";
            trivial 1. "yellow-waste-water-purification" "";
            stockpile "sulfur";
            trivial 1. "gas-sulfur-dioxide" "";
            trivial 1. "water-synthesis" "";
            trivial 1. "filter-coal" "";
        ]
    ) @
    (* metallurgy *)
    (match `t3 with
    | `t0 -> 
        [
            trivial 1. ~inserters:0. "angelsore1-crushed-smelting" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore3-crushed-smelting" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore5-crushed-smelting" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore6-crushed-smelting" "stone-furnace";
            trivial 1. "steel-plate" "";
        ]
    | `t1 ->
        [
            trivial 1. ~inserters:0. "iron-plate" "stone-furnace";
            trivial 1. ~inserters:0. "copper-plate" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore5-crushed-smelting" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore6-crushed-smelting" "stone-furnace";            
            trivial 1. "steel-plate" "";
        ]
    | `t2 -> 
        [
            trivial 1. "molten-iron-smelting-1" "";
            trivial 1. "iron-ore-smelting" "";
            trivial 1. "angels-plate-iron" "";
            trivial 1. ~inserters:0. "copper-plate" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore5-crushed-smelting" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore6-crushed-smelting" "stone-furnace";
            trivial 1. "steel-plate" "";
        ]
    | `t3 ->
            [

            trivial 1. "molten-iron-smelting-1" "";
            trivial 1. "iron-ore-smelting" "";
            trivial 1. "angels-plate-iron" "";
            trivial 1. ~inserters:0. "copper-plate" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore5-crushed-smelting" "stone-furnace";
            trivial 1. ~inserters:0. "angelsore6-crushed-smelting" "stone-furnace";                
            trivial 1. "angels-plate-steel" "";
        ]
    )

let improve_configuration_hardcoded () =
    let c = configuration in
    let unique_producer_recipes =
        List.concat_map Game_data.recipes ~f:(fun recipe ->
            List.map recipe.outputs ~f:(fun (_amt, name) -> name, recipe.name))
        |> Item_name.Map.of_alist_multi
        |> Map.filter_mapi ~f:(fun ~key:_ ~data:recipes ->
            match recipes with
            | [] | _ :: _ :: _ -> None
            | [ recipe ] ->
                Option.map (autoadd_machine (recipe_category recipe))
                ~f:(fun machine ->
                    (recipe, (Blueprint.trivial
                        ?inserters:None
                        ?land_:None
                        ~recipe
                        ~machine
                        ())))
        )
        |> Map.filteri ~f:(fun ~key ~data:_ ->
            let already_produced =
                List.exists c ~f:(fun c ->
                    List.exists (Map.to_alist (Blueprint.output c.blueprint)) ~f:(fun (k, v) ->
                        v > 0. && Item_name.(=) k key
                    )
                )
            in
            not already_produced
        )
        |> Map.to_alist
    in
    print_report (c @ List.map ~f:(fun (name, (recipe, blueprint)) ->
        {
            Configuration.V1.
            blueprint;
            quality = Expanding;
            quantity = 0.0;
            name = "auto: " ^ (Recipe_name.to_string recipe) ^ " for " ^ (Item_name.to_string name) ;
        }
    ) unique_producer_recipes)
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
        improve_configuration_hardcoded ();
        return ()
    )
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
