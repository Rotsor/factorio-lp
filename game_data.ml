open! Core

module Item_name = Game_data_raw.Item_name
module Entity_name = Game_data_raw.Entity_name
module Recipe_name  = Game_data_raw.Recipe_name
module Crafting_category = Game_data_raw.Crafting_category

module Product = struct

  type kind =
    | Fluid of
        { temperature : float option; }
    | Item
  [@@deriving sexp]

  type t = {
    kind : kind;
    name : Item_name.t;
    expected_amount : float;
  } [@@deriving sexp]

  let of_raw ({
      type_;
      name;
      amount;
      temperature;
      amount_min;
      amount_max;
      probability;
    } : Game_data_raw.Product.t) =
    {
      kind = (match (type_, temperature) with
        | `item, Some _ ->
          raise_s [%sexp "temperature of item was specified"]
        | `fluid, temperature ->
          Fluid { temperature }
        | `item, None -> Item
        );
      name;
      expected_amount =
        (let probabilistic_amount =
           match probability, amount_min, amount_max with
           | None, None, None -> None
           | Some p, Some min, Some max ->
             Some (p *. ((min +. max) /. 2.))
           | _ -> raise_s [%sexp "probabilistic amount not fully defined"]
         in
         match amount, probabilistic_amount with
         | None, None ->
           raise_s [%sexp "amount not defined"]
         | Some amount, None | None, Some amount -> amount
         | Some _, Some _ -> raise_s [%sexp "both probabilistic and deterministic amount specified"]
        );
    }
end

module Ingredient = struct
  type fluid_properties = {
    minimum_temperature : float option;
    maximum_temperature : float option;
  } [@@deriving sexp]
  type t = {
    type_ : [ `fluid of fluid_properties | `item ];
    name : Item_name.t;
    amount : float;
  } [@@deriving sexp]

  let of_raw ({
      type_;
      name;
      amount;
      minimum_temperature;
      maximum_temperature;
    } : Game_data_raw.Ingredient.t) =
    { type_ =
        (match type_, (minimum_temperature, maximum_temperature) with
         | `fluid, _ ->
           `fluid { minimum_temperature; maximum_temperature }
         | `item, (Some _, _ | _, Some _) ->
           raise_s [%sexp "min or max temperature specified for an item"]
         | `item, (None, None) ->
           `item);
      name;
      amount;
    }
end

module Effect = struct
  type t = Game_data_raw.Effect.t = {
    bonus : float;
  } [@@deriving sexp]
end

module Module_effects = struct
  type t = Game_data_raw.Module_effects.t = {
    consumption : Effect.t sexp_option;
    speed : Effect.t sexp_option;
    productivity : Effect.t sexp_option;
    pollution : Effect.t sexp_option;
  } [@@deriving sexp]
end

module Item_prototype = struct

  module As_fuel = struct
    type t = {
      category : string;
      fuel_value : float;
      burnt_result : Item_name.t option;
    } [@@deriving sexp]
  end

  module As_module = struct
    type t = {
      effects : Module_effects.t;
      category : string;
      tier : float;
      limitations : Recipe_name.t list;
    } [@@deriving sexp]
  end
  
  type t = {
    as_fuel : As_fuel.t option;
    place_result : Entity_name.t option;
    stack_size : int;
    rocket_launch_products : Product.t list option;
    as_module : As_module.t option;
  } [@@deriving sexp]

  let of_raw ({
    fuel_value;
    fuel_category;
    burnt_result;
    place_result;
    stack_size;
    rocket_launch_products;
    module_effects;
    module_category;
    module_tier;
    module_limitations
  } : Game_data_raw.Item_prototype.t) =
    let as_fuel =
      match (Game_data_raw.Joules.to_mj fuel_value, fuel_category, burnt_result) with
      | 0.0, None, None -> None
      | fuel_value, Some category, burnt_result ->
        Some { As_fuel.category; fuel_value; burnt_result }
      | _ -> raise_s [%sexp "inconsistent fuel_value, fuel_category amd burnt_result"]
    in
    let as_module =
      match module_effects, module_category, module_tier, module_limitations with
      | None, None, None, None -> None
      | Some effects, Some category, Some tier, Some limitations ->
        Some { As_module.effects; category; tier; limitations }
      | _ -> raise_s [%sexp "neither all nor none module fields specified"]
    in
    let rocket_launch_products = Option.map ~f:(List.map ~f:Product.of_raw) rocket_launch_products in
    ({ as_fuel; as_module; place_result; stack_size; rocket_launch_products; } : t)

end

module Entity_type = Game_data_raw.Entity_type

module Position = Game_data_raw.Position

module Bounding_box = Game_data_raw.Bounding_box

module Burner_prototype = struct
  type t = {
    effectivity : float;
    fuel_inventory_size : int;
    burnt_inventory_size : int;
    fuel_categories : bool String.Map.t;
  } [@@deriving sexp]
end

module Electrical_prototype = struct
  type t = {
    drain : float;
    output_flow_limit : float;
    input_flow_limit : float;
  } [@@deriving sexp]
end

module Entity_prototype = struct

(*  type boiler_power_source =
    | Electrical
    (*    | Liquid? *)
    | Burning of {
        effectivity : float;
      }
    | Thermal

  type generator_power_source =
    | Burning_liquid
    | Heated_liquid
    | Burning *)

  type kind =
    | Machine of { (* includes furnaces *)
        module_inventory_size : int;
        ingredient_count : int;
        fixed_recipe : Recipe_name.t option;
        crafting_speed : float;
        crafting_categories : Crafting_category.Set.t;
        max_energy_usage : float;
        power_source : [`chemical | `electrical];
      }
    | Offshore_pump of {
        pumping_fluid : Item_name.t;
        pumping_speed : float;
      }
(*    | Boiler of {
        boiler_target_temperature : float option;
        max_energy_usage : float;
        powered_by : boiler_power_source;
      }
    | Generator of {
        powered_by : generator_power_source;
        generator_maximum_temperature : float;
      }
    | Solar_panel of {
        power : float
      }
    | Beacon of {
        distribution_efficiency : float;
      }
    | Inserter of {
        drain : float;
      }*)
  [@@deriving sexp]

  type t = {
    collision_box : Bounding_box.t;
    drain : float;
    kind : kind;
  }
     [@@deriving sexp] 

     let of_raw (
         {
           type_;
           collision_box;
           module_inventory_size;
           ingredient_count;
           fixed_recipe;
           crafting_speed;
           crafting_categories;
           resource_categories;
           burner_prototype;
           max_energy_usage;
           electrical_prototype;
           generator_fluid_usage_per_tick = _;
           generator_maximum_temperature = _;
           solar_panel_production = _;
           beacon_distribution_effectivity = _;
           pumping_fluid;
           pumping_speed;
           boiler_target_temperature = _;
           generator_effectivity = _;
         } as ep
         : Game_data_raw.Entity_prototype.t) : t option =
       ignore resource_categories;
       ignore burner_prototype;
       ignore burner_prototype;
       let drain = match electrical_prototype with
         | None -> 0.0
         | Some { drain; output_flow_limit = _; input_flow_limit = _ } -> Game_data_raw.Joules_per_frame.to_MW drain
       in
       let kind =
         match (type_, module_inventory_size, ingredient_count, fixed_recipe, crafting_speed, crafting_categories) with
         | ((Assembling_machine | Furnace | Rocket_silo),
            Some module_inventory_size,
            Some ingredient_count, fixed_recipe, Some crafting_speed, Some crafting_categories) ->
           Some (Machine {
               module_inventory_size;
               ingredient_count;
               fixed_recipe;
               crafting_speed;
               crafting_categories = Set.of_map_keys crafting_categories;
               max_energy_usage = (match max_energy_usage with
                   | None -> raise_s [%sexp "no energy usage?"]
                   | Some x -> Game_data_raw.Joules_per_frame.to_MW x);
               power_source = (match electrical_prototype, burner_prototype with
                   | None, Some _ -> `chemical
                   | Some _, None -> `electrical
                   | Some _, Some _ -> raise_s [%sexp "both electrical and chemical?"]
                   | None, None -> raise_s [%sexp "no power source?"]);
             })
         | _ ->
           (match type_ with
            | (Assembling_machine | Furnace | Rocket_silo)
              ->
              raise_s [%sexp "failed to convert", (ep : Game_data_raw.Entity_prototype.t) ]
            | Offshore_pump ->
              (match pumping_fluid, pumping_speed with
               | Some pumping_fluid, Some pumping_speed ->
                 Some (Offshore_pump { pumping_fluid; pumping_speed = Game_data_raw.Units_per_frame.per_sec pumping_speed })
               | _ -> raise_s [%sexp "failed to convert an offshore pump", (ep : Game_data_raw.Entity_prototype.t) ]
              )
            | Corpse
            | Container
            | Logistic_container
            | Lamp
            | Arithmetic_combinator
            | Constant_combinator
            | Decider_combinator
            | Power_switch
            | Programmable_speaker
            | Transport_belt
            | Underground_belt
            | Splitter
            | Storage_tank
            | Loader
            | Generator
            | Inserter
            | Electric_pole
            | Pump
            | Curved_rail
            | Straight_rail
            | Train_stop
            | Rail_signal
            | Rail_chain_signal
            | Artillery_wagon
            | Cargo_wagon
            | Fluid_wagon
            | Car
            | Tank
            | Locomotive
            | Pipe
            | Pipe_to_ground
            | Logistic_robot
            | Construction_robot
            | Roboport
            | Boiler
            | Electric_energy_interface
            | Reactor
            | Heat_pipe
            | Solar_panel
            | Accumulator
            | Mining_drill
            | Beacon
            | Lab
            | Land_mine
            | Unit
            | Wall
            | Gate
            | Ammo_turret
            | Electric_turret
            | Fluid_turret
            | Artillery_turret
            | Radar
            | Player_port
            | Infinity_container
            | Simple_entity_with_force
            | Simple_entity_with_owner
            | Cliff
            | Player
            | Fish
            | Tree
            | Simple_entity
            | Rail_remnants
            | Turret
            | Unit_spawner
            | Market
            | Combat_robot
            | Projectile
            | Particle
            | Particle_source
            | Explosion
            | Artillery_flare
            | Artillery_projectile
            | Decorative
            | Character_corpse
            | Deconstructible_tile_proxy
            | Flame_thrower_explosion
            | Beam
            | Entity_ghost
            | Arrow
            | Fire
            | Sticker
            | Stream
            | Flying_text
            | Item_entity
            | Item_request_proxy
            | Leaf_particle
            | Smoke_with_trigger
            | Smoke
            | Tile_ghost
            | Resource
            | Rocket_silo_rocket
            | Rocket_silo_rocket_shadow -> None)
       in
       Option.map kind ~f:(fun kind -> { kind; drain; collision_box })

end

module Fluid_prototype = struct
  type t = {
    default_temperature : float;
    max_temperature : float;
    heat_capacity : float; (* J at max temperature *)
    gas_temperature : float;
    fuel_value : float;
  } [@@deriving sexp]

  let of_raw ({
      default_temperature;
      max_temperature;
      heat_capacity;
      gas_temperature;
      fuel_value;
    } : Game_data_raw.Fluid_prototype.t) =
    {
      default_temperature;
      max_temperature;
      heat_capacity;
      gas_temperature;
      fuel_value = Game_data_raw.Joules.to_mj fuel_value;
    }
end

module Recipe = struct
  type t = {
    category : Crafting_category.t;
    ingredients : Ingredient.t list;
    products : Product.t list;
    effort : float;
    enabled : bool;
  } [@@deriving sexp]  

  let of_raw ({
    category;
    ingredients;
    products;
    effort;
    enabled;
  } : Game_data_raw.Recipe.t) = {
    category;
    ingredients = List.map ingredients ~f:Ingredient.of_raw;
    products = List.map products ~f:Product.of_raw;
    effort;
    enabled;
  }
end
open Async

type t = {
  items : Item_prototype.t Item_name.Map.t;
  fluids : Fluid_prototype.t Item_name.Map.t;
  accessible_item_names : Item_name.Set.t;
  accessible_entities : Entity_prototype.t Item_name.Map.t;
  items_to_place : Item_name.t Entity_name.Map.t;
  recipes : Recipe.t Recipe_name.Map.t;
} [@@deriving sexp]

(*    ~/.factorio/script-output/game-data.sexp *)

let of_raw ({
    items;
    fluids;
    entities;
    recipes;
  } : Game_data_raw.t) =
  let items = Map.map ~f:Item_prototype.of_raw items in
  let recipes = Map.filter recipes ~f:(fun recipe -> recipe.enabled) in
  let accessible_item_names =
    List.concat_map (Map.data recipes) ~f:(fun recipe ->
        List.map recipe.products ~f:(fun product -> product.name)
      )
    |> Item_name.Set.of_list
  in
  let items_to_place =
    Map.filter_map items ~f:(fun item -> item.place_result)
    |> Map.to_alist
    |> List.map ~f:(fun (item_name, entity) -> entity, item_name)
    |> Entity_name.Map.of_alist_exn
  in
  { items;
    fluids = Map.map ~f:Fluid_prototype.of_raw fluids;
    items_to_place;
    accessible_item_names;
    accessible_entities =
      (Map.filter_map ~f:Entity_prototype.of_raw entities
      |> Map.to_alist
      |> List.filter_map ~f:(fun (entity_name, entity) ->
          match Map.find items_to_place entity_name with
          | Some k ->
            if Set.mem accessible_item_names k then
              Some (k, entity)
            else None
          | None -> None)
      |> Item_name.Map.of_alist_exn);
    recipes = Map.map recipes ~f:Recipe.of_raw;
  }
  

let entity_by_item_name t item_name =
  Map.find t.accessible_entities item_name

let load ~path =
  Reader.load_sexp_exn path [%of_sexp: Game_data_raw.t]
  >>= fun res ->
  return (of_raw res)
