open! Core

module Item_name : Identifiable = String
module Entity_name : Identifiable = String
module Recipe_name : Identifiable = String
module Crafting_category : Identifiable = String

module Product = struct
  type t = {
    type_ : [ `fluid | `item ];
    name : Item_name.t;
    amount : float option;
    temperature : float option;
    amount_min : float option;
    amount_max : float option;
    probability : float option;
  } [@@deriving sexp]
end

module Ingredient = struct
  type t = {
    type_ : [ `fluid | `item ];
    name : Item_name.t;
    amount : float;
    minimum_temperature : float option;
    maximum_temperature : float option;
  } [@@deriving sexp]
end

module Effect = struct
  type t = {
    bonus : float;
  } [@@deriving sexp]
end

module Module_effects = struct
  type t = {
    consumption : Effect.t sexp_option;
    speed : Effect.t sexp_option;
    productivity : Effect.t sexp_option;
    pollution : Effect.t sexp_option;
  } [@@deriving sexp]
end

module Item_prototype = struct
  type t = {
    fuel_value : float;
    place_result : Entity_name.t option;
    stack_size : float;
    fuel_category : string option;
    burnt_result : Item_name.t option;
    rocket_launch_products : Product.t list option;
    module_effects : Module_effects.t option;
    module_category : string option;
    module_tier : float option;
    module_limitations : Recipe_name.t list option;
  } [@@deriving sexp]
end

module Entity_type = struct
  type t =
    | Corpse
    | Assembling_machine
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
    | Offshore_pump
    | Furnace
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
    | Rocket_silo
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
    | Rocket_silo_rocket_shadow
  [@@deriving sexp]

  let t_of_sexp sexp =
    let sexp' =
      match sexp with
      | Sexp.Atom s ->
        Sexp.Atom (String.capitalize (String.tr ~target:'-' ~replacement:'_' s))
      | s -> s
    in
    match t_of_sexp sexp' with
    | exception _e -> t_of_sexp sexp
    | res -> res
end

module Position = struct
  type t = {
    x : float;
    y : float;
  } [@@deriving sexp]
end

module Bounding_box = struct
  type t = {
    left_top : Position.t;
    right_bottom : Position.t;
  } [@@deriving sexp]
end

module True = struct
  type t = unit

  let t_of_sexp sexp = match sexp with
    | Sexp.Atom "true" -> ()
    | s -> raise_s [%sexp "expected 'true', got", (s : Sexp.t)]

  let sexp_of_t () = Sexp.Atom "true"
end

module Burner_prototype = struct
  type t = {
    effectivity : float;
    fuel_inventory_size : int;
    burnt_inventory_size : int;
    fuel_categories : True.t String.Map.t;
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
  type t = {
    type_ : Entity_type.t;
    collision_box : Bounding_box.t;
    module_inventory_size : int option;
    ingredient_count : int option;
    fixed_recipe : Recipe_name.t option;
    crafting_speed : float option;
    crafting_categories : True.t Crafting_category.Map.t option;
    resource_categories : True.t String.Map.t option;
    burner_prototype : Burner_prototype.t option;
    max_energy_usage : float option;
    electrical_prototype : Electrical_prototype.t option;
    generator_fluid_usage_per_tick : float option;
    generator_maximum_temperature : float option;
    generator_effectivity : float option;
    solar_panel_production : float option;
    beacon_distribution_effectivity : float option;
    pumping_fluid : Item_name.t option;
    pumping_speed : float option;
    boiler_target_temperature : float option;
  } [@@deriving sexp]
end

module Fluid_prototype = struct
  type t = {
    default_temperature : float;
    max_temperature : float;
    heat_capacity : float; (* J at max temperature *)
    gas_temperature : float;
    fuel_value : float;
  } [@@deriving sexp]
end

module Recipe = struct
  type t = {
    category : Crafting_category.t;
    ingredients : Ingredient.t list;
    products : Product.t list;
    effort : float;
    emissions_multiplier : float;
    enabled : bool;
  } [@@deriving sexp]
end

open Async

type t = {
  items : Item_prototype.t Item_name.Map.t;
  fluids : Fluid_prototype.t Item_name.Map.t;
  entities : Entity_prototype.t Entity_name.Map.t;
  recipes : Recipe.t Recipe_name.Map.t;
} [@@deriving sexp]

(*    ~/.factorio/script-output/game-data.sexp *)

let pp ~path =
  Reader.load_sexp_exn path [%of_sexp: t]
  >>= fun res ->
  print_s [%sexp (res : t)];
  return ()
