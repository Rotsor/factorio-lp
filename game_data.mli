open Async

module Item_name = Game_data_raw.Item_name
module Recipe_name = Game_data_raw.Recipe_name
module Entity_name = Game_data_raw.Entity_name
module Crafting_category = Game_data_raw.Crafting_category
module Bounding_box = Game_data_raw.Bounding_box

module Module_effects : sig
  type t = Game_data_raw.Module_effects.t
end

module Fluid_prototype : sig
  type t = {
    default_temperature : float;
    max_temperature : float;
    heat_capacity : float; (* J at max temperature *)
    gas_temperature : float;
    fuel_value : float;
  } [@@deriving sexp]
end

module Product : sig

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

end

module Ingredient : sig
  type fluid_properties = {
    minimum_temperature : float option;
    maximum_temperature : float option;
  } [@@deriving sexp]
  type t = {
    type_ : [ `fluid of fluid_properties | `item ];
    name : Item_name.t;
    amount : float;
  } [@@deriving sexp]

end

module Item_prototype : sig

  module As_fuel : sig
    type t = {
      category : string;
      fuel_value : float;
      burnt_result : Item_name.t option;
    } [@@deriving sexp]
  end

  module As_module : sig
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
end

module Entity_prototype : sig

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
  [@@deriving sexp]

  type t = {
    collision_box : Bounding_box.t;
    drain : float;
    kind : kind;
  }
end

module Recipe : sig
  type t = {
    category : Crafting_category.t;
    ingredients : Ingredient.t list;
    products : Product.t list;
    effort : float;
    enabled : bool;
  }
end

type t = {
  items : Item_prototype.t Item_name.Map.t;
  fluids : Fluid_prototype.t Item_name.Map.t;
  accessible_item_names : Item_name.Set.t;
  accessible_entities : Entity_prototype.t Item_name.Map.t;
  items_to_place : Item_name.t Entity_name.Map.t;
  recipes : Recipe.t Recipe_name.Map.t;
} [@@deriving sexp]

val entity_by_item_name : t -> Item_name.t -> Entity_prototype.t option

val pp : path:string -> unit Deferred.t

val load : path:string -> t Deferred.t
