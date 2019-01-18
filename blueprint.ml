open! Core
module G = Game_data
open Types_nice
module Game_data = G
open Common

module V1 = struct
    type component = 
        | Passive of Item_name.t
        | Free_output of Item_name.t * float
        | Inserter of { utilization : float }
        | Trivial_recipe of {
            machine : Item_name.t;
            recipe : Recipe_name.t;
        }
        | Offshore_pump of Item_name.t
    [@@deriving sexp]            
    type t = (float * component) list [@@deriving sexp]
end

type component = V1.component =
    | Passive of Item_name.t
    | Free_output of Item_name.t * float
    | Inserter of { utilization : float }
    | Trivial_recipe of {
        machine : Item_name.t;
        recipe : Recipe_name.t;
      }
    | Offshore_pump of Item_name.t
type t = (float * component) list

let add_componentwise ~f blueprint =
    Value.sum (List.map blueprint ~f:(fun (scale, component) ->
        Value.scale (f component) scale))

module Bounding_box = Game_data_raw.Bounding_box

let inserter_as_investment ~utilization : Investment.t = {
  output = Value.of_list [0. - (13.2 * utilization + 0.4) * 0.001, Item_name.electrical_mj];
  capital = Value.of_list [(1.0, Item_name.of_string "inserter")];
}

let trivial_recipe_output_and_capital game_data ~recipe ~machine =
  let entity = Option.value_exn (Game_data.entity_by_item_name game_data machine) in
  let len = Bounding_box.weird_metric entity.collision_box in
  let land_ = Bounding_box.area_with_margin entity.collision_box in
  let recipe_itself : Investment.t = {
    output =
      Crafting_recipes.crafting_recipe_output
        game_data ~machine ~recipe ~module_effects:(Modules.Effects.zero) ~utilization:1.0;
    capital = Value.of_list [1.0, machine];
  }
  in
  Investment.linear_combination     
    [ 1., recipe_itself;
      land_, Investment.passive_capital (Value.singleton (Item_name.of_string "landfill-sand-3") 1.0);
      1.0, inserter_as_investment ~utilization:1.0;
      len, Investment.free_output (Value.singleton (Item_name.of_string "building-size") (-1.0));
    ]

let output game_data : t -> Value.t = 
    add_componentwise ~f:(function
    | Passive _item -> Value.zero
    | Free_output (item, number) -> Value.of_list [number, item]
    | Inserter { utilization } -> Value.of_list [0. - (13.2 * utilization + 0.4) * 0.001, Item_name.electrical_mj]
    | Offshore_pump pump ->
      (match Option.value_exn (Game_data.entity_by_item_name game_data pump) with
      | { kind = Offshore_pump { pumping_fluid; pumping_speed }; drain; collision_box = _; } ->
        assert (Float.(=) drain 0.);
        Value.of_list [ pumping_speed, pumping_fluid ]
      | { kind = Machine _; _ } ->
        assert false)
    | Trivial_recipe {
        machine;
        recipe;
      } ->
      (trivial_recipe_output_and_capital game_data ~recipe ~machine).output)

let capital game_data : t -> Value.t = 
    add_componentwise ~f:(
        function
        | Passive item -> Value.of_list [(1.0, item)]
        | Free_output _ -> Value.zero
        | Inserter _ -> Value.of_list [(1.0, Item_name.of_string "inserter")]
        | Trivial_recipe {
            machine;
            recipe;
          } -> 
          (trivial_recipe_output_and_capital game_data ~recipe ~machine).capital
        | Offshore_pump pump ->
          (Value.of_list [1.0, pump])
      )

let trivial ~recipe ~machine () = [ 1., Trivial_recipe { machine; recipe; } ]
