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
        | Recipe of {
            machine : Item_name.t;
            recipe : Recipe_name.t;
            utilization : float;
        }
        | Offshore_pump of Item_name.t
    [@@deriving sexp]            
    type t = (float * component) list [@@deriving sexp]
end

type component = V1.component =
    | Passive of Item_name.t
    | Free_output of Item_name.t * float
    | Inserter of { utilization : float }
    | Recipe of {
        machine : Item_name.t;
        recipe : Recipe_name.t;
        utilization : float;
      }
    | Offshore_pump of Item_name.t
type t = (float * component) list

let add_componentwise ~f blueprint =
    Value.sum (List.map blueprint ~f:(fun (scale, component) ->
        Value.scale (f component) scale))

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
    | Recipe {
        machine;
        recipe;
        utilization;
    } ->
      let ({ kind; drain; collision_box = _ } : Game_data.Entity_prototype.t) = Option.value_exn (Game_data.entity_by_item_name game_data machine) in
      match kind with
      | Offshore_pump _ -> assert false
      | Machine machine ->
        let recipe = Map.find_exn game_data.recipes recipe in
        let recipes_per_second = 
          (machine.crafting_speed / recipe.effort) * utilization
        in
        let per_recipe =
          Value.(-)
              (Value.of_list (List.map recipe.products ~f:(fun { name; expected_amount; kind = _ } -> (expected_amount, name))))
              (Value.of_list (List.map recipe.ingredients ~f:(fun { name; amount; type_ =
                                                                                      (* CR: it's wrong to not take fluid constraints into account *)
                                                                                      _ } -> (amount, name))))
        in
        Value.(-) (Value.scale per_recipe recipes_per_second)
          (Value.of_list (
              [ drain, Item_name.electrical_mj ] @
              (match machine.power_source with
              | `chemical ->
                [((machine.max_energy_usage * utilization), Item_name.chemical_mj)]
              | `electrical -> 
                [((machine.max_energy_usage * utilization), Item_name.electrical_mj)])
            )))

let capital : t -> Value.t = 
    add_componentwise ~f:(
        function
        | Passive item -> Value.of_list [(1.0, item)]
        | Free_output _ -> Value.zero
        | Inserter _ -> Value.of_list [(1.0, Item_name.of_string "inserter")]
        | Recipe {
            machine;
            recipe = _;
            utilization = _;
          } -> (Value.of_list [1.0, machine])
        | Offshore_pump pump ->
          (Value.of_list [1.0, pump])
      )

module Bounding_box = Game_data_raw.Bounding_box

let trivial game_data ~recipe ~machine ?(inserters = 1., 1.) 
    ?land_
    () =
  let entity = Option.value_exn (Game_data.entity_by_item_name game_data machine) in
  let len = Bounding_box.weird_metric entity.collision_box in
  let land_ = Option.value land_ ~default:(Bounding_box.area_with_margin entity.collision_box) in
    [ 1., Recipe { machine; recipe; utilization = 1.0 };
        land_, Passive (Item_name.of_string "landfill-sand-3");
        fst inserters, Inserter { utilization = snd inserters };
        len, Free_output (Item_name.of_string "building-size", -1.0);
    ]
