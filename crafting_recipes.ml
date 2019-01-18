open! Base

let (/) = Float.(/)
let ( * ) = Float.( * )
open Game_data

let crafting_recipe_output game_data ~recipe ~machine ~module_effects ~utilization =
  let ({ kind; drain; collision_box = _ } : Game_data.Entity_prototype.t) = Option.value_exn (Game_data.entity_by_item_name game_data machine) in
  match kind with
  | Offshore_pump _ -> assert false
  | Machine machine ->
    let recipe = Map.find_exn game_data.recipes recipe in
    let recipes_per_second = 
      (machine.crafting_speed / recipe.effort) * utilization * (Modules.Effects.effective_speed_multiplier module_effects)
    in
    let power = (machine.max_energy_usage * utilization) * (Modules.Effects.effective_power_multiplier module_effects) in
    let per_recipe =
      Value.(-)
        (Value.scale (Value.of_list (List.map recipe.products ~f:(fun { name; expected_amount; kind = _ } -> (expected_amount, name)))) (Modules.Effects.effective_productivity_multiplier module_effects))
        (Value.of_list (List.map recipe.ingredients ~f:(fun { name; amount; type_ =
                                                                              (* CR: it's wrong to not take fluid constraints into account *)
                                                                              _ } -> (amount, name))))
    in
    Value.(-) (Value.scale per_recipe recipes_per_second)
      (Value.of_list (
          [ drain, Item_name.electrical_mj ] @
          (match machine.power_source with
           | `chemical ->
             [(power, Item_name.chemical_mj)]
           | `electrical -> 
             [(power, Item_name.electrical_mj)])
        ))
