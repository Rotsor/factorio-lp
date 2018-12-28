open! Core
open Types_nice
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
type t = (float * component) list

let add_componentwise ~f blueprint =
    Value.sum (List.map blueprint ~f:(fun (scale, component) ->
        Value.scale (f component) scale))

let output : t -> Value.t = 
    add_componentwise ~f:(function
    | Passive _item -> Value.zero
    | Free_output (item, number) -> Value.of_list [number, item]
    | Inserter { utilization } -> Value.of_list [0. - (13.2 * utilization + 0.4) * 0.001, Item_name.electrical_mj]
    | Recipe {
        machine;
        recipe;
        utilization;
    } ->
        let machine = Game_data.lookup_machine machine in
        let recipe = Game_data.lookup_recipe recipe in
        let recipes_per_second = 
            (machine.crafting_speed / recipe.effort) * utilization
        in
        let per_recipe =
            Value.(-)
                (Value.of_list recipe.outputs)
                (Value.of_list recipe.inputs)
        in
        Value.(-) (Value.scale per_recipe recipes_per_second)
            (Value.of_value (
                match machine.power with
                | Chemical { power } ->
                    [((power * utilization * 60. / 1e6), Item_name.chemical_mj)]
                | Electrical { power; drain } ->
                    [(((power * utilization + drain) * 60. / 1e6), Item_name.electrical_mj)]
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
        } -> (Value.of_list [1.0, machine]))

let trivial ~recipe ~machine ?(inserters = 1., 1.) ?(land_ = 5. * 5.) () =
    [ 1., Recipe { machine; recipe; utilization = 1.0 };
        land_, Passive (Item_name.of_string "landfill-sand-3");
        fst inserters, Inserter { utilization = snd inserters };
    ]
