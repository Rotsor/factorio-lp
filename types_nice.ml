open! Core

type power = Types.power =
    | Chemical of { power : float }
    | Electrical of { power : float; drain : float }
[@@deriving sexp]

module Item_name0 : Identifiable = String
module Item_name = struct
    include Item_name0
    
    let electrical_mj = of_string "electrical-MJ"
    let chemical_mj = of_string "chemical-MJ"
end
module Recipe_name : Identifiable = String
module Category : Identifiable = String

module Recipe = struct
    type t = {
        name : Recipe_name.t;
        inputs : (float * Item_name.t) list;
        outputs : (float * Item_name.t) list;
        effort : float;
        category : Category.t;
    } [@@deriving sexp]
end

module Machine = struct
    type t = {
        name : Item_name.t;
        categories : Category.t list;
        crafting_speed : float;
        ingredient_count : int option;
        power : power;
        size_x : float;
        size_y : float;
    } [@@deriving sexp]
end
module Game_data = struct

    let machines = List.map Generated.machines ~f:(fun {name; categories; crafting_speed; power; ingredient_count; size_x; size_y } ->
        let name = Item_name.of_string name in
        let categories = List.map ~f:Category.of_string categories in
        { Machine.name; categories; crafting_speed; power; ingredient_count; size_x; size_y }) @ [
            {
                    name = Item_name.of_string "offshore-pump";
                    categories = [Category.of_string "offshore-pumpage"];
                    crafting_speed = 1.;
                    power = Chemical { power = 0. };
                    ingredient_count = None;
                    size_x = 2.; size_y = 2.;
            };
            {
                name = Item_name.of_string "steam-engine";
                categories = [Category.of_string "electricity"];
                crafting_speed = 0.9;
                power = Chemical { power = 0.9/.0.5 *. 1e6/.60. };
                ingredient_count = None;
                size_x = 5.; size_y = 2.;
            };
            {
                name = Item_name.of_string "steam-engine-2";
                categories = [Category.of_string "electricity"];
                crafting_speed = 1.8;
                power = Chemical { power = 1.8/.0.6 *. 1e6/.60. };
                ingredient_count = None;
                size_x = 5.; size_y = 2.;
            };
            {
                name = Item_name.of_string "free-conversion-machine";
                categories = [Category.of_string "free-conversion"];
                crafting_speed = 1.;
                power = Chemical { power = 0. };
                ingredient_count = None;
                size_x = 0.; size_y = 0.;
            };
        ]

    let expected_amount ~recipe_name ~output_name : Types.product_amount -> float option =
        fun w ->
        match w with
        | Deterministic x -> Some x
        | Types.Probabilistic {amount_min = _; amount_max = _; probability = Some 0.0} ->
            (match (String.is_substring (Item_name.to_string output_name) ~substring:"void") with
            | true -> None
            | false ->
                raise_s [%sexp "void not called void", (w : Types.product_amount), (recipe_name : Recipe_name.t), (output_name : Item_name.t)])
        | (Probabilistic { amount_min = Some amount_min; amount_max = Some amount_max; probability = Some probability }) ->
            Some ((amount_min +. amount_max) /. 2. *. probability)
        | _ ->
            raise_s [%sexp "never seen before", (w : Types.product_amount), (recipe_name : Recipe_name.t), (output_name : Item_name.t)]
                
    let recipes = List.map Generated.recipes ~f:(
        fun { name; inputs; outputs; effort; category } ->
            { Recipe.name = Recipe_name.of_string name;
            inputs = List.map inputs ~f:(fun (c, x) -> (c, Item_name.of_string x));
            outputs = List.filter_map outputs ~f:(fun (amount, output_name) ->
                let recipe_name = Recipe_name.of_string name in
                let output_name = Item_name.of_string output_name in
                Option.map (expected_amount ~recipe_name ~output_name amount)
                ~f:(fun amount -> (amount, output_name))
            ); effort; category = Category.of_string category }
    ) @ 
    [ {
            name = Recipe_name.of_string "electrical-MJ";
            inputs = [];
            outputs = [1., Item_name.electrical_mj];
            effort = 1.;
            category = Category.of_string "electricity";
        };
        {
            name = Recipe_name.of_string "chemical-MJ-charcoal";
            inputs = [1.0, Item_name.of_string "charcoal"];
            outputs = [5., Item_name.chemical_mj];
            effort = 1.;
            category = Category.of_string "free-conversion";
        };
        {
            name = Recipe_name.of_string "chemical-MJ-pellet-charcoal";
            inputs = [1.0, Item_name.of_string "pellet-charcoal"];
            outputs = [30., Item_name.chemical_mj];
            effort = 1.;
            category = Category.of_string "free-conversion";
        };
        {
            name = Recipe_name.of_string "free-conversion-machine";
            inputs = [];
            outputs = [1., Item_name.of_string "free-conversion-machine"];
            effort = 1.;
            category = Category.of_string "free-conversion";
        };
        {
            name = Recipe_name.of_string "water-pumpage";
            inputs = [];
            outputs = [1200., Item_name.of_string "water"];
            effort = 1.;
            category = Category.of_string "offshore-pumpage";
        };
         ]

    let lookup_machine = 
        let map = Item_name.Map.of_alist_exn (List.map machines ~f:(fun machine -> machine.name, machine)) in
        fun k -> match Map.find map k with
        | Some v -> v
        | None -> raise_s [%sexp "not found machine", (k : Item_name.t)]

    let lookup_recipe = 
        let map = Recipe_name.Map.of_alist_exn (List.map recipes ~f:(fun recipe -> recipe.name, recipe)) in
        fun k -> match Map.find map k with
        | Some v -> v
        | None -> raise_s [%sexp "not found recipe", (k : Recipe_name.t)]

end