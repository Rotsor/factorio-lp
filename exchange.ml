open! Core
open! Types_nice
open! Common


module Market_maker : sig
    type t

    val initial : t
    val auction : t -> [%sexp_of: 'a] -> ('a * Value.t) list -> (t * ('a * float) list)
    val sneaky_auction : t -> [%sexp_of: 'a] -> ('a * Value.t) list -> (t * ('a * float) list)
    val dropoff : t -> Value.t -> (t * float)
    val prices : t -> float Item_name.Map.t
    val price : t -> Item_name.t -> float
    val appraise : t -> Value.t -> float Item_name.Map.t
    val money_paid : t -> float
end = struct
    module Item_stock : sig
        type t
        val zero : Item_name.t -> t
        val price : t -> float

        val money_paid : t -> float
        val buy_inf_value : t -> float

        (* Client drops off some stock: stock grows, 
          price decreases if the amount is positive.
          Returns the amount the client earns. *)
        val dropoff : t -> float -> (t * float)
    end = struct
        type t = { amount : float; scale : float }

        let scale item = match Item_name.to_string item with
            | "electrical-MJ" -> 1.
            | "checmical-MJ" -> 1.
            | _ -> 1.
        let zero item = { scale = scale item; amount = - (scale item); }
        let price { amount; scale } = (- (amount / scale)) * 0.001 / scale
        let buy_inf_value { amount; scale } = - 0.001 * (amount * amount  / 2.) / scale / scale
(*        let price t = exp (- t)
        let buy_inf_value t = exp (- t) (* integral of price *) *)
        let dropoff t amount =
            let t' = { t with amount = t.amount + amount } in
            (t', buy_inf_value t' - buy_inf_value t)
        let money_paid t = - snd (dropoff t ((- (t.scale)) -t.amount))
    end

    type t = Item_stock.t Item_name.Map.t

    let money_paid t = 
        List.map (Map.data t) ~f:Item_stock.money_paid
        |> List.fold ~init:0. ~f:(+)

    let prices t = Map.map t ~f:Item_stock.price
    let dropoff_map t value =
        let m = Map.merge value t ~f:(fun ~key -> function
            | `Left value -> Some (Item_stock.dropoff (Item_stock.zero key) value)
            | `Right stock -> Some (stock, 0.0)
            | `Both (value, stock) -> Some (Item_stock.dropoff stock value)
            )
        in Map.map m ~f:fst, Map.map ~f:snd m
    let dropoff t value =
        let t', earnings =
            dropoff_map t value
        in
        t', (List.fold ~init:0. ~f:(+) (Map.data earnings))

    let initial = Item_name.Map.empty

    let price t key =
        Item_stock.price (Option.value (Map.find t key) ~default:(Item_stock.zero key))

    let appraise t value = 
        Map.mapi value ~f:(fun ~key ~data:v ->
            v * price t key)

    let auction (type a) (t : t) (sexp_of_a) (values : (a * Value.t) list) : t * (a * float) list =
        let combined_value = 
            Value.sum (List.map ~f:snd values)
        in
        let m, earnings = dropoff_map t combined_value in
        let average_price =
            Map.merge earnings combined_value ~f:(fun ~key:_ -> function
            | `Left earning -> 
                assert (earning = 0.);
                None
            | `Right value -> assert (value = 0.); None
            | `Both (earning, value) ->
                if (value = 0.) then None
                else Some (earning / value))
        in
        m, (
            List.map values ~f:(
                fun (i, m) ->
                let by_item = 
                (Map.mapi m ~f:(fun ~key ~data:quantity ->
                    quantity * 
                    (match (Map.find average_price key) with
                    | Some v -> v
                    | None -> price t key 
                    )
                ))
                in
                Map.data by_item |> List.fold ~f:(+) ~init:0.0
                |> fun res ->
                (if not (Float.is_finite res) then 
                raise_s [%sexp "infinite", {res : float; i : a; by_item : float Item_name.Map.t; earnings : float Item_name.Map.t}]
                );
                res
                |> fun s -> (i, s)
            ))

    let rec sneaky_auction : 'a . t -> [%sexp_of: 'a] -> ('a * Value.t) list -> (t * ('a * float) list) = 
        fun (type a) (t : t) (sexp_of_a) (values : (a * Value.t) list) ->
        let module M = struct
            type t = a * Value.t [@@deriving sexp_of]
        end
        in
        let (t', results) = auction t M.sexp_of_t (List.map values ~f:(fun v -> (v, snd v))) in
        let goods, bads = List.partition_tf results ~f:(fun (_r, f) -> f > 0.) in
        let really_bads, maybe_ok = 
            List.partition_map bads ~f:(fun ((a, r), _f) ->
                if List.fold ~init:0. ~f:(+) (Map.data (appraise t r)) > 1e-12
                then
                    `Snd (a, Value.scale r 0.3)
                else
                    `Fst a
            )
        in
        match bads with
        | [] -> 
            (t', List.map ~f:(fun ((a, _), v) -> (a, v)) goods)
        | _ :: _ ->
            let (t', goods) = sneaky_auction t sexp_of_a (List.map ~f:fst goods @ maybe_ok) in
            t', (List.map really_bads ~f:(fun a -> a, 0.) @ goods)
            

    let _ = Item_stock.price
    let _ = Item_stock.buy_inf_value
end

let shuffle t = t

module Contestant = struct
    type t = {
        name : string;
        capital : Value.t;
        output : Value.t;
    } [@@deriving sexp_of]
    let adjusted_output t ~growth_rate = 
        Value.(-) t.output (Value.scale t.capital growth_rate)
end

let power_report ~growth_rate mm (s : Contestant.t) p =
    let income_net = ref 0. in
    let income = 
        Map.mapi (s.Contestant.output) ~f:(fun ~key ~data ->
            let money = (Market_maker.price mm key * data) in
            income_net := !income_net + money;
            sprintf "(%.2f%s)=%s%.3f" data (Item_name.to_string key) (if money >= 0. then "+" else "-") (Float.abs money)
        )
    in
    let capital = 
        Map.data (Market_maker.appraise mm s.Contestant.capital)
        |> List.fold ~f:(+) ~init:0.
        |> fun res -> res * growth_rate
    in
    printf "%50s %10.3g: %10s (%s)\n" s.name (p) (sprintf "%f+%f" !income_net capital) (Map.data income |> String.concat ~sep:" ")

let take' ~keep l n =
    let rec go l n = match l with
        | x :: xs when keep x -> x :: go xs n
        | _x :: xs when Int.(=) n 0 -> go xs n
        | x :: xs -> x :: go xs (Int.(-) n 1)
        | [] -> []
    in
    go l n

let run_competition contestants =
    let states = 
        List.map contestants ~f:(fun c -> (c, ref 0.001))
    in
    let mm_state = ref Market_maker.initial in
    let states = shuffle states in
    let growth_rate = ref (1./360000.) in
    let bailouts = ref (String.Map.empty) in
    let bailout contestant amount =
        bailouts := Map.update !bailouts contestant.Contestant.name ~f:(function
            | None -> (1, amount)
            | Some (c, a) -> (Int.(+) c 1, a + amount)
            );
    in
    let rec go i () = 
        let max_power = ref (0., "") in
        let old () =List.iter (shuffle states)
            ~f:(fun (contestant, power) ->
                let value = 
                    Value.scale
                        (Contestant.adjusted_output contestant ~growth_rate:!growth_rate)
                        (!power *. 0.0001)
                in
                let (new_mm_state, money) = Market_maker.dropoff !mm_state value in
                mm_state := new_mm_state;
                (* make power always positive *)
                power := (let candidate = !power + money in if candidate <= 0. then !power / 2. else candidate);
                max_power := max !max_power (!power, contestant.Contestant.name);
                assert (!power >= 0.);
            ) 
        in
        let new_ () =
            let mm, earnings =
                Market_maker.sneaky_auction !mm_state [%sexp_of: Contestant.t * float ref] (List.map states ~f:(fun ((contestant, power) as state) -> (state, 
                    Value.scale
                        (Contestant.adjusted_output contestant ~growth_rate:!growth_rate)
                        (!power *. 0.1)
                )))
            in
            mm_state := mm;
            List.iter earnings ~f:(fun ((contestant, power), money) ->
                assert (money >= 0.);
                assert (!power >= 0.);
                power := (let candidate = !power + money in 
                assert (money = money);
                if candidate <= 0. then 
                    (let res = !power / 2. in bailout contestant (res - candidate); res)
                else candidate);
                assert (!power >= 0.);
                max_power := max !max_power (!power, contestant.Contestant.name);
                assert (!power >= 0.);
             )
        in
        let _ = old in
        let known_recipes = 
        [
            "carbon-separation-2@liquifier";
            "slag-processing-stone@ore-crusher";
            "algae-green@algae-farm";
            "cellulose-fiber-algae@assembling-machine-1";
            "wood-pellets@assembling-machine-1";
            "dirt-water-separation@angels-electrolyser";
            "sb-wood-bricks-charcoal@stone-furnace";
            "wood-bricks@assembling-machine-1";
            "water-mineralized@liquifier";
            "electrical-kJ@steam-engine";
        ]
        in
        if Int.(=) (i mod 100) 0 then (
        printf "money paid by mm: %g\n" (Market_maker.money_paid !mm_state);
        printf "total power: %g\n" (List.fold ~f:(+) ~init:0. (List.map states ~f:(fun (_, p) -> !p)));
        printf "bailouts:\n";
        print_s [%sexp (!bailouts : (int * float) String.Map.t)];
        printf "\ngrowth rate: %f\n" !growth_rate;
        List.iter (take' ~keep:(fun (c, _p) ->
            List.exists known_recipes ~f:(fun s -> String.(=) s c.Contestant.name)
            || String.is_substring ~substring:"electrical" c.Contestant.name
            )
            
            (sort_by ~f:(fun (_c,p) -> - (!p)) states) 1)
            ~f:(fun (s, p) -> power_report ~growth_rate:!growth_rate !mm_state s !p);
        printf "most expensive items:\n";
        List.iter (take' ~keep:(fun (c, _) -> 
            match Item_name.to_string c with
            | "charcoal" 
            | "wood-bricks"
            | "green-algae"
            | "gas-carbon-dioxide"
            | "water-mineralized"
            | "water"
            | "slag"
            | "stone-crushed"
            | "gas-hydrogen"
            | "gas-oxygen"
             ->  true
            | _ -> false
        ) (sort_by ~f:(fun (_c,f) -> - f) 
            (Map.to_alist (Market_maker.prices !mm_state))
        ) 5)
            ~f:(fun (i, p) -> printf "%30s %10.6g\n" (Item_name.to_string i) (p));
        printf "cheapest items:\n";
        List.iter (List.take (sort_by ~f:(fun (_c,f) -> f) 
            (Map.to_alist (Market_maker.prices !mm_state))
        ) 21)
            ~f:(fun (i, p) -> printf "%30s %10.3g\n" (Item_name.to_string i) (p)));
        printf "%!";        
        new_ ();
        growth_rate := fst !max_power;
        growth_rate := (1./36000000.);

        go (Int.(+) i 1) ()
    in
    go 0 ()
    