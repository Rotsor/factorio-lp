open! Core
open! Common

let (=) = Rat.(=)

let ( * ) = Rat.( * )
let (+) = Rat. (+)
let (/) = Rat.(/)
let (-) = Rat.(-)

let (~-) = Rat.(~-)

module Id = Unique_id.Int ()

module Make(B : sig 
    type t [@@deriving sexp_of]
    include Comparable.S with type t := t
    end) : sig 
    type t [@@deriving sexp_of]

    val singleton : B.t -> t
    val (+) : t -> t -> t
    val scale : t -> Rat.t -> t

    val to_map : t -> Rat.t B.Map.t

end = struct
    
    type origin =
        | Singleton of B.t
        | Sum of t * t
        | Scale of t * Rat.t
        and
    t = {
        origin : origin;
        id : Id.t;
    }

    type back_edges = (t * (Rat.t * t) list) Id.Map.t
    let to_map =
        let add_edge a b ~strength (acc : back_edges) =
            match Map.find acc a.id with
            | Some (a', edges) -> 
                assert (phys_equal a' a);
                Map.set acc ~key:a.id ~data:(a, ((strength, b) :: edges))
            | None ->
                Map.set acc ~key:a.id ~data:(a, ((strength, b) :: []))
        in
        let rec collect_back_edges t ~visited (acc : back_edges) : back_edges =
            match Set.mem !visited t.id with
            | true -> acc
            | false ->
                visited := Set.add !visited t.id;
                match t.origin with
                | Singleton _ -> acc
                | Sum (x, y) ->
                    let acc = add_edge x t ~strength:Rat.one acc in
                    let acc = add_edge y t ~strength:Rat.one acc in
                    let acc = collect_back_edges x ~visited acc in
                    let acc = collect_back_edges y ~visited acc in
                    acc
                | Scale (x, c) ->
                    let acc = add_edge x t ~strength:c acc in
                    collect_back_edges x ~visited acc
        in
        fun main_t ->
            let back_edges = collect_back_edges main_t ~visited:(ref Id.Set.empty) (Id.Map.empty) in
            let rec
                eval t = 
                    if phys_equal t main_t
                    then
                        Rat.one
                    else
                        (match Map.find (Lazy.force m) t.id with
                        | None ->
                            raise_s [%sexp "no incoming edges. contribution must be zero, but why did we reach this case?"]
                        | Some (_, x) ->
                            Lazy.force x)
            and
             m = lazy (Map.map back_edges ~f:(fun (dest, edges) ->
                dest,
                lazy (
                    (
                    List.map edges ~f:(fun (strength, src) -> 
                        strength * eval src
                    )
                    |> List.fold ~init:Rat.zero ~f:(Rat.(+)))
                )
            ))
            in
            let m = 
                Map.data (Lazy.force m) 
                |> List.filter_map ~f:(fun (dest, strength) ->
                    match dest.origin with
                    | Singleton x ->
                        Some (x, Lazy.force strength)
                    | Scale _ ->
                        None
                    | Sum _ -> 
                        None
                )
                |> B.Map.of_alist_multi
                |> Map.map ~f:(List.fold ~init:Rat.zero ~f:Rat.(+))
            in
            m

    (*let evaluate ~evaluate origin k = match origin with
        | Singleton k2 -> (match B.(=) k k2 with
            | true -> Rat.one
            | false -> Rat.zero)
        | Sum (x, y) ->
            lookup x k + lookup y k
        
    let unmultiplied_lookup ~lookup cache ~origin k = 
        match Map.find !cache k with
        | Some v -> v
        | None ->
            let res = cache_miss ~lookup origin k in
            cache := Map.set !cache ~key:k ~data:res;
            res

    let rec lookup t k =
        (unmultiplied_lookup ~lookup t.cache ~origin:t.origin k) * t.scale

    let scale t c = { t with scale = t.scale * c } 

    let mk origin = 
        let cache = ref (B.Map.empty) in
        {
            scale = Rat.one;
            origin;
            cache;
        } *)

    let mk origin =
        let id = Id.create () in
        {
            id;
            origin;
        }

    let (+) x y = mk (Sum (x, y)) 

    let singleton x = mk (Singleton x)

    let scale x c = mk (Scale (x, c))

     let sexp_of_t t = [%sexp (to_map t : Rat.t B.Map.t)]
end

