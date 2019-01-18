open! Base


module Effects = struct
  type t = {
    consumption : float;
    speed : float;
    productivity : float;
    pollution : float;
  }

  let (+) = (+.)
  
  let effective_speed_multiplier t =
    Float.max 0.2 (1.0 + t.speed)

  let effective_productivity_multiplier t =
    1.0 + t.productivity

  let effective_power_multiplier t =
    Float.max (1.0 + t.consumption) 0.2

  let of_raw
      ({ consumption; speed; productivity; pollution } : Game_data_raw.Module_effects.t)
    =
    let f =function
      | None -> 0.0
      | Some ({ bonus } : Game_data_raw.Effect.t) -> bonus
    in
    {
      consumption = f consumption;
      speed = f speed;
      productivity = f productivity;
      pollution = f pollution;
    }

  let (+)
      { consumption = consumption1; speed = speed1; productivity = productivity1; pollution = pollution1 }
      { consumption = consumption2; speed = speed2; productivity = productivity2; pollution = pollution2 } =
    let (+) = (+.) in
    {
      consumption = consumption1 + consumption2;
      speed = speed1 + speed2;
      productivity = productivity1 + productivity2;
      pollution = pollution1 + pollution2;
    }

  let zero =
    {
      consumption = 0.;
      speed = 0.;
      productivity = 0.;
      pollution = 0.;
    }
  
  let scale
    {
      consumption;
      speed;
      productivity;
      pollution;
    } s =
    let ( * ) = ( *. ) in
    {
      consumption = consumption * s;
      speed = speed * s;
      productivity = productivity * s;
      pollution = pollution * s;
    }    
end
