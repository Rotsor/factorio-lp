/silent-command

function atom(s)
  s = tostring(s)
  needs_quoting = string.find(s, '[^-a-z0-9A-Z_]')
  if(needs_quoting == nil)
  then
    r = s
  else
    needs_escaping = string.find(s, '["\\]')
    if(needs_escaping == nil)
    then
      r = "\"" .. s .. "\""
    else
      r = escape_jdgfhdkfgh(r)
    end
  end
  return { sexp_string = r }
end

function sexp_table(d, f5)
  local t = { }
  t[#t+1] = "("
  for k,v in pairs(d) do
      t[#t+1] = "(" .. atom(k).sexp_string .. " " .. f5(v).sexp_string .. ")"
  end
  t[#t+1] = ")"
  return { sexp_string = table.concat(t,"") }
end

function sexp_table_multiline(d, f5)
  local t = { }
  t[#t+1] = "("
  for k,v in pairs(d) do
      t[#t+1] = "\n(" .. atom(k).sexp_string .. " " .. f5(v).sexp_string .. ")"
  end
  t[#t+1] = ")"
  return { sexp_string = table.concat(t,"") }
end

function sexp_record(d)
  return sexp_table(d, function(v) return v end)
end

function sexp_array(array, f4)
  local t = { }
  t[#t+1] = "("
  for k,v in ipairs(array) do
    t[#t+1] = f4(v).sexp_string .. " "
  end
  t[#t+1] = ")"
  return { sexp_string = table.concat(t,"") }
end

function opt_atom(v)
  if v == nil then return { sexp_string = "()" }
  else return { sexp_string = "(" .. atom(v).sexp_string .. ")" }
  end
end

function sexp_product(v)
  if v == nil then nill_sexp_product ()
  else
  return sexp_record({
    type_ = atom(v.type),
    name = atom(v.name),
    amount = opt_atom(v.amount),
    temperature = opt_atom(v.temperature),
    amount_min = opt_atom(v.amount_min),
    amount_max = opt_atom(v.amount_max),
    probability = opt_atom(v.probability)
  })
  end
end

function sexp_ingredient(v)
  return sexp_record({
    type_ = atom(v.type),
    name = atom(v.name),
    amount = atom(v.amount),
    minimum_temperature = opt(atom)(v.minimum_temperature),
    maximum_temperature = opt(atom)(v.maximum_temperature),
  })
end

function sexp_name(x)
  return atom(x.name)
end

function sexp_table_c(f1)
  return function(v) return sexp_table(v, f1) end
end

function sexp_array_c(f2)
  return function(v) return sexp_array(v, f2) end
end

function opt(f3)
  return function(v) if v == nil then return {sexp_string = "()"} else
    return { sexp_string = "(" .. f3(v).sexp_string .. ")" } end end
end

function sexp_effects(v)
  return sexp_table(v, function(x) return sexp_table(x, atom) end)
end

function break_line(v)
  return ({ sexp_string = "\n" .. v.sexp_string })
end

function sexp_position(v)
  return sexp_table(v, atom)
end

function sexp_bounding_box(v)
  return sexp_table(v, sexp_position)
end

function sexp_electric_energy_source_prototype(v)
  return sexp_record({
    drain = atom(v.drain),
    output_flow_limit = atom(v.output_flow_limit),
    input_flow_limit = atom(v.input_flow_limit),
  })
end

function sexp_burner_prototype(v)
  return sexp_record({
    effectivity = atom(v.effectivity),
    fuel_inventory_size = atom(v.fuel_inventory_size),
    burnt_inventory_size = atom(v.burnt_inventory_size),
    fuel_categories = sexp_table(v.fuel_categories, atom)
  })
end


contents = sexp_record({
  items =
    sexp_table_multiline(game.item_prototypes, function (v)
      return sexp_record({
        fuel_value = atom(v.fuel_value),
        place_result = opt(sexp_name)(v.place_result),
        stack_size = atom(v.stack_size),
        fuel_category = opt(atom)(v.fuel_category),
        burnt_result = opt(sexp_name)(v.burnt_result),
        rocket_launch_products = opt(sexp_array_c(sexp_product))(v.rocket_launch_products),
        module_effects = opt(sexp_effects)(v.module_effects),
        module_category = opt(atom)(v.category),
        module_tier = opt(atom)(v.tier),
        module_limitations = opt(sexp_array_c(atom))(v.limitations)
      })
    end),
  fluids =
    sexp_table_multiline(game.fluid_prototypes, function(v)
      return sexp_record({
        fuel_value = atom(v.fuel_value),
        default_temperature = atom(v.default_temperature),
        max_temperature = atom(v.max_temperature),
        heat_capacity = atom(v.heat_capacity),
        gas_temperature = atom(v.gas_temperature),
      })
      end),
  entities = 
    sexp_table_multiline(game.entity_prototypes, function(v)
      return sexp_record({
        type_ = atom(v.type),
        collision_box = sexp_bounding_box(v.collision_box),
        module_inventory_size = opt(atom)(v.module_inventory_size),
        ingredient_count = opt(atom)(v.ingredient_count),
        crafting_speed = opt(atom)(v.crafting_speed),
        crafting_categories = opt(sexp_table_c(atom))(v.crafting_categories),        
        resource_categories = opt(sexp_table_c(atom))(v.resource_categories),
        max_energy_usage = opt(atom)(v.max_energy_usage),
        burner_prototype =
          opt(sexp_burner_prototype)(v.burner_prototype),
        electrical_prototype =
          opt(sexp_electric_energy_source_prototype)(v.electric_energy_source_prototype),
        generator_fluid_usage_per_tick = 
          opt(atom)(v.fluid_usage_per_tick),
        generator_maximum_temperature =
          opt(atom)(v.maximum_temperature),
        boiler_target_temperature =
          opt(atom)(v.target_temperature),
        pumping_fluid = opt(sexp_name)(v.fluid),
        pumping_speed = opt(atom)(v.pumping_speed),
        solar_panel_production = opt(atom)(v.production),
        generator_effectivity = opt(atom)(v.effectivity),
        beacon_distribution_effectivity = opt(atom)(v.distribution_effectivity),
        fixed_recipe = opt(atom)(v.fixed_recipe),
      })
      end),
  recipes =
    sexp_table_multiline(game.player.force.recipes, function(v)
      return sexp_record({
        enabled = atom(v.enabled),
        category = atom(v.category),
        ingredients = sexp_array(v.ingredients, sexp_ingredient),
        products = sexp_array(v.products, sexp_product),
        effort = atom(v.energy),
      })
      end
    ),
})

game.write_file("game-data.sexp", contents.sexp_string)
