
/silent-command

accessibleitems = {}


function ocaml_string (s)
    return "{qbl|" .. s .. "|qbl}"
end

function ocaml_float (f)
    return "Float.of_string(" .. ocaml_string(tostring(f)) .. ")"
end

function ocaml_float_option(f)
    if f == nil then
        return "None"
    else
        return "Some (".. ocaml_float(f) ..")"
    end
end


for a, b in pairs(game.player.force.recipes) do
    if b.enabled then
        for x,y in pairs (b.products) do
            accessibleitems[y.name] = 1
        end
    end
end

listitems = {}

machine_lines = {}
recipe_lines = {}

ignored_crafters = {}
for k,v in pairs(game.item_prototypes) do
    x="crafting_categories"
    x="crafting_speed"
    x="energy_usage"
    x="max_energy_usage"
    x="effectivity"
    x="burner_prototype"
    x="electric_energy_source_prototype"
    x="fluid_usage_per_tick"
    x="ingredient_count"
    x="module_inventory_size"
    x="allowed_effects"
    x="mining_speed"
    x="mining_power"
    is_smelter = 0
    if not (v.place_result == nil) and accessibleitems[k] == 1 then
        e = v.place_result
        cc = e.crafting_categories
        line = k
        if not (cc == nil) then
            drain = 0
            elec = e.electric_energy_source_prototype
            if not (elec == nil) then
                drain = elec.drain
            end
            line = k .. " (" .. v.name .. ")"  .. " " .. tostring(drain) .. " " .. e.energy_usage .. " " .. e.max_energy_usage .. " " ..  e.crafting_speed
            ocaml_categories={}
            for x,y in pairs(cc) do
                line = line .. " " .. x
                table.insert(ocaml_categories,ocaml_string(x))
            end
            table.insert(listitems, line)
            if not (elec == nil) then
                ocaml_power = "Electrical { power = " .. ocaml_float(e.max_energy_usage) .. "; drain = " .. ocaml_float(elec.drain) .. "}"
            else
                ocaml_power = "Chemical { power = " .. ocaml_float(e.max_energy_usage) .. "}"
            end
            ocaml_line = 
                "{ name =" .. ocaml_string(k) .. "; " ..
                "categories = [ " .. table.concat(ocaml_categories, "; ") .. "]; " ..
                "crafting_speed = " .. ocaml_float(e.crafting_speed) .. "; " ..
                "power = " .. ocaml_power .. "; " ..
                "};"
            table.insert(machine_lines,ocaml_line)
        else
            table.insert(ignored_crafters,k .. tostring(e.crafting_speed))
        end
        
    end
end

table.sort(listitems)
game.write_file("items.txt", table.concat(listitems, "\r\n"))

listresources = {}

function keys (t)
    local res = {}
    for k,v in pairs(t) do
        table.insert(res, k)
    end
    return res
end

for a, b in pairs(game.player.force.recipes) do
    if b.enabled then
    item = b.category .. ", " ..  b.energy .. "," .. b.name .. " ### " .. "   " .. " @ Produces : "
    ocaml_outputs={}
    for c,d in pairs (b.products) do
        if d.amount ~= nil then    
            table.insert(ocaml_outputs, "(Deterministic (" .. ocaml_float (d.amount) .. "))," .. ocaml_string (d.name))
            item = item .. d.amount .. ":" .. d.name .. ", "
        else
            table.insert(ocaml_outputs, "(Probabilistic " ..
              "{ amount_min = " .. ocaml_float_option (d.amount_min) .. ";" ..
              "amount_max = " .. ocaml_float_option (d.amount_max) .. ";" .. 
              "probability = " .. ocaml_float_option (d.probability) .. ";" ..
              "}), " .. ocaml_string (d.name))
            item = item .. "WARNING: " .. d.name .. ", "
        end
    end
    ocaml_inputs = {}
    item = item .. " @ Ingredients: "
    for x,y in pairs (b.ingredients) do
        table.insert(ocaml_inputs, ocaml_float (y.amount) .. "," .. ocaml_string (y.name))
        item = item .. y.amount .. ":" .. y.name .. ", "
    end
    ocaml_line = 
       "{ name =" .. ocaml_string(b.name) .. "; " ..
       "inputs = [ " .. table.concat(ocaml_inputs, "; ") .. "]; " ..
       "outputs = [ " .. table.concat(ocaml_outputs, "; ") .. "]; " ..
       "effort = " .. ocaml_float(b.energy) .. "; " ..
       "category = " .. ocaml_string (b.category) .. "; " ..
       "};"
    table.insert(listresources,item) 
    table.insert(recipe_lines,ocaml_line) 
    end
end
table.sort(listresources)
game.write_file("recipies.txt", table.concat(listresources, "\r\n"))
game.write_file("ignored_crafters.txt", table.concat(ignored_crafters, "\r\n"))
game.write_file("accessible_items.txt", table.concat(keys(accessibleitems), "\r\n"))


generated_lines = {}
table.insert(generated_lines,"open! Base")
table.insert(generated_lines,"open! Types")
table.insert(generated_lines,"let machines = [")
for i, l in pairs(machine_lines) do
    table.insert(generated_lines,l)
end
table.insert(generated_lines,"]")
table.insert(generated_lines,"let recipes = [")
for i, l in pairs(recipe_lines) do
    table.insert(generated_lines,l)
end
table.insert(generated_lines,"]")

game.write_file("generated.ml", table.concat(generated_lines, "\r\n"))