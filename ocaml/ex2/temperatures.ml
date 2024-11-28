(*
Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer (Look at:

http://en.wikipedia.org/wiki/Comparison_of_temperature_scales
to read about them).

Write a function that given a pure number returns a conversion table for it among any of the 8 scales.
Write a function that given a temperature in a specified scale returns a list of all the corresponding temperatures in the other scales, note that the scale must be specified.
Hint. Define a proper datatype for the temperature.
*)

type scale =
  | Celsius
  | Fahrenheit
  | Kelvin
  | Rankine
  | Delisle
  | Newton
  | Reaumur
  | Romer

type temperature = { value : float; scale : scale }

let any2celsius (t : temperature) =
  match t.scale with
  | Celsius -> t
  | Fahrenheit -> { value = (t.value -. 32.) *. 5. /. 9.; scale = Celsius }
  | Kelvin -> { value = t.value -. 273.15; scale = Celsius }
  | Rankine -> { value = (t.value -. 491.67) *. 5. /. 9.; scale = Celsius }
  | Delisle -> { value = 100. -. (t.value *. 2. /. 3.); scale = Celsius }
  | Newton -> { value = t.value *. 100. /. 33.; scale = Celsius }
  | Reaumur -> { value = t.value *. 5. /. 4.; scale = Celsius }
  | Romer -> { value = (t.value -. 7.5) *. 40. /. 21.; scale = Celsius }

let celsius2any (t : temperature) (sc : scale) =
  match sc with
  | Celsius -> t
  | Fahrenheit -> { value = (t.value *. 9. /. 5.) +. 32.; scale = Fahrenheit }
  | Kelvin -> { value = t.value +. 273.15; scale = Kelvin }
  | Rankine -> { value = (t.value +. 273.15) *. 9. /. 5.; scale = Rankine }
  | Delisle -> { value = (100. -. t.value) *. 3. /. 2.; scale = Delisle }
  | Newton -> { value = t.value *. 33. /. 100.; scale = Newton }
  | Reaumur -> { value = t.value *. 4. /. 5.; scale = Reaumur }
  | Romer -> { value = (t.value *. 21. /. 40.) +. 7.5; scale = Romer }

let any2any (t : temperature) =
  let scales =
    [ Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reaumur; Romer ]
  in
  List.fold_left
    (fun acc elm -> (elm, celsius2any (any2celsius t) elm) :: acc)
    [] scales

let temp2string t =
  match t.scale with
  | Celsius -> Printf.sprintf "%4.5f°C" t.value
  | Fahrenheit -> Printf.sprintf "%4.5f°F" t.value
  | Kelvin -> Printf.sprintf "%4.5f°K" t.value
  | Rankine -> Printf.sprintf "%4.5f°R" t.value
  | Delisle -> Printf.sprintf "%4.5f°De" t.value
  | Newton -> Printf.sprintf "%4.5f°N" t.value
  | Reaumur -> Printf.sprintf "%4.5f°Ré" t.value
  | Romer -> Printf.sprintf "%4.5f°Rø" t.value

let print_table (tbl : (scale * temperature) list) =
  let rec print_body tbl =
    match tbl with
    | [] -> Format.printf "|\n"
    | (_, t) :: tl ->
        Format.printf "| %12s " (temp2string t);
        print_body tl
  in
  print_body tbl

let () = print_table (any2any { value = 0.; scale = Celsius })
let () = print_table (any2any { value = 932.0; scale = Fahrenheit })
let () = print_table (any2any { value = 1013.67; scale = Rankine })
