let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let get_as_int str n = int_of_char (String.get str n) - 48
let parse_freq_array str = Array.init (String.length str) (get_as_int str)

let sum_arrays arr1 str =
  let arr2 = parse_freq_array str in
  Array.map2 ( + ) arr1 arr2

let gamma_epsilon len lines =
  let freq_array = Array.make 12 0 in
  let freq_to_bdigits freq = if freq > len / 2 then (1, 0) else (0, 1) in
  Array.map freq_to_bdigits @@ List.fold_left sum_arrays freq_array lines

let digits_to_str arr =
  let str2_of_int2 (x, y) = (string_of_int x, string_of_int y) in
  let concat_str_pairs (x, y) (a, b) = (x ^ a, y ^ b) in
  Array.fold_left concat_str_pairs ("", "") @@ Array.map str2_of_int2 arr

let float_of_bdigit len (ind, digit) =
  match digit with
  | '1' -> 1.0 *. (2. ** float_of_int (len - ind - 1))
  | _ -> 0.

let float_of_bstring bstr =
  String.to_seqi bstr
  |> Seq.map (float_of_bdigit (String.length bstr))
  |> Seq.fold_left ( +. ) 0.

let multiply_bstrings (x, y) = float_of_bstring x *. float_of_bstring y
let lines = read_lines "day03/input.txt"

(* part 1 *)
let () =
  lines |> gamma_epsilon 1000 |> digits_to_str |> multiply_bstrings
  |> print_float |> print_newline

(* let print_tuple (a,b) = Printf.printf "(%f, %f)\n" a b *)

(* part 2 function *)
let life_support lines =
  let digits = List.map parse_freq_array lines in
  let rec aux index blist (a, b) =
    let freq_array = Array.make 12 0 in
    let freq_to_bdigit freq =
      if float_of_int freq >= float_of_int (List.length blist) /. 2. then a
      else b
    in
    let freq =
      Array.map freq_to_bdigit
      @@ List.fold_left (fun x y -> Array.map2 ( + ) x y) freq_array blist
    in
    match blist with
    | [] -> [||]
    | [ x ] -> x
    | _ :: _ ->
        aux (index + 1)
          (List.filter
             (fun s ->
               if Array.get s index = Array.get freq index then true else false)
             blist)
          (a, b)
  in
  let o2 = aux 0 digits (1, 0) in
  let co2 = aux 0 digits (0, 1) in
  (o2, co2)

(* part 2 *)
let () =
  let o2, co2 = life_support lines in
  let bdigits_to_str bdig =
    Array.fold_left ( ^ ) "" (Array.map string_of_int bdig)
  in
  multiply_bstrings (bdigits_to_str o2, bdigits_to_str co2)
  |> print_float |> print_newline
