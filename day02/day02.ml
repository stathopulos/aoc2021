let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

(* part 1 *)
type point = int * int

let c_forward (x, y) mag = (x + mag, y)
let c_up (x, y) mag = (x, y - mag)
let c_down (x, y) mag = (x, y + mag)

(* part 2 *)
type aim_point = point * int

let aim_down ((x, y), aim) arg = ((x, y), aim + arg)
let aim_up ((x, y), aim) arg = ((x, y), aim - arg)
let aim_forward ((x, y), aim) arg = ((x + arg, y + (aim * arg)), aim)

let process (forward, up, down, point) lst =
  let process_line point str =
    match String.split_on_char ' ' str with
    | [ "forward"; mag ] -> forward point (int_of_string mag)
    | [ "up"; mag ] -> up point (int_of_string mag)
    | [ "down"; mag ] -> down point (int_of_string mag)
    | _ -> point
  in
  let rec process_list point = function
    | h :: t -> process_list (process_line point h) t
    | [] -> point
  in
  process_list point lst

let solve sub eval =
  read_lines "day02/input.txt" |> process sub |> eval |> print_endline

let _ =
  let part_1 (x, y) = "Part 1: " ^ string_of_int (x * y) in
  let c_sub = (c_forward, c_up, c_down, (0, 0)) in
  solve c_sub part_1

let _ =
  let part_2 ((x, y), _) = "Part 2: " ^ string_of_int (x * y) in
  let aim_sub = (aim_forward, aim_up, aim_down, ((0, 0), 0)) in
  solve aim_sub part_2
