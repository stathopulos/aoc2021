let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let part1 lst =
  let lst = List.map int_of_string lst in
  let rec aux count = function
    | a :: b :: rest ->
        if b > a then aux (count + 1) (b :: rest) else aux count (b :: rest)
    | _ -> count
  in
  aux 0 lst

let part2 lst =
  let list = List.map int_of_string lst in
  let rec aux count = function
    | a :: b :: c :: d :: rest ->
        if b + c + d > a + b + c then aux (count + 1) (b :: c :: d :: rest)
        else aux count (b :: c :: d :: rest)
    | _ -> count
  in
  aux 0 list

let solve filename =
  let content = read_lines filename in
  print_endline @@ "Part 1: " ^ string_of_int (part1 content);
  print_endline @@ "Part 2: " ^ string_of_int (part2 content)
;;

solve "day01/input.txt"
