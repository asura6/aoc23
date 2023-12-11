let read_lines name : string list =
  let ic = open_in name in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
        close_in ic;
        List.rev acc
  in
  loop []

let char_is_number = function x when x >= '0' && x <= '9' -> true | _ -> false

let edge_number (s : string) : int =
  let chars = s |> String.to_seq |> List.of_seq in
  let left_char = List.find char_is_number chars in
  let chars = List.rev chars in
  let right_char = List.find char_is_number chars in
  let num_str = String.of_seq (List.to_seq [ left_char; right_char ]) in
  int_of_string num_str

let get_edge_numbers (lines : string list) : int list =
  let rec loop lines edges =
    match lines with
    | [] -> ([], edges)
    | h :: t -> loop t (edge_number h :: edges)
  in
  let _, edges = loop lines [] in
  edges

let main =
  let lines = read_lines "day1/data.txt" in
  let edge_numbers = get_edge_numbers lines in
  let rec sum_list numbers acc =
    match numbers with [] -> acc | h :: t -> sum_list t acc + h
  in
  let sum = sum_list edge_numbers 0 in
  let () = print_endline "" in
  let () = print_int sum in
  let () = print_endline "" in
  ()
