open Stock
open Portfolio
open Date
open Sys
open Str

(******************** TYPES ********************)

type user_id = string

type league = {
  players : (user_id * portfolio) list;
  budget : float;
  date : date;
  last_date : date;
  period : period;
  start_date : date;
  end_date : date;
  turns : int;
  move_limit: int option;
  stock_map : Yojson.Basic.t Stock.MyJson.t;
}

(************** HELPER FUNCTIONS ***************)
(** [print_stock s l] is a stringed representation of ticker [s] with 
    information about its value, change in value based on date and past date, 
    all present in [l]. *)
let print_stock (s : string) (l : league) = 
  let stock_name = String.sub s 0 ((String.length s) - 5) in
  try 
    let past = Stock.instantiate stock_name l.last_date l.stock_map in 
    let curr = Stock.instantiate stock_name l.date l.stock_map in 
    let diff = Stock.value curr -. Stock.value past in 
    let perc = diff *. 100. /. Stock.value past in 
    let pos_zero_int = if diff > 0. then Pos 
      else if diff < 0. then Neg else Zero in
    let diff_str = if diff >= 0. then "+" ^ Stock.str_value diff 
      else Stock.str_value diff in 
    let perc_str = if perc >= 0. then "+" ^ Stock.str_value perc ^ "%"
      else Stock.str_value perc ^ "%" in 
    ANSITerminal.(print_string [white] (stock_name ^ "\t"));
    begin match pos_zero_int with 
      | Pos -> Portfolio.pos_fn curr diff_str perc_str 7;
      | Neg -> Portfolio.neg_fn curr diff_str perc_str 7;
      | Zero -> Portfolio.zero_fn curr diff_str perc_str 7;
    end 
  with _ -> ANSITerminal.(print_string [white] (stock_name ^ "\t-\t-\t-\t-\t"))

(********** INTERFACE IMPLEMENTATIONS ***********)

let new_league budget period players start_date end_date string_json_map = {
  players = 
    players |> List.map (fun a -> (a, new_port budget a string_json_map));
  budget = budget;
  date = start_date;
  last_date = start_date;
  period = period;
  start_date = start_date;
  end_date = end_date;
  turns = num_of_turns start_date end_date period;
  stock_map = string_json_map;
  move_limit = None;
}

let new_tl_league budget period players start_date end_date string_json_map 
    turn_limit = {
  players = 
    players |> List.map (fun a -> (a, new_port budget a string_json_map));
  budget = budget;
  date = start_date;
  last_date = start_date;
  period = period;
  start_date = start_date;
  end_date = end_date;
  turns = num_of_turns start_date end_date period;
  stock_map = string_json_map;
  move_limit = Some turn_limit;
}

let update l = 
  let date' = next_date l.date l.period in
  let date' = if is_after l.end_date date' then date' else l.end_date in
  let budget_fn = fun (a,b) -> (a, b |> Portfolio.update date' l.budget) in
  match l.turns - 1 with
  | n when n > 0 -> 
    Some {l with turns = n; 
                 players = l.players |> List.map budget_fn; 
                 last_date = l.date;
                 date = date';}
  | _ -> None

let date l = l.date

let last_date l = l.last_date

let rounds l = l.turns

let players l = l.players

let map l = l.stock_map 

let has_move_limit l = 
  l.move_limit != None

let move_limit l = 
  match l.move_limit with 
  |None -> 0
  |Some x -> x

let get_portfolio l user = List.assoc user (players l)

let set_portfolio port user l = 
  {l with players = (user, port) :: (l.players |> List.remove_assoc user)}

let print_stocks (l : league) = 
  let all_json_stocks = Sys.readdir ("json") 
                        |> Array.to_list 
                        |> List.sort (Stdlib.compare) in
  ANSITerminal.(print_string [white] (
      "NAME\t\tVALUE\t$CHANGE\t%CHANGE\t\tNAME\t\tVALUE\t$CHANGE\t%CHANGE\n"));
  let rec two_time l = function 
    | s1 :: s2 :: t -> print_stock s1 l;
      print_string "\t";
      print_stock s2 l;
      print_endline "";
      two_time l t
    | s1 :: [] -> print_stock s1 l;
      print_endline "";
    | [] -> print_endline ""; in
  two_time l all_json_stocks

let rec listify acc n = function
  | [] -> acc
  | (x, y) :: t -> 
    let shorten s = String.sub (s |> str_of_len 20) 0 19 in 
    let to_listify = acc^ (string_of_int n) ^ ". "^
                     (shorten x)^ " " ^ (str_value y) ^ "\n" in 
    listify to_listify (n+1) t

let leaderboard l = 
  let val_y = fun (x, y) -> (x, value y) in
  let compare2 = fun (_, x) (_, y) -> compare y x in 
  l.players 
  |> List.map val_y 
  |> List.fast_sort compare2

let scoreboard l =
  let board_header = head1 "SCOREBOARD" in
  l 
  |> leaderboard 
  |> listify board_header 1 




