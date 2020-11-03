open Date
open Portfolio

type 'a out = 
  | Legal of 'a 
  | Illegal of string

(* ##### GAME VARIABLES #####*)
let player_cap = 4
let player_floor = 1
let date_cap = (2019, 12, 10)
let date_floor = (1998, 12, 08)
let cash_floor = 100.
let cash_cap = 1000000.

(* ##### MESSAGES ##### *)
let cash_floor_str = 
  cash_floor 
  |> int_of_float 
  |>string_of_int

let cash_cap_str = 
  cash_cap 
  |> int_of_float 
  |>string_of_int

let bad_command_message = "Sorry, I didn't understand that Empty Command."
let proceed = "How would you like to proceed?"
let num_players_error_mess = "Our system does not support that many players, 
please re-input"
let invalid_input_mess =  "You have provided invalid input."
let invalid_also = "Invalid Input. type \"info\" for more information on the "
                   ^"available commands, if you need. Pls try again;"
let invalid_name_exists = "That name is already taken, pls input another."
let invalid_name_empty = "You need a non-empty name, pls input another."
let invalid_date_early = "The date you have chosen is too early. pls input"^
                         " another"
let invalid_date_late = "The date you have chosen is too late. pls input "^
                        "another"
let invalid_end_early = "The date you have chosen is before your start date."^
                        " pls input another"
let invalid_cash_floor = "Your cash was too low so we gave you " ^ 
                         cash_floor_str ^ ", the lowest permitted amount."
let invalid_cash_cap = "Your cash was too high so we gave you " ^ 
                       cash_cap_str ^ ", the highest permitted amount."
let invalid_period_floor = "Your period choice was too low so we made it 1."
let invalid_period_cap = "Your period choice was too high pls put another one "
                         ^"in."
let command_information = "These are the commands you can use:\n
- info                  -> shows this information.
- board                 -> shows a list of current stock information.
- end                   -> ends the simulation.
- portfolio             -> gives a description of your current portfolio.
- leaderboard           -> shows current score.
- cash                  -> shows current spendable liquid money.
- liquidate             -> turns all assets into cash.
- sell <stock> <amount> -> sells money <amount> of your <stock> from your 
                           portfolio. sell <stock> 'all' sells all of your 
                           stocks of a particular company and gets rid of the 
                           stock in your portfolio
- buy <stock> <amount>  -> buys money <amount> of <stock> from the market. sell 
                           <stock> 'all' spends all your liquid cash on that 
                           one stock."

let intro_dialogue = "\n\n\n\n\nIt's time to test your investing skills.\n" ^
                     "Welcome to our Stock Simulator! \n"^ 
                     "Created by Johnathan, Obii, and Teresa!\n\n\n\n\n"

(* ##### FUNCTIONS ##### *)

let rec print_list = function 
  | [] -> ()
  | e::l -> print_string e ; 
    print_endline"" ; 
    print_list l 

let mod_period n = function
  |Weekly _ -> Weekly n
  |Monthly _ -> Monthly n
  |Daily _ -> Daily n

let periodify = function
  | "WEEKLY" -> Weekly 1
  | "MONTHLY" -> Monthly 1
  | "DAILY" -> Daily 1
  | _ -> failwith "TF?"

let amountify = function
  | "ALL" -> All
  | n -> if (float_of_string n) >= 0. then Amnt (float_of_string n) else 
      failwith "negative number"

let clean str =
  str 
  |> String.split_on_char ' ' 
  |> List.filter (fun x -> not (x = "")) 
  |> String.concat " "

let clean_date str =
  str 
  |> String.split_on_char ' ' 
  |> String.concat ""
  |> String.split_on_char '-'
  |> List.filter (fun x -> not (x = ""))

let rec in_player_name lst = 
  let name = clean (read_line()) in
  match name with
  | "" -> print_endline invalid_name_empty; 
    in_player_name lst
  | s when List.exists (fun x -> x = s) lst -> 
    print_endline invalid_name_exists; 
    print_string "> ";
    in_player_name lst
  | s -> s :: lst

let rec in_num_player () = 
  print_endline "How many players do you have?";
  print_string  "> ";
  try 
    let num = read_line() 
              |> clean
              |> int_of_string in
    begin match num with
      | n when n < player_floor -> 
        print_endline num_players_error_mess; 
        in_num_player ()
      | n when n > player_cap -> 
        print_endline num_players_error_mess; 
        in_num_player ()
      | exception r-> 
        print_endline invalid_input_mess; 
        in_num_player ()
      | n -> n
    end 
  with _ -> print_endline invalid_input_mess; in_num_player ()

let rec in_cash () = 
  print_endline "How much cash would you like to start with?";
  print_string  "> ";
  try
    let num = 
      read_line ()
      |> clean
      |> float_of_string in
    begin match num with
      | n when (Float.compare n cash_floor) < 0 -> 
        print_endline invalid_cash_floor; cash_floor
      | n when (Float.compare n cash_cap) > 0-> 
        print_endline invalid_cash_cap; cash_cap
      | exception r-> 
        print_endline invalid_input_mess; in_cash ()
      | n -> n
    end 
  with _ -> print_endline invalid_input_mess; in_cash ()

let rec in_period_helper p =
  let prompt = match p with
    |Daily _ -> "days"
    |Monthly _ -> "months"
    |Weekly _ -> "weeks"
  in
  print_endline ("how many " ^ prompt ^ " do you want to pass each turn?");
  print_string "> ";
  try 
    let num = 
      read_line()
      |> clean
      |> int_of_string in
    begin match num with
      | n when n < 1 -> print_endline invalid_period_floor; p
      | exception r-> print_endline invalid_input_mess; in_period_helper p
      | n -> mod_period n p
    end
  with _ -> print_endline invalid_input_mess; in_period_helper p

let rec in_period () = 
  print_endline 
    "How should time progress between turns? Daily, Weekly or Monthly?";
  print_string "> ";

  match read_line()
        |> clean 
        |> String.uppercase_ascii 
        |> periodify  
  with
  | exception _ -> print_endline invalid_input_mess; in_period ()
  | n -> in_period_helper n

let rec mode_helper () =
  print_endline 
    "How many transactions should players be allowed to make per round?";
  print_string "> ";
  try 
    let num = 
      read_line()
      |> clean 
      |> int_of_string in
    match num with
    | n when n < 1 -> print_endline invalid_input_mess; mode_helper ()
    | exception _ -> print_endline invalid_input_mess; mode_helper ()
    | n when n > 10 -> print_endline 
                         "That is too many transactions. Please try again.";
      mode_helper()
    | n -> Some n
  with _ -> print_endline invalid_input_mess; mode_helper()

let rec in_mode () = 
  print_endline 
    "What mode would you like to play in? Easy or Hard?";
  print_string "> "; 
  match read_line()
        |> clean
        |> String.uppercase_ascii with 
  | exception _ -> print_endline invalid_input_mess;
    in_mode()
  | "EASY" -> None
  | "HARD" -> mode_helper ()
  | _ -> print_endline invalid_input_mess; 
    in_mode()

let league_maker mode cash_start pd players start_date end_date stock_map =
  match mode with 
  | None -> 
    State.new_league cash_start pd players start_date end_date stock_map
  | Some (x) -> 
    State.new_tl_league cash_start pd players start_date end_date stock_map x

let rec in_start () = 
  print_endline "What day would you like to start the simulation on? 
 NOTE: format should be dd-mm-yyyy";
  print_string  "> ";
  try let date = 
        read_line()
        |> clean_date
        |> List.map int_of_string in
    begin match date with
      | d :: m :: y :: [] when not (is_valid_date (y,m,d) ) -> 
        print_endline invalid_input_mess; in_start ()
      | d :: m :: y :: [] when not (is_after (y,m,d) date_floor )-> 
        print_endline invalid_date_early; in_start ()
      | d :: m :: y :: [] when not (is_after date_cap (y,m,d) )-> 
        print_endline invalid_date_late; in_start ()
      | d :: m :: y :: [] -> (y,m,d)
      | _ -> print_endline invalid_input_mess; in_start ()
    end
  with _ -> print_endline invalid_input_mess; in_start ()

let rec in_end ds = 
  print_endline "What day would you like to end the simulation on? 
NOTE: format should be dd-mm-yyyy";
  print_string  "> ";
  try let date = read_line()
                 |> clean_date
                 |> List.map int_of_string in
    match date with
    | d :: m :: y :: [] when not (is_valid_date (y,m,d) ) -> 
      print_endline invalid_input_mess; in_end ds
    | d :: m :: y :: [] when not (is_after (y,m,d) date_floor )-> 
      print_endline invalid_date_early; in_end ds
    | d :: m :: y :: [] when not (is_after date_cap (y,m,d) )-> 
      print_endline invalid_date_late; in_end ds
    | d :: m :: y :: [] when not (is_after (y,m,d) ds )-> 
      print_endline invalid_end_early; in_end ds
    | d :: m :: y :: [] -> (y,m,d)
    | _ -> print_endline invalid_input_mess; in_end ds
  with _ -> print_endline invalid_input_mess; in_end ds


let rec print_player_names n lst = 
  match n with 
  | 0 -> lst
  | x -> print_endline("What is the username of Player " ^ 
                       (string_of_int x) ^"?");
    print_string  "> ";
    print_player_names (x-1) (in_player_name lst)

(** [player_loop l p] represents each round of play *)
let rec players_loop l players_to_go all_players = 
  match players_to_go with 
  | [] -> 
    begin match State.update l with
      | Some l' -> simulator_loop l' all_players
      | None -> end_game l all_players 
    end
  | h::t -> 
    print_endline ("It is " ^h ^ "'s turn.");
    print_endline proceed;
    print_string "> " ;
    begin try
        in_turn h t l players_to_go all_players
      with _ -> (print_endline invalid_also;
                 players_loop l players_to_go all_players)
    end 

(**  [simulator_loop p] runs the simulation by asking the player for a 
     command, responding to the command, and continuing to replay this loop 
     until the end command is issued. *)
and simulator_loop (l:State.league) players= 
  let current_date = 
    l 
    |> State.date 
    |> Date.date_string in
  let rounds_rem = 
    l 
    |> State.rounds 
    |> string_of_int in
  print_endline ("\nThe date of transactions for this round is " ^ 
                 current_date) ;
  print_endline ("There are "^ rounds_rem  ^ " rounds of play remaining.");
  if State.has_move_limit l 
  then players_loop_hard 1 l players players 
  else players_loop l players players

and in_turn h t l players_to_go all_players =
  match Command.parse (read_line ()) with 
  | End -> ()
  | exception Command.Empty -> empty h t l players_to_go all_players
  | exception Command.Malformed -> malformed h t l players_to_go all_players
  | Port -> port h t l players_to_go all_players 
  | Buy(obj) -> in_buy h t l players_to_go all_players obj
  | Sell(obj) -> in_sell h t l players_to_go all_players obj
  | Next ->  next h t l players_to_go all_players
  | Board -> board h t l players_to_go all_players 
  | Info -> info h t l players_to_go all_players
  | Leader -> leader h t l players_to_go all_players
  | Cash -> cash h t l players_to_go all_players
  | Liquidate -> liquid h t l players_to_go all_players

and empty h t l players_to_go all_players = 
  print_endline bad_command_message;
  players_loop l players_to_go all_players

and malformed h t l players_to_go all_players =
  print_endline "Sorry, I didn't understand that Malformed Command.";
  players_loop l players_to_go all_players

and port h t l players_to_go all_players =
  Portfolio.string_of_portfolio (State.map l) (State.date l) (State.last_date l) 
    (State.get_portfolio l h);
  players_loop l players_to_go all_players

and in_buy h t l players_to_go all_players = function
  | t'::a::r-> 
    let amount = amountify a in 
    let dt = State.date l in 
    let pt = State.get_portfolio l h in 
    let p = Portfolio.buy t' amount dt pt in
    let new_l = State.set_portfolio p h l in 
    players_loop new_l players_to_go all_players
  | _ -> failwith ("invalid input")

and in_sell h t l players_to_go all_players = function
  | t'::a::r-> 
    let amount = amountify a in 
    let dt = State.date l in 
    let pt = State.get_portfolio l h in 
    let p = Portfolio.sell t' amount dt pt in
    let new_l = State.set_portfolio p h l in 
    players_loop new_l players_to_go all_players
  | _ -> failwith ("invalid input")

and next h t l players_to_go all_players =
  players_loop l t all_players

and board h t l players_to_go all_players =
  print_endline "Here is the stock board: ";
  State.print_stocks l;
  players_loop l players_to_go all_players

and info h t l players_to_go all_players =
  print_endline command_information;
  players_loop l players_to_go all_players

and leader h t l players_to_go all_players =
  print_endline (State.scoreboard l);
  players_loop l players_to_go all_players

and cash h t l players_to_go all_players =
  let cash_string = 
    h 
    |> State.get_portfolio l 
    |> Portfolio.cash 
    |> Stock.str_value in
  print_endline ("$" ^ cash_string);
  players_loop l players_to_go all_players

and liquid h t l players_to_go all_players =
  let p = Portfolio.liquidate (State.date l) (State.get_portfolio l h) in
  players_loop (State.set_portfolio p h l) players_to_go all_players

(********* hard mode *********)
and print_moves i = "You have " ^ (string_of_int i) ^ 
                    " transactions remaining in this round."

and players_loop_hard moves l players_to_go all_players = 
  let move_limit = State.move_limit l in 
  let moves_left = move_limit - moves +1 in 
  match players_to_go with 
  | [] -> 
    begin match State.update l with 
      | Some l' -> simulator_loop l' all_players
      | None -> end_game l all_players
    end 
  | h::t -> 
    if moves > move_limit then next_hard t l all_players else (
      print_endline ("It is " ^h ^ "'s turn.");
      print_endline (print_moves moves_left);
      print_endline proceed;
      print_string "> " ;
      begin try 
          in_turn_hard moves h t l players_to_go all_players 
        with _ -> (print_endline invalid_also;
                   players_loop_hard moves l players_to_go all_players)
      end )


and in_turn_hard move_count h t l players_to_go all_players = 
  match Command.parse (read_line ()) with 
  | End -> ()
  | exception Command.Empty -> 
    empty_hard move_count h t l players_to_go all_players
  | exception Command.Malformed -> 
    malformed_hard move_count h t l players_to_go all_players
  | Port -> 
    port_hard move_count h t l players_to_go all_players 
  | Buy(obj) -> 
    in_buy_hard move_count h t l players_to_go all_players obj
  | Sell(obj) -> 
    in_sell_hard move_count h t l players_to_go all_players obj
  | Next -> next_hard t l all_players 
  | Board -> board_hard move_count l players_to_go all_players
  | Info -> info_hard move_count l players_to_go all_players
  | Leader -> leader_hard move_count l players_to_go all_players
  | Cash -> cash_hard move_count h t l players_to_go all_players
  | Liquidate -> liquid h t l players_to_go all_players

and cash_hard m h t l players_to_go all_players =
  let cash_string = 
    h 
    |> State.get_portfolio l 
    |> Portfolio.cash 
    |> Stock.str_value in
  print_endline ("$" ^ cash_string);
  players_loop_hard m l players_to_go all_players

and empty_hard m h t l players_to_go all_players = 
  print_endline bad_command_message;
  players_loop_hard m l players_to_go all_players

and malformed_hard m h t l players_to_go all_players = 
  print_endline "Sorry, I didn't understand that Malformed Command.";
  players_loop_hard m l players_to_go all_players

and port_hard m h t l players_to_go all_players = 
  let pt = State.get_portfolio l h in 
  Portfolio.string_of_portfolio (State.map l) (State.date l)
    (State.last_date l) pt;
  players_loop_hard m l players_to_go all_players

and in_buy_hard m h t l players_to_go all_players = function
  | ta:: a:: r -> 
    let amount = amountify a in 
    let dt = State.date l in 
    let pt = State.get_portfolio l h in 
    let p = Portfolio.buy ta amount dt pt in 
    let new_league = State.set_portfolio p h l in 
    players_loop_hard (m+1) new_league players_to_go all_players
  | _ -> failwith ("invalid input")

and in_sell_hard m h t l players_to_go all_players = function
  | ta:: a:: r -> 
    let amount = amountify a in 
    let dt = State.date l in 
    let pt = State.get_portfolio l h in 
    let p = Portfolio.sell ta amount dt pt in
    let new_league = State.set_portfolio p h l in 
    players_loop_hard (m+1) new_league players_to_go all_players
  | _ -> failwith ("invalid input")

and next_hard t l all_players =
  players_loop_hard 1 l t all_players

and board_hard m l players_to_go all_players = 
  print_endline "Here is the stock board: ";
  State.print_stocks l;
  players_loop_hard m l players_to_go all_players

and info_hard m l players_to_go all_players =
  print_endline command_information;
  players_loop_hard m l players_to_go all_players

and leader_hard m l players_to_go all_players = 
  print_endline (State.scoreboard l);
  players_loop_hard m l players_to_go all_players

and end_game l all_players = 
  print_endline (State.scoreboard l);
  "Thank You for playing our game, you goddess! ;p" |> print_endline

(** [main ()] prompts for the game to play, then starts it. *)
and main () =
  let stock_map = start_dialogue (); in
  let num_players = in_num_player () in 
  let player_names = print_player_names num_players [] in 
  let cash_start = in_cash () in 
  let start_date = in_start () in
  let end_date = in_end start_date in
  let pd = in_period () in 
  let mode = in_mode () in 
  let league = league_maker mode cash_start 
      pd player_names start_date end_date stock_map in 
  middle_stuff ();
  simulator_loop league player_names;
  end_dialogue ()

and start_dialogue () = 
  ANSITerminal.(print_string [on_green] intro_dialogue);
  print_endline "";
  print_endline "Loading ...";
  let stock_map1 = 
    Sys.readdir "json" 
    |> Array.to_list 
    |> List.sort (Stdlib.compare) 
    |> Stock.map_generator in 
  print_endline ""; 
  print_endline 
    "To start your simulation, please set your starting conditions.";
  stock_map1

and middle_stuff () = print_endline "It is time to begin.";
  print_endline ".....";

and end_dialogue () = 
  print_endline "\nWould you like to restart (Y) or exit (N)?. 
  type 'Y' for restart and 'N' to quit ";
  print_string "> ";
  match (read_line())
        |> clean 
        |> String.uppercase_ascii with
  | y when (y = "Y" || y = "'Y'") -> 
    main ()
  | n when (n = "N" || n = "'N'") -> 
    print_endline "\n\nThe Game is over";
    exit 0
  | _ -> print_endline invalid_input_mess; end_dialogue ()

(* Execute the game engine. *)
let () = main ()