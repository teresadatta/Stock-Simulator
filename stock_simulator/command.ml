type obj = 
  | String of string
  | Int of int

type object_phrase = string list

type command = 
  | Port
  | Buy of object_phrase 
  | Sell of object_phrase
  | End 
  | Next
  | Board
  | Info
  | Leader
  | Cash
  | Liquidate

exception Empty

exception Malformed

(** [verbs] are the possible commands in the terminal. *)
let verbs = ["portfolio"; "buy"; "sell"; "end"; "next"; "board"]

(** [listify str] changes a string into a list of words (i.e., consecutive 
    sequence of non-space characters). *)
let listify str =
  str 
  |> String.uppercase_ascii 
  |> String.split_on_char ' ' 
  |> List.filter (fun x -> not (x = ""))

let parse str =
  match (listify str) with
  | [] -> raise Empty
  | h :: c -> 
    begin match h with 
      | "BUY" -> if c = [] then raise Malformed else Buy c
      | "SELL" -> if c = [] then raise Malformed else Sell c
      | "NEXT" -> if not (c = []) then raise Malformed else Next 
      | "END" -> if not (c = []) then raise Malformed else End
      | "PORTFOLIO" -> if not (c = []) then raise Malformed else Port
      | "BOARD" -> if not (c = []) then raise Malformed else Board
      | "INFO" -> if not (c = []) then raise Malformed else Info
      | "LEADERBOARD" -> if not (c = []) then raise Malformed else Leader
      | "CASH" -> if not (c = []) then raise Malformed else Cash
      | "LIQUIDATE" -> if not (c = []) then raise Malformed else Liquidate
      | _ -> raise Malformed end


