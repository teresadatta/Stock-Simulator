(** This module represents the league's state of play.  *)
open Portfolio
open Stock
open Date
open Sys

(** [user_id] is the type of a user identifier. *)
type user_id

(** [league] is the type representing a league of users. *)
type league 

(** [new_league budget period players start end map] generates a new league with
    [budget], [period], [players], [start] as start date, [end] as end date, and 
    [map] as the string-json data structure as their respective attributes. In 
    this league, each player can make an unlimited number of moves during each
    round of play. *)
val new_league : float -> period -> string list -> date -> 
  date -> Yojson.Basic.t Stock.MyJson.t -> league

(** [new_tl_league budget period players start end map move_limit] generates 
    a new league with [budget], [period], [players], [start] as start date, 
    [end] as end date, and 
    [map] as the string-json data structure as their respective attributes. In 
    this league, each player can [move_limit] moves during each
    round of play. *)
val new_tl_league : float -> period -> string list -> date -> 
  date -> Yojson.Basic.t Stock.MyJson.t -> int -> league

(** [date l] returns the current date of [l]. *)
val date : league -> date

(** [last_date l] returns the last_date contained in [l]. *)
val last_date : league -> date

(** [map l] returns the stock_map contained in [l]. *)
val map : league -> Yojson.Basic.t MyJson.t

(** [has_move_limit l] returns whether there is a limit on the number of moves
    a player can make each round. If false, then players can make an unlimited 
    number of moves per round.  *)
val has_move_limit: league -> bool

(**[move_limit l]  returns the number of moves a player can make each round. 
   If 0, then the player can make an unlimited number of moves.*)
val move_limit: league -> int

(** [rounds l] returns the amount of turns remaining in [l]. *)
val rounds : league -> int

(** [get_portfolio l u] returns [u]'s portfolio in [l]. *)
val get_portfolio : league -> string -> portfolio

(** [update l] returns [Some m] where [m] is an updated league or [None]. *)
val update : league -> league option

(** [set_portfolio p u l] updates [u]'s portfolio in [l] to [p]. *)
val set_portfolio : portfolio -> string -> league -> league

(** [print_stock l] prints the stock values and dollar amount/percent changse 
    in values of all stocks contained in the json folder based on the current 
    date and last turn's date in [l]. *)
val print_stocks : league -> unit

(** [scoreboard l] is the string representation of t.*)
val scoreboard : league -> string