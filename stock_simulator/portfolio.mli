(** Manages information of user's stock portfolio*)

open Stock
open Date

(** [transation] is the type used to identify different purchases. *)
type transaction = Buy | Sell

type pos_zero_neg = Pos | Neg | Zero

(** [purch] is the type used to record purchases and sales. *)
type purch

(** [share] is the type containing the stocks and quantity owned.  *)
type share

(** [portfolio] is the type for portfolios. *)
type portfolio = {
  user_id: string;
  purchases: purch list;
  stocks: share list;
  cash: float;
  value: float;
  net_value: float;
  stock_map : Yojson.Basic.t Stock.MyJson.t;
}

(**[key] is the type of the amount of a stock. *)
type key = All | Amnt of float

(** [new_port name amount map] instantiates an empty portfolio for the user 
    [name] with cash value of [amount] based on the values contained in [map].*)
val new_port : float -> string -> Yojson.Basic.t Stock.MyJson.t -> portfolio

(** [buy ticker amount date port] is [port] with [amount] of stock named 
    [ticker] bought on [date]. *)
val buy : string -> key -> date -> portfolio -> portfolio

(** [sell ticker amount date port] is [port] with [amount] of stock named 
    [ticker] sold on [date]. *)
val sell : string -> key -> date -> portfolio -> portfolio

(** [update date port] is [port] with its stocks' values updated according
    to their value on [date]. *)
val update : date -> float -> portfolio -> portfolio

(** [value port] is the total liquid valuation of [port]. *)
val value : portfolio -> float

(** [cash port] is the amount of cash available to spend in [port]. *)
val cash : portfolio -> float

(** [pos_fn curr diff_str perc_str n] outputs to the terminal information about 
    the value of [curr], [diff_str] and [perc_str] both of length [n] in green 
    representing positive changes in the values. *)
val pos_fn : Stock.t -> string -> string -> int -> unit

(** [neg_fn curr diff_str perc_str n] outputs to the terminal information about 
    the value of [curr], [diff_str] and [perc_str] both of length [n] in red 
    representing negative changes in the values. *)
val neg_fn : Stock.t -> string -> string -> int -> unit

(** [zero_fn curr diff_str perc_str n] outputs to the terminal information about 
    the value of [curr], [diff_str] and [perc_str] both of length [n] in white 
    representing no changes in the values. *)
val zero_fn : Stock.t -> string -> string -> int -> unit

(** [string_of_portfolio map curr old p] is a stringed representation of [p], 
    including information about portfolios values on [curr] and [old] based on 
    [map]. *)
val string_of_portfolio : Yojson.Basic.t Stock.MyJson.t -> date -> date -> 
  portfolio -> unit

(** [str_of_len n t] is the [t] as a string with at least [n] characters. 
    Appends spaces to the end of [t] if it is shorter than [n].*)
val str_of_len : int -> string -> string

(** [head1 t] puts [t] in header format. *)
val head1 : string -> string 

(** [liquidate date p] sells all of p's shares *)
val liquidate : date -> portfolio -> portfolio

