(** This module holds information on the stocks and stock maps present.*)

open Yojson.Basic.Util
open Date

(** [t] is the type of the stock. *)
type t = {
  ticker : string;
  value : float;
}

(** [MyJson] is a module representing a map of stocks.*)
module MyJson : Map.S with type key := string

(** [instantiate ticker date map] creates a stock with symbol [ticker] with the 
    value on [date] based on its corresponding json in [map]. *) 
val instantiate : string -> date -> Yojson.Basic.t MyJson.t -> t

(** [update st date] is an updated stock of [st] with the value on [date] based 
    based on its corresponding json in [map]. *)
val update : t -> date -> Yojson.Basic.t MyJson.t -> t

(** [value st] is the current market value of [st]. *)
val value : t -> float

(** [ticker t] is a the ticker name of the stock *)
val ticker : t -> string

(** [equal t1 t2] is whether the tickers of [t1] and [t2] are the same. *)
val equal : t -> t -> bool

(** [str_value v] returns the stringed version of [v] according to conventional
    two decimal currency displays. *)
val str_value : float -> string 

(** [map_generator s] returns a map of the stock names in [s] to their respective
    json data structures. *)
val map_generator : string list -> Yojson.Basic.t MyJson.t