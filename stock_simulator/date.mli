(** Handles information related to time and dates. *)

(** [date] is the implementation of a date: (y, m, d) where [y] is the year, 
    [m] is the month, and [d] is the date. *)
type date = int * int * int  

(** [period] is how often the game updates stock values:
    includes [Daily n] for every n days, [Weekly n] for every n weeks, 
    [Monthly n] for every n months. *)
type period = 
  | Daily of int 
  | Weekly of int 
  | Monthly of int

(** [date_string date] converts [date] into a stringed date. 
    I.e. (2019, 3, 4) becomes "2019-03-04". *)
val date_string : date -> string

(** [is_valid_date] is whether the [date] is valid. *)
val is_valid_date : date -> bool 

(** [date_instant y m d] instantiates a date of year [y], month [m], day [d].*)
val date_instant : int -> int -> int -> date 

(** [is_after d1 d2] is whether [d1] comes after [d2]. *)
val is_after : date -> date -> bool 

(** [next_date d p] returns the date after [d] steps [p]. Follows all leap-year
    and calendar rules. *)
val next_date : date -> period -> date

(** [num_of_turns start end period] is the number of turns between [start] and 
    [end] with [period] as the step. *)
val num_of_turns : date -> date -> period -> int

(** [preceding_date d] returns the immediately preceding date of [d] according 
    to calendar rules. *)
val preceding_date : date -> date
