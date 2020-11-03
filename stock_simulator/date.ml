(******************** TYPES ********************)

type date = int * int * int  

type period = 
  | Daily of int 
  | Weekly of int 
  | Monthly of int

(************** HELPER FUNCTIONS ***************)

(** [leap_year y] is whether [y] is a leap-year based on leap-year rules. *)
let leap_year y =
  if y mod 100 = 0 then y mod 400 = 0 else y mod 4 = 0 

(** [num_days y m] is the number of days in the month [m] of year [y]. *)
let num_days y = function
  | m when m < 1 || m > 12 -> failwith "Improper month"
  | m when m = 4 || m = 6 || m = 9 || m = 11 -> 30
  | 2 -> if leap_year y then 29 else 28 
  | _ -> 31

let month_helper y new_m new_d = 
  let new_y = y + (new_m / 12) in 
  let new_m1 = new_m mod 12  in 
  let new_mo = if new_m1 = 0 then 12 else new_m1 in 
  new_y, new_mo, new_d 

(** [daily_helper y m d n] returns the next date [n] days after the day with
    year [y], month [m], day [d]. *)
let daily_helper y m d n = 
  let new_d = d + n in 
  let days = num_days y m in  
  if new_d > days then 
    let new_m = m + (new_d / days) in 
    let new_d1 = new_d mod days in 
    let new_d = if new_d1 = 0 then days else new_d1 in 
    if new_m > 12 then 
      month_helper y new_m new_d
    else y, new_m, new_d
  else y, m, new_d

(********** INTERFACE IMPLEMENTATIONS ***********)

let date_string (y, m, d) =
  let y_concat = string_of_int y in 
  let m_concat = if m < 10 then "0" ^ string_of_int m else string_of_int m in 
  let d_concat = if d < 10 then "0" ^ string_of_int d else string_of_int d in 
  y_concat ^ "-" ^ m_concat ^ "-" ^ d_concat

let is_valid_date (y, m, d) : bool = 
  try
    1 <= m && m <= 12 &&
    1 <= d && d <= num_days y m 
  with _ -> false

let date_instant y m d : date = 
  if is_valid_date (y, m, d) then (y, m, d) else 
    failwith "Invalid date"

let is_after (y1, m1, d1) (y2, m2, d2) = 
  if y1 > y2 then true 
  else if y1 = y2 && m1 > m2 then true 
  else if y1 = y2 && m1 = m2 && d1 > d2 then true 
  else false

let rec next_date ((y, m, d) : date) = function
  | Daily n when n > 0 -> daily_helper y m d n
  | Weekly n when n > 0 -> next_date (y, m, d) (Daily (n * 7))
  | Monthly n when n > 0 -> let new_m = m + n in 
    let mod_m1 = new_m mod 12 in 
    let mod_m = if mod_m1 = 0 then 12 else mod_m1 in 
    let new_y = y + (new_m - 1) / 12 in 
    if d < num_days new_y mod_m then new_y, mod_m, d
    else new_y, mod_m, num_days new_y mod_m
  | _ -> failwith "Invalid period entry"

let num_of_turns start_date end_date period = 
  let rec num_of_turns_acc start_date end_date period acc = 
    if is_after start_date end_date then acc 
    else num_of_turns_acc (next_date start_date period) 
        end_date period (acc + 1) 
  in num_of_turns_acc start_date end_date period 0

let preceding_date (y, m, d) = 
  if d != 1 then y, m, d - 1
  else if m != 1 then y, m - 1, num_days y (m - 1)
  else y - 1, 12, num_days (y - 1) 12
