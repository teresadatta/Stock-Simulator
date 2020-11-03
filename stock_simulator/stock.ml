open Yojson.Basic.Util
open Date

(******************** TYPES *********************)

type t  = {
  ticker : string;
  value : float;
}

module MyJson = Map.Make(String);;

(************** HELPER FUNCTIONS ***************)

(** [get_value j date loops] given [j] and following the format of Alpha 
    Vantage APIs, returns the value on [date]. If [loops] is more than 7, 
    than it fails - the stock does not have a value for that date. *)
let rec get_value j date loops = 
  if loops > 7 then failwith("It doesn't exist!") else
    try 
      let date_s = date_string date in 
      let value = j 
                  |> member "Time Series (Daily)" 
                  |> member date_s 
                  |> member "4. close" 
                  |> to_string 
                  |> float_of_string 
      in value
    with _ -> get_value j (preceding_date date) (loops + 1)

(********** INTERFACE IMPLEMENTATIONS ***********)

let instantiate (ticker : string)(date : date)(map : Yojson.Basic.t MyJson.t) =  
  try
    if is_valid_date date 
    then
      let u_ticker = String.uppercase_ascii ticker in 
      let j = MyJson.find ticker map in  
      {
        ticker  = u_ticker;
        value = get_value j date 0
      }
    else failwith "Invalid date"
  with _ -> raise (Failure "Error with ticker input or date")


let update (t : t) (date : date) (map : Yojson.Basic.t MyJson.t) = 
  if is_valid_date date then 
    let j = MyJson.find t.ticker map in 
    {
      ticker  = t.ticker;
      value = get_value j date 0
    }
  else failwith "Invalid date"

let value (t : t) = 
  t.value

let ticker (t : t) =
  t.ticker

let equal (t1 : t) (t2 : t) = 
  t1.ticker = t2.ticker

let str_value (v : float) = 
  let after_index str i = String.sub str i (String.length str - i) in
  let new_v = 
    v *. 100. 
    |> Float.round 
    |> Float.trunc in 
  let str = string_of_float (new_v /. 100.) in 
  let after_decimal = after_index str (String.index str '.') in 
  match String.length after_decimal with 
  | 1 -> str ^ "00"
  | 2 -> str ^ "0"
  | _ -> str

let map_generator (jsons : string list) = 
  let map = MyJson.empty in 
  let period_index str = String.index str '.' in
  let json_help = fun str -> String.sub str 0 (period_index str) in 
  let no_json = List.map json_help jsons in
  let stockify s= "json" ^ Filename.dir_sep ^ s ^ ".json" in 
  let fold_help = fun init stock -> MyJson.add stock 
      (Yojson.Basic.from_file (stockify stock )) init in
  List.fold_left fold_help map no_json