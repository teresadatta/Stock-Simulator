open Stock
open Date

(******************** TYPES ***********************)

type transaction = Buy | Sell

type pos_zero_neg = Pos | Neg | Zero

type key = All | Amnt of float

type purch = {
  ticker: string;
  date: date;
  amount: float;
  transaction : transaction;
  num_shares : float;
  market_value : float;
}

type share = {
  stock: Stock.t;
  number: float;
}

type portfolio = {
  user_id: string;
  purchases: purch list;
  stocks: share list;
  cash: float;
  value: float;
  net_value: float;
  stock_map : Yojson.Basic.t Stock.MyJson.t;
}

(********** HELPER FUNCTIONS & VALUES ***********)

let long_line = " #%#%#%#%#%# "
let small_line = " ###### "
let head1 t = long_line ^ t ^ long_line ^ "\n\n"
let head2 t = small_line ^ t ^ small_line ^ "\n"



(** [new_purchase ticker amount date] is a purchase of an [amount] and 
    [num_shares] of the stock priced at [market_value] named [ticker] on 
    [date] and transaction type [transaction].  *)
let new_purchase ticker amount date transaction num_shares market_value = {
  ticker = ticker;
  date = date;
  amount = amount;
  transaction = transaction;
  num_shares = num_shares;
  market_value = market_value;
}

(** [new_stock number stock] is the [number] of shares of [stock].   *)
let new_stock number stock = {
  stock = stock;
  number = number;
}

(** [share_value share] is the value of [share].  *)
let share_value share = 
  (share.stock |> value) *. share.number

(** [ticker share] is the ticker name of [share].  *)
let ticker share =
  share.stock |> ticker

let get_share ticker' port =
  let rec get_in_stocks name = function
    | [] -> failwith "not possible"
    | h :: _ when ticker h = name-> h
    | h :: t -> get_in_stocks name t
  in
  get_in_stocks ticker' port.stocks

(**[add stock amount stocks] adds [amount] of [stock] to [stocks]. 
   [amount] is the amount paid *)
let add stock amount stocks=
  let number = amount /. (stock |> Stock.value) in
  let rec add1 = function
    | [] -> (stock |> new_stock number) :: []
    | h :: t when equal h.stock stock -> 
      { h with number = h.number +. number } :: t
    | h :: t -> h :: add1 t
  in add1 stocks

(**[reduce stock amount stocks] is [stocks] with [amount] of [stock] removed *)
let reduce stock amount stocks=
  let number = amount /. (stock |> Stock.value) in
  let rec sell1 = function
    | [] -> []
    | h :: t when equal h.stock stock -> 
      { h with number = h.number -. number } :: t
    | h :: t -> h :: sell1 t
  in sell1 stocks

let rec reduce_all ticker' = function
  | [] -> []
  | h :: t when ticker h = ticker' -> t
  | h :: t -> h ::(reduce_all ticker'  t)

(** [possible_buy amount port] is true if [port] has [amount] or more liquid
    cash available. False, otherwise. *)
let possible_buy amount = function
  | { cash } when cash > amount -> true
  | _ -> false

(** [possible_sell name amount stocks] is true if [amount] or more of [name] 
    is in [stocks]. False, otherwise. *)
let rec possible_sell name amount = function
  | [] -> false
  | h :: _ when ticker h = name && share_value h >= amount -> true
  | h :: t -> possible_sell name amount t

let rec str_of_len n t = 
  if String.length t < n then str_of_len n (t ^ " ") else t

(** [string_of_portfolio purch] is a stringed representation of [purch]. *)
let string_of_purch purch = 
  let str_trans p = match p.transaction with 
    | Buy -> "Buy"
    | Sell -> "Sell" in
  "\t" ^ (purch.date |> date_string)  ^ 
  "\t" ^ (purch |> str_trans) ^ 
  "\t" ^ purch.ticker ^ 
  "\t" ^ (purch.num_shares |> Stock.str_value |> str_of_len 10) ^
  "\t" ^ (purch.market_value |> Stock.str_value |> str_of_len 10) ^
  "\t" ^ (purch.amount |> Stock.str_value) ^ "\n"

let first_part p port_value cash_value = 
  ANSITerminal.(print_string [yellow] (head1 p.user_id);
                print_string [white] ("The total value of your portfolio is: $" 
                                      ^ port_value ^ "\n" ^ 
                                      "The total cash you have to spend is: $" 
                                      ^ cash_value ^ "\n" ^
                                      "The net change in your portfolio is: "))
let second_part p = 
  ANSITerminal.(print_string [white] 
                  ("\nTransaction log: \n \tDate      \t" ^
                   "Type\tSymbol\tShares     \tMrktVal     \t$ Amount\n");
                print_string [white] ((p.purchases 
                                       |> List.map string_of_purch 
                                       |> List.fold_left (^) "") ^ "\n"))


(********** INTERFACE IMPLEMENTATIONS ***********)

let new_port amount name map = {
  user_id = name;
  purchases = [];
  stocks = [];
  cash = amount;
  value = amount;
  net_value = 0.; 
  stock_map = map;
}

let sell_all ticker date port= 
  try
    let share = get_share ticker port in
    let amount = share_value share in
    let value = share.stock |> value in
    let num_purchased = share.number in
    let sell_all = function
      |{ purchases; stocks; cash } as p -> 
        { p with purchases = (new_purchase ticker (-. amount) date 
                                Sell num_purchased value) :: purchases;
                 cash = cash +. amount;
                 stocks = reduce_all ticker stocks
        }
    in sell_all port
  with _ -> failwith "not possible"

let buy_all ticker date port = 
  let stock = instantiate ticker date port.stock_map in
  let amount = port.cash in
  let value = Stock.value stock in
  let num_purchased = amount /. value in
  let buy_all = function
    | { purchases; stocks; cash } as p when amount > 0. -> 
      { p with purchases = (new_purchase ticker amount date 
                              Buy num_purchased value) :: purchases;
               cash = cash -. amount;
               stocks = add stock amount stocks
      }
    | _ -> failwith "not possible"
  in buy_all port

let buy' ticker amount date port = 
  let stock = instantiate ticker date port.stock_map in
  let value = Stock.value stock in
  let num_purchased = amount /. value in
  let buy = function
    | { purchases; stocks; cash } as p when possible_buy amount p -> 
      { p with purchases = (new_purchase ticker amount date 
                              Buy num_purchased value) :: purchases;
               cash = cash -. amount;
               stocks = add stock amount stocks
      }
    | _ -> buy_all ticker date port
  in buy port

let sell' ticker amount date port= 
  let stock = instantiate ticker date port.stock_map in
  let value = Stock.value stock in
  let num_purchased = amount /. value in
  let sell = function
    | { purchases; stocks; cash } as p when 
        possible_sell ticker amount stocks -> 
      { p with purchases = (new_purchase ticker (-. amount) date 
                              Sell num_purchased value) :: purchases;
               cash = cash +. amount;
               stocks = reduce stock amount stocks
      }
    | _ -> failwith "not possible"
  in sell port

let buy ticker amount date port =
  match amount with
  |All -> buy_all ticker date port
  |Amnt a -> buy' ticker a date port

let sell ticker amount date port =
  match amount with
  |All -> sell_all ticker date port
  |Amnt a -> sell' ticker a date port

let value port =
  (port.stocks 
   |> List.map share_value 
   |> List.fold_left (+.) 0.) 
  +. port.cash

let cash port = 
  port.cash

let update date budg port =
  let update = function
    | { stock; number } as p -> 
      { p with stock = Stock.update stock date port.stock_map }
  in
  let p' = { port with stocks = List.map update port.stocks; } in
  let v' = value p' in
  { p' with value = v'; net_value = v' -. budg }

let pos_fn curr diff_str perc_str n = 
  ANSITerminal.(print_string [green] ("\u{25b2}" ^ "\t");
                (print_string [white] 
                   (Stock.value curr |> Stock.str_value |> str_of_len n));
                (print_string [green] ("\t" ^ str_of_len n diff_str ^ "\t" ^ 
                                       str_of_len n perc_str ^ "\t")))

let neg_fn curr diff_str perc_str n =  
  ANSITerminal.(print_string [red] ("\u{25bc}" ^ "\t");
                (print_string [white] 
                   (Stock.value curr |> Stock.str_value |> str_of_len n ));
                (print_string [red] ("\t" ^ str_of_len n diff_str ^ "\t" 
                                     ^ str_of_len n perc_str ^ "\t")))

let zero_fn curr diff_str perc_str n =
  ANSITerminal.(print_string [white] ("\u{25ba}" ^ "\t");
                (print_string [white] 
                   (Stock.value curr |> Stock.str_value |> str_of_len n);
                 (print_string [white] ("\t" ^ str_of_len n diff_str ^ "\t" ^ 
                                        str_of_len n perc_str ^ "\t"))))

(** [string_of_portfolio s] is a stringed representation of [s]. *)
let rec string_of_share map current_date past_date s = 
  let past = Stock.instantiate (s.stock |> Stock.ticker) past_date map in 
  let curr = Stock.instantiate (s.stock |> Stock.ticker) current_date map in 
  let diff = Stock.value curr -. Stock.value past in 
  let perc = diff *. 100. /. Stock.value past in 
  let pos_zero_int = if diff > 0. then Pos 
    else if diff < 0. then Neg else Zero in
  let diff_str = if diff >= 0. then "+" ^ Stock.str_value diff 
    else Stock.str_value diff in 
  let perc_str = if perc >= 0. then "+" ^ Stock.str_value perc ^ "%"
    else Stock.str_value perc ^ "%" in 
  ANSITerminal.(print_string [white] 
                  ("\t" ^ (s.stock |> Stock.ticker |> str_of_len 4) ^
                   "\t" ^ (s.number |> Stock.str_value |> str_of_len 10)));
  match_pos_zero_int curr diff_str perc_str pos_zero_int 10;
  ANSITerminal.(print_string [white] ((s |> share_value |> Stock.str_value 
                                       |> str_of_len 10) ^ "\n"))

and match_pos_zero_int curr diff_str perc_str = function 
  | Pos -> pos_fn curr diff_str perc_str
  | Neg -> neg_fn curr diff_str perc_str
  | Zero -> zero_fn curr diff_str perc_str


let string_of_portfolio map current_date old_date p = 
  let port_value = p.value |> Stock.str_value in 
  let cash_value = p.cash |> Stock.str_value in 
  let net_change = if (p.net_value >= 0.) 
    then "+$" ^ (p.net_value |> Stock.str_value) 
    else "-$" ^ (p.net_value |> Float.neg |> Stock.str_value) in 
  first_part p port_value cash_value; 
  if p.net_value >= 0. 
  then ANSITerminal.(print_string [green] ("\u{25b2} " ^ net_change ^ "\n\n"))
  else ANSITerminal.(print_string [red] ("\u{25bc} " ^ net_change ^ "\n\n"));
  ANSITerminal.(print_string [white] ("Stocks held: \n\tSymbol" ^
                                      "\tShares    \tMrktVal   \t$Change   " ^
                                      "\t%Change   \tTotalVal\n"));
  (p.stocks |> List.iter (string_of_share map current_date old_date));
  second_part p

let liquidate date p =
  let shares_tick = p.stocks |> List.map ticker in
  let rec liquidate acc = function
    |h :: t -> liquidate  (sell_all h date p) t
    |[] -> acc in
  liquidate p shares_tick

