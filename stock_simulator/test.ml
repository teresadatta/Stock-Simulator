(** Test Plan: 
    The following modules: Command, Date, Portfolio, State, Stock were 
    automatically tested by OUnit. However, Portfolio, State, and Stock were also
    manually tested in both the Terminal Simulator and in Utop.
    The simulator as accessed by the user via Main was manually tested in 
    the Terminal interface.
    The test cases were developed via Black Box testing methods, meaning that the
    listed test cases check for accuracy according to the 
    publicly found specifications in the .mli files. However, for methods in which 
    test cases failed, in order to ensure accuracy, glass box testing was also 
    utilized in order to ensure the functionality of the written code. 

    This testing plan demonstrates the correctness of our system to the best of our
    abilites. By first ensuring the accuracy of our helper functions via 
    OUnit testing, we gain the first step to system confidence. However, this has 
    been assured by repeated manual tests and utilizations of our simulator via 
    "make use". We repeatedly tried to break our system in order to find faults, 
    and we played as many rounds as reasonable in order to utilize 
    as many combinations of commands as reasonably possible. Unfortunately, because 
    of the design of our system, there are practically infinite combinations that 
    can constitute a game, so although it is possible that there is some error that
    we did not account for, we have made every effort to make this have the 
    least probability of occurring.  *)


open OUnit2
open Command
open Date
open Portfolio
open State
open Stock 

(********************************************************************
   Tests for Command 
 ********************************************************************)

let listify_test 
    (name: string)
    (input: string)
    (expected_output: string list) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Command.listify input)
    )

let parse_test 
    (name : string) 
    (input: string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse input))

let p = Port
let buy_s = Command.Buy (["FB"])
let sell_s = Command.Sell(["THIS"])
let buy_c = Command.Buy (["CORNELL"])
let sell_c = Command.Sell (["CORNELL"])
let n = Next
let b = Board
let e = End


let command_tests =
  [ 
    parse_test "port" "portfolio" p;
    parse_test "    port " "     portfolio " p;
    parse_test "     buy FB " "      buy  FB " buy_s;
    parse_test "     sell this  one " "       sell this   " sell_s;
    parse_test "       buy fb" "      buy fb" buy_s;
    parse_test "buy fb" "buy fb" buy_s;
    parse_test "sell this" " SELl tHIs " sell_s;
    parse_test "corn buy" "buy cornell  " buy_c;
    parse_test "corn BUY" "BUY CORNELL" buy_c;
    parse_test "sell corn" "sell CORNELL" sell_c;
    parse_test "sell this one" "sell this" sell_s;
    parse_test "next" "next" n;
    parse_test "   next  " "   NEXt " n;
    parse_test "   BoArd  " "  BoArd " b;
    parse_test "end " "end" e;
    "empty"    >:: (fun _ -> assert_raises (Empty) (fun () -> parse "  "));
    "empty"    >:: (fun _ -> assert_raises (Empty) (fun () -> parse ""));
    "malformed"  >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse " buy "));
    "malformed"  >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse " corn "));
    "malformed"  >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse " s234987"));
    "malformed"  >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse " 12234987"));
    "malformed"  >:: 
    (fun _ -> assert_raises (Malformed) (fun () -> parse " end this "));

    listify_test "hi " "hi " ["HI"];
    listify_test "let's try this again" "let's try this again" 
      ["LET'S"; "TRY"; "THIS"; "AGAIN"];
    listify_test "one" "one" ["ONE"];
    listify_test "alsdkjfl 200" "alsdkjfl 200" ["ALSDKJFL"; "200"];
    listify_test "alsdkjfl1 200" "alsdkjfl1 200" ["ALSDKJFL1"; "200"];
    listify_test "           one" "         one" ["ONE"];
  ]

(********************************************************************
   Tests for Date
 ********************************************************************)

let date_string_test 
    (name: string)
    (input: date)
    (expected_output: string) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Date.date_string input)
    )

let is_valid_date_test 
    (name: string)
    (input: date)
    (expected_output: bool) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Date.is_valid_date input)
    )

let date_instant_test 
    (name: string)
    (input_year: int)
    (input_month: int)
    (input_day: int)
    (expected_output: date) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Date.date_instant input_year input_month input_day)
    )

let is_after_test
    (name: string)
    (input_1: date)
    (input_2: date)
    (expected_output: bool) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Date.is_after input_1 input_2)
    )

let next_date_test 
    (name: string)
    (input_1: date)
    (input_2: Date.period)
    (expected_output: date) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Date.next_date input_1 input_2)
    )

let num_turns_test 
    (name: string)
    (input_1: date)
    (input_2: date)
    (input_3: Date.period)
    (expected_output: int) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Date.num_of_turns input_1 input_2 input_3)
    )

let preceding_date 
    (name: string)
    (input_1: date)
    (expected_output: date) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Date.preceding_date input_1 )
    )

let d3 = Daily(3)
let d1 = Daily(1)
let w2 = Weekly(2)
let m1 = Monthly(1)

let date_tests = 
  [ preceding_date "pd1" (2020, 7 , 8) (2020, 7 , 7);
    preceding_date "pd2" (2020, 2 , 30) (2020, 2 , 29);
    preceding_date "pd2" (2000, 1 , 1) (1999, 12 , 31);
    preceding_date "pd3" (2019, 11, 1) (2019, 10, 31);
    preceding_date "pd4" (2019, 10, 1) (2019, 9, 30);
    preceding_date "pd5" (2019, 9, 1) (2019, 8, 31);
    preceding_date "pd6" (2019, 8, 1) (2019, 7, 31);
    preceding_date "pd7" (2019, 1, 1) (2018, 12, 31);
    preceding_date "pd8" (2019, 8, 8) (2019, 8, 7);
    preceding_date "pd9" (2019, 3, 1) (2019, 2, 28);
    preceding_date "pd10" (2018, 3, 1) (2018, 2, 28);
    preceding_date "pd11" (2020, 3, 1) (2020, 2, 29);
    preceding_date "pd12" (2015, 3, 1) (2015, 2, 28);
    preceding_date "pd13" (2015, 3, 14) (2015, 3, 13);
    preceding_date "pd14" (1988, 3, 1) (1988, 2, 29);
    preceding_date "pd15" (1988, 3, 5) (1988, 3, 4);
    preceding_date "pd16" (1988, 3, 16) (1988, 3, 15);


    num_turns_test "nt1" (2000, 1, 1) (2000, 1, 14) d1 14;
    num_turns_test "nt2" (2000, 1, 1) (2001, 1, 1) m1 13; 
    num_turns_test "nt1tf" (2000, 12, 1) (2000, 12, 1) m1 1;
    num_turns_test "nt1te" (2000, 10, 1) (2000, 11, 1) m1 2;
    num_turns_test "nt1tg" (2000, 11, 1) (2000, 12, 1) m1 2;
    num_turns_test "nt3" (2000, 1, 1) (2000, 1, 14) w2 1;
    num_turns_test "nt4" (2000, 1, 1) (2001, 1, 1) w2 27; 
    num_turns_test "nt5" (2000, 1, 1) (2000, 1, 14) d3 5;
    num_turns_test "nt6" (2000, 1, 1) (2001, 1, 1) d3 123; 
    num_turns_test "nt6" (2000, 1, 1) (2001, 1, 1) d1 367;
    num_turns_test "nt1tf" (2000, 12, 1) (2000, 12, 1) d1 1;
    num_turns_test "nt1te" (2000, 10, 1) (2000, 11, 1) d1 32;
    num_turns_test "nt1tg" (2000, 11, 1) (2000, 12, 1) d1 31;
    num_turns_test "nt1tf" (2000, 12, 1) (2000, 12, 1) d3 1;
    num_turns_test "nt1te" (2000, 10, 1) (2000, 11, 1) d3 11;
    num_turns_test "nt1tg" (2000, 11, 1) (2000, 12, 1) d3 11;
    num_turns_test "nt1tf" (2000, 12, 1) (2000, 12, 1) w2 1;
    num_turns_test "nt1te" (2000, 10, 1) (2000, 11, 1) w2 3;
    num_turns_test "nt1tg" (2000, 11, 1) (2000, 12, 1) w2 3;


    next_date_test "december" (2000, 11, 1) m1 (2000, 12, 1);
    next_date_test "nt3" (2000, 4, 1) m1 (2000, 5, 1);
    next_date_test "nd1" (2000, 1, 1) m1 (2000, 2, 1);
    next_date_test "nd2" (2000, 12, 31) m1 (2001, 1, 31);
    next_date_test "crazy" (2000, 11, 5) (Monthly (13)) (2001, 12, 5);
    next_date_test "nd2" (1999, 12, 31) (Monthly (2)) (2000, 2, 29);
    next_date_test "nd3" (2005, 3, 3) d1 (2005, 3, 4);
    next_date_test "nd3a" (2005, 3, 31) d1 (2005, 4,1);
    next_date_test "nd3c" (2005, 3, 31) d3 (2005, 4,3);
    next_date_test "nd3b" (2005, 3, 3) d3 (2005, 3, 6);
    next_date_test "nd4" (2005, 2, 23) w2 (2005, 3, 9);
    next_date_test "nd5" (2005, 12, 20) w2 (2006, 1, 3);

    is_after_test "a1" (1999, 1, 1) (1999, 1, 1) false;
    is_after_test "a2" (1999, 1, 1) (1999, 1, 2) false;
    is_after_test "a3" (1999, 1, 31) (1999, 1, 2) true;
    is_after_test "a4" (2000, 1, 1) (1999, 1, 2) true;
    is_after_test "a2" (1999, 1, 2) (1999, 1, 1)  true;
    is_after_test "a3" (1999, 1, 2) (1999, 1, 3) false;
    is_after_test "a4" (1998, 1, 4) (1999, 1, 2) false;

    date_instant_test "i1" 2938 3 7 (2938, 3, 7);
    date_instant_test "i2" 2005 3 31 (2005, 3, 31);
    date_instant_test "i3" 1929 3 7 (1929, 3, 7);
    date_instant_test "i4" 1919 3 17 (1919, 3, 17);
    date_instant_test "i5" 29385 3 7 (29385, 3, 7);
    date_instant_test "i6" 2938 12 17 (2938, 12, 17);

    is_valid_date_test "v1" (200, 12, 12) true;
    is_valid_date_test "v2" (12908, 1, 394857) false;
    is_valid_date_test "v3" (2000, 2, 29 ) true;
    is_valid_date_test "v3" (2000, 2, 2 ) true;
    is_valid_date_test "v3" (2000, 10, 7 ) true;
    is_valid_date_test "v3" (2000, 10, 13 ) true;
    is_valid_date_test "v3b" (2100, 2, 29 ) false;
    is_valid_date_test "v3b" (2100, 2, 239487 ) false;
    is_valid_date_test "v3b" (2100, 2, 2239487 ) false;
    is_valid_date_test "v3b" (2100, 0, 12 ) false;
    is_valid_date_test "v3b" (2100, 2, 0 ) false;
    is_valid_date_test "v3b" (2100, 2239847, 2239487 ) false;
    is_valid_date_test "v4" (2000, 2, 28) true;

    date_string_test "ds1" (2000, 1, 1)"2000-01-01";
    date_string_test "ds2" (1999, 10,10) "1999-10-10";
    date_string_test "ds3" (1999, 7,11) "1999-07-11";
    date_string_test "ds3" (1999, 11,7) "1999-11-07";
    date_string_test "ds3" (1999, 7,31) "1999-07-31";
  ]

(********************************************************************
   Tests for Portfolio, also tested in Utop/Terminal
 ********************************************************************)

let new_port_test 
    (name: string)
    (input: float)
    (input2: string)
    (input3: Yojson.Basic.t Stock.MyJson.t)
    (expected_output: portfolio) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Portfolio.new_port input input2 input3)
    )

let buy_test
    (name: string)
    (input: string)
    (input2: Portfolio.key)
    (input3: date)
    (input4: portfolio)
    (expected_output: portfolio) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Portfolio.buy input input2 input3 input4)
    )

let sell_test
    (name: string)
    (input: string)
    (input2: Portfolio.key)
    (input3: date)
    (input4: portfolio)
    (expected_output: portfolio) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Portfolio.sell input input2 input3 input4)
    )

let update_test 
    (name: string)
    (input: date)
    (input2: float)
    (input4: portfolio)
    (expected_output: portfolio) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Portfolio.update input input2 input4)
    )

let port_value_test
    (name: string)
    (input: portfolio)
    (expected_output: float) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Portfolio.value input)
    )

let port_cash_test
    (name: string)
    (input: portfolio)
    (expected_output: float) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Portfolio.cash input)
    )

let p_empty: Portfolio.portfolio = {
  user_id = "empty";
  purchases = [];
  stocks = [];
  cash = 1000.;
  value = 1000.;
  net_value = 0.;
  stock_map = MyJson.empty;
}

let p_pecan: Portfolio.portfolio = {
  user_id = "pecan";
  purchases = [];
  stocks = [];
  cash = 1111.;
  value = 1111.;
  net_value = 0.;
  stock_map = MyJson.empty;
}

let p_pie: Portfolio.portfolio = {
  user_id = "PIE";
  purchases = [];
  stocks = [];
  cash = 4200.;
  value = 4200.;
  net_value = 0.;
  stock_map = MyJson.empty;
}

let p_pumpkin: Portfolio.portfolio = {
  user_id = "Pumpkin";
  purchases = [];
  stocks = [];
  cash = 1234.;
  value = 1234.;
  net_value = 0.;
  stock_map = MyJson.empty;
}

let portfolio_tests = 
  [
    new_port_test "empty" 1000. "empty" MyJson.empty p_empty;
    new_port_test "pecan" 1111. "pecan" MyJson.empty p_pecan;
    new_port_test "pumpkin" 1234. "Pumpkin" MyJson.empty p_pumpkin;
    new_port_test "pie" 4200. "PIE" MyJson.empty p_pie;
    port_value_test "empty val" p_empty 1000.;
    port_value_test "pecan val" p_pecan 1111.;
    port_cash_test "empty cash" p_empty 1000.;
    port_cash_test "pecan cash" p_pecan 1111.;
    port_value_test "pie val" p_pie 4200.;
    port_value_test "pumpkin val" p_pumpkin 1234.;
    port_cash_test "pie cash" p_pie 4200.;
    port_cash_test "pumpkin cash" p_pumpkin 1234.;

    update_test "empty update" (2000, 1, 1) 1000. p_empty p_empty;
    update_test "pecan update" (2000, 10, 7) 1111. p_pecan p_pecan;
    update_test "pecan update" (2000, 1, 7) 1111. p_pecan p_pecan;
    update_test "pecan update" (2000, 12, 7) 1111. p_pecan p_pecan;

  ]

(********************************************************************
   Tests for State, also tested in Utop/Terminal
 ********************************************************************)


let date_test
    (name: string)
    (input: league)
    (expected_output: date) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (State.date input)
    )

let rounds_test 
    (name: string)
    (input: league)
    (expected_output: int) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (State.rounds input)
    )

let get_portfolio_test
    (name: string)
    (input: league)
    (input2: string)
    (expected_output: portfolio) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (State.get_portfolio input input2)
    )

let start_league = State.new_league 1000. (Daily(1)) ["T";"J";"O"] 
    (2000,1,1) (2000, 1, 31) MyJson.empty
let start_league1 = State.new_league 1000. (Weekly(1)) ["T";"J";"O"] 
    (2010,10,31) (2011, 1, 31) MyJson.empty
let start_league2 = State.new_league 1000. (Monthly(1)) ["T";"J";"O"] 
    (2000,1,1) (2000, 2, 31) MyJson.empty
let start_league3 = State.new_league 111111. (Monthly(1)) ["T";"J";"O"] 
    (2000,1,1) (2000, 2, 31) MyJson.empty

let p_T: Portfolio.portfolio = {
  user_id = "T";
  purchases = [];
  stocks = [];
  cash = 1000.;
  value = 1000.;
  net_value = 0.;
  stock_map = MyJson.empty;
}

let p_J: Portfolio.portfolio = {
  user_id = "J";
  purchases = [];
  stocks = [];
  cash = 1000.;
  value = 1000.;
  net_value = 0.;
  stock_map = MyJson.empty;
}

let p_O: Portfolio.portfolio = {
  user_id = "O";
  purchases = [];
  stocks = [];
  cash = 111111.;
  value = 111111.;
  net_value = 0.;
  stock_map = MyJson.empty;
}

let p_OT: Portfolio.portfolio = {
  user_id = "T";
  purchases = [];
  stocks = [];
  cash = 111111.;
  value = 111111.;
  net_value = 0.;
  stock_map = MyJson.empty;
}


let state_tests = [
  date_test "dt1" start_league (2000,1,1);
  date_test "dt2" start_league1 (2010,10,31);
  date_test "dt3" start_league2 (2000, 1, 1);
  rounds_test "rt1" start_league 31;
  rounds_test "rt2" start_league1 14;
  rounds_test "rt3" start_league2 2;
  get_portfolio_test "p1" start_league "T" p_T;
  get_portfolio_test "p2" start_league "J" p_J;
  get_portfolio_test "p3" start_league3 "O" p_O;
  get_portfolio_test "p4" start_league3 "T" p_OT;
]

(********************************************************************
   Tests for Stock, also tested in Utop/Terminal
 ********************************************************************)


let instantiate_test 
    (name: string)
    (input: string)
    (input2: date)
    (input3: Yojson.Basic.t Stock.MyJson.t)
    (expected_output: Stock.t) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Stock.instantiate input input2 input3)
    )
let update_stock_test
    (name: string)
    (input: Stock.t)
    (input2: date)
    (input3: Yojson.Basic.t Stock.MyJson.t)
    (expected_output: Stock.t) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Stock.update input input2 input3)
    )

let stock_value_test
    (name: string)
    (input: Stock.t)
    (expected_output: float) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Stock.value input)
    )
let stock_ticker_test
    (name: string)
    (input: Stock.t)
    (expected_output: string) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Stock.ticker input )
    )
let stock_equal_test
    (name: string)
    (input: Stock.t)
    (input2: Stock.t)
    (expected_output: bool) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Stock.equal input input2)
    )
let str_value_test
    (name: string)
    (input: float)
    (expected_output: string) : test =
  name >:: ( fun _ ->
      assert_equal expected_output
        (Stock.str_value input)
    )

let map_equal_test
    (name : string)
    (input : string list)
    (expected_output : Yojson.Basic.t Stock.MyJson.t) : test =
  name >:: (fun _ ->
      assert_equal expected_output (Stock.map_generator input))


let empty_map = MyJson.empty
let s_map = MyJson.add "MCD" 0.239487
let mcd = "MCD"
let mcd_lc = "mcd"

let nke = ["NKE.json"]
let nke_sbux = ["NKE.json";"SBUX.json";]

let nke_json = Yojson.Basic.from_file ("json" ^ Filename.dir_sep ^ "NKE.json")
let sbux_json = Yojson.Basic.from_file ("json" ^ Filename.dir_sep ^ "SBUX.json")

let nke_map = MyJson.empty |> MyJson.add "NKE" nke_json 
let nke_amzn_map = 
  MyJson.empty 
  |> MyJson.add "NKE" nke_json 
  |> MyJson.add "SBUX" sbux_json

let mcd_stock = {
  ticker = mcd;
  value = 0.239487;
}

let mcdl_stock = {
  ticker = mcd_lc;
  value = 100.;
}

let other_mcd = {
  ticker = "MCD";
  value = 314.;
}


let stock_tests = [
  str_value_test "sv1" 0.0567 "0.06";
  str_value_test "sv2" 100. "100.00";
  str_value_test "sv3" 5.67 "5.67";
  str_value_test "sv4" 05.67 "5.67";
  str_value_test "sv5" 3.1 "3.10";
  str_value_test "sv6" 0.4 "0.40";
  str_value_test "sv7" 0.05 "0.05";
  stock_value_test "sv8" mcd_stock 0.239487;
  stock_ticker_test "sv9" mcd_stock "MCD";
  stock_value_test "sv10" mcdl_stock 100.;
  stock_ticker_test "sv11" mcdl_stock "mcd";
  stock_value_test "sv12" other_mcd 314.;
  stock_ticker_test "sv13" other_mcd "MCD";
  stock_equal_test "se1" mcd_stock other_mcd true;
  stock_equal_test "se2" other_mcd mcd_stock true;
  map_equal_test "nike map" nke nke_map;
  map_equal_test "nike sbux map" nke_sbux nke_amzn_map;
]

(********************************************************************)

let suite =
  "test suite for final project"  >::: List.flatten [
    command_tests;
    date_tests;
    portfolio_tests;
    state_tests;
    stock_tests
  ]

let _ = run_test_tt_main suite