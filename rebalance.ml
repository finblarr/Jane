open Core.Std
open Async.Std
open Yojson.Basic.Util

(* type containing all the information needed to trade *)
type quote = 
    {name: string; (* Apple Inc *)
     symbol: string; (* AAPL *)
     mutable last_price: float; (* 100.4 *) 
     mutable change: float; (* 0.780000000000001 *)
     mutable change_percent: float; (* 0.782975306163422 *)
     mutable timestamp: string; (* "Mon Oct 6 10:33:14 UTC-04:00 2014" *)
     mutable marketcap: int;  (* 601181846800 *)
     mutable volume: int; (* 1409539 *) 
     mutable change_ytd: float; (* 80.14563 *)
     mutable change_percent_ytd: float; (* 25.2719580593477 *)
     mutable high: float; (* 100.64 *)
     mutable low: float; (* 99.82 *)
     mutable opening: float; (* 99.6 *) 
    }

(* Generate a search URI for the dev.markitondemand api based on the given symbol *)
let query_uri (symbol: string): Uri.t =
  let base_uri = Uri.of_string "http://dev.markitondemand.com/Api/v2/Quote/json?" in 
  Uri.add_query_param base_uri ("symbol", [symbol])

(* Update stock in memory *) 
let update_stock (json: Yojson.Basic.json): quote = 
   let open Yojson.Basic.Util in 
   {name = json |> member "Name" |> to_string;
    symbol = json |> member "Symbol" |> to_string;
    last_price = json |> member "LastPrice" |> to_float;
    change = json |> member "Change" |> to_float;
    change_percent = json |> member "ChangePercent" |> to_float;
    timestamp = json |> member "Timestamp" |> to_string;
    marketcap = json |> member "MarketCap" |> to_int;
    volume = json |> member "Volume" |> to_int;
    change_ytd = json |> member "ChangeYTD" |> to_float;
    change_percent_ytd = json |> member "ChangePercentYTD" |> to_float;
    high = json |> member "High" |> to_float;
    low = json |> member "Low" |> to_float;
    opening = json |> member "Open" |> to_float;
   }


(* gets a quote for the given symbol *)
let get_quote (symbol:string): quote =
  Http_client.Convenience.http_get ("http://dev.markitondemand.com/Api/v2/Quote/json?symbol=" ^ symbol)
  |> Yojson.Basic.from_string 
  |> update_stock

    

(* finds the number of shares to buy; will eventually buy the stocks *)
let buy_amount (amount_to_alloc: float) (symbol: string): (string * float) = 
  let quote = get_quote symbol in 
  let numb_shares = amount_to_alloc /. quote.last_price in  
  (symbol, numb_shares)
  
(* will eventually be used to reconcile the trading with the database*)
let reconcile_result (symbol, numb_shares) = 
  printf "Purchased %f shares of %s." numb_shares symbol

let to_string ((symbol, numb_shares): string * float): string = 
  "{\"Symbol\":\"" ^ symbol ^ "\",\"Amount\":" ^ (Float.to_string numb_shares) ^ "}"

(* evenly allocates the amount provided among the stocks passed to the function *)

(* takes a json and converts it into a tuple  *)
let get_symbol_and_amount (json: Yojson.Basic.json): string * float = 
  let open Yojson.Basic.Util in 
  let symbol = json |> member "Symbol" |> to_string in 
  let amount = json |> member "Amount" |> to_float in 
  (symbol, amount)

(* takes the string and converts it to a list of tuples  *)
let string_to_tuple_list (portfolio_string: string): (string * float) list = 
  let json_string_list = String.split portfolio_string ~on:';' in 
  let json_list = List.map json_string_list ~f:Yojson.Basic.from_string in 
  List.map json_list ~f:get_symbol_and_amount 

(* gets the current value of the position for a given security*)
let get_value ((symbol, amount): string * float): float = 
  let quote = get_quote symbol in 
  amount *. quote.last_price

(* sums the portfolio tuples to get the current portfolio value *)              
let get_total (tuple_list: (string * float) list): float = 
  let totals = List.map tuple_list ~f:get_value in 
  List.fold_left totals ~f:(+.) ~init:0.

let rebalance (total: float) (symbols: string list) =  
  let symbols = 
    List.map symbols ~f:String.lowercase 
  in 
  let n = List.length symbols in 
  let amount_to_alloc = total /. (Float.of_int n) in
  let buy_symbol = buy_amount amount_to_alloc in 
  List.map symbols ~f:buy_symbol 

let results_to_string results = 
  let tuple_strings = List.map results ~f:to_string in 
  let tuple_string = String.concat tuple_strings ~sep:"," in 
  "{\"Portfolio\":[" ^ tuple_string ^ "]}"

let results_to_file filename results = 
  let json = Yojson.Basic.from_string (results_to_string results) in 
  Yojson.Basic.to_file filename json

let () =
  (* read the portfolio file into memory as a string and convert it into
   * a list of tuples *)
  let portfolio = Yojson.Basic.from_file "portfolio.json"
                  |> Yojson.Basic.Util.member "Portfolio" in
  let portfolio_list = (convert_each get_symbol_and_amount portfolio) in
  (* add it up to get the total *)
  let total = get_total portfolio_list in
  (* rebalance the portfolio with the new total *)
  rebalance total ["AAPL"; "MSFT"; "F";]
  |> results_to_file "portfolio.json" 
