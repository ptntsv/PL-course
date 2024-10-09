val months = [ "January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November",
"December"];

val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

fun is_older(d1: int*int*int, d2: int*int*int) = 
  if #1 d1 < #1 d2 then true
  else if #1 d1 = #1 d2
  then
    if #2 d1 < #2 d2 then true
    else if #2 d1 = #2 d2
    then
      if #3 d1 < #3 d2 then true
      else false
    else false
  else false

fun number_in_month(xs: (int*int*int) list, m: int) = 
  if null xs
  then 0
  else
    (if #2 (hd xs) = m
    then 1
    else 0) + number_in_month(tl xs, m);

fun number_in_months(ds: (int*int*int) list, ms: int list) =
  if null ms
  then 0
  else
    number_in_month(ds, hd ms) + number_in_months(ds, tl ms);

fun dates_in_month(ds: (int*int*int) list, m: int) = 
  if null ds
  then []
  else
    let val dates = dates_in_month(tl ds, m)
    in
      (if number_in_month([hd ds], m) > 0
       then hd ds::dates
       else dates)
    end;

fun dates_in_months(ds: (int*int*int) list, ms: int list) = 
  if null ms
  then []
  else
    dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms);

fun get_nth(ss: string list, n: int) = 
  if (n - 1) = 0
  then hd ss
  else
    get_nth(tl ss, n - 1);

fun date_to_string(d: (int*int*int)) = 
  get_nth(months, #2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d);

fun number_before_reaching_sum(sum: int, xs: int list) = 
  (if not(null xs) andalso hd xs < sum
  then 1 + number_before_reaching_sum(sum - hd xs, tl xs)
  else 0);

fun what_month(d: int) =
  number_before_reaching_sum(d, days_in_month) + 1;

fun month_range(d1:int, d2: int) =
  if d1 > d2
  then []
  else
    let val m = what_month(d1)
    in 
      m::month_range(d1 + 1, d2)
    end;

fun oldest(ds: (int*int*int) list) =
  if null ds
  then NONE
  else
    let val v = oldest(tl ds)
    in
      if isSome v andalso is_older(valOf v, hd ds)
      then v
      else SOME(hd ds)
    end;

fun in_list(xs: int list, x: int) = 
  if null xs
  then false
  else hd xs = x orelse in_list(tl xs, x);

fun remove_duplicates(xs: int list) = 
  if null (tl xs)
  then [hd xs]
  else
    let val unique = remove_duplicates(tl xs)
    in
      if in_list(unique, hd xs) then unique
      else (hd xs)::unique
    end;

fun another_remove_duplicates(xs: int list, unique: int list) = 
  if null xs
  then unique
  else
    if in_list(unique, hd xs)
    then another_remove_duplicates(tl xs, unique)
    else another_remove_duplicates(tl xs, unique @ [hd xs]);

fun number_in_months_challenge(xs: (int*int*int) list, ms: int list) = 
  number_in_months(xs, remove_duplicates(ms));
