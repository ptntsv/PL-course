(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1.sml";

(*is_older*)
val test1_0 = is_older ((1,2,3),(2,3,4)) = true;
val test1_1 = is_older ((1, 1, 3), (1, 2, 3)) = true;
val test1_2 = is_older ((1, 1, 2), (1, 1, 3)) = true;
val test1_3 = is_older ((1, 1, 2), (1, 1, 2)) = false;
val test1_4 = is_older ((2, 1, 2), (1, 1, 2)) = false;
val test1_5 = is_older ((10, 1, 1), (1, 10, 10)) = false;

(*number_in_month*)
val test2_0 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1;
val test2_1 = number_in_month ([(2012,2,28),(2013,12,1), (2021, 2, 1), (2012, 2,
 30), (2013, 2, 1)],2) = 4;
val test2_2 = number_in_month ([(2012,5,28),(2013,12,1), (2021, 2, 1), (2012, 2,
 30), (2013, 2, 1)],5) = 1;

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
