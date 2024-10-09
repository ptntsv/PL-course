(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";

val ss = ["String", "A", "a", "", "Long string", "Much longer string"];
val ss' = ["String", "Strin1", "Strina"];
val ss'' = ["Strina", "Strin1", "String"];
val lst5 = ["ahhahahhahahahahl", "Abc", "AB", "ABCDE", "abc", "A"];

longest_string1 ss = longest_string3 ss;
longest_string1 [] = longest_string3 [];
longest_string1 ss' = longest_string3 ss';
longest_string1 ss'' = longest_string3 ss'';

longest_string2 ss = longest_string4 ss;
longest_string2 [] = longest_string4 [];
longest_string2 ss' = longest_string4 ss';
longest_string2 ss'' = longest_string4 ss'';


val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test5_1 = longest_capitalized lst5 = "ABCDE"

val test6 = rev_string "abc" = "cba"
val test6_1 = rev_string "a" = "a"
val test6_2 = rev_string "12345" = "54321"


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val nested_p = ConstructorP("c1", ConstructorP("c2",
TupleP([Wildcard, Variable("abc"), ConstructorP("c2", TupleP([Wildcard,
Wildcard, Variable("z"), Variable("x"), Variable("c"), Variable("abc"),
Variable("z")]))])));

val test9a = count_wildcards nested_p = 3;

val test9b = count_wild_and_variable_lengths nested_p;

val test9c = count_some_var ("x", Variable("x")) = 1;

val test9d = count_some_var ("abc", nested_p) = 2;

val unique_p = ConstructorP("c1", ConstructorP("c2",
TupleP([Wildcard, Variable("abc"), ConstructorP("c2", TupleP([Wildcard,
Wildcard, Variable("y"), Variable("x"), Variable("c"), Variable("cba"),
Variable("z")]))])));

val test10a = check_pat nested_p = false;
val test10b = check_pat unique_p = true;

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
