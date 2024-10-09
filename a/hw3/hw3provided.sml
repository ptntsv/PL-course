(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

fun only_capitals ss = List.filter (fn s => Char.isUpper(String.sub(s, 0))) ss;

fun longest_string1 ss = foldl (fn (s, acc) => if String.size s > String.size
  acc then s else acc) "" ss;

fun longest_string2 ss = foldl (fn (s, acc) => if String.size s >= String.size
  acc then s else acc) "" ss;

fun longest_string_helper f ss =
  List.foldl (fn (s, acc) => if f(String.size s, String.size acc) then s else
    acc) "" ss

val longest_string3 = longest_string_helper (fn (x, y) => x > y);

val longest_string4 = longest_string_helper (fn (x, y) => x >= y);

val longest_capitalized = longest_string3 o only_capitals;

val rev_string = implode o rev o explode;

fun first_answer f xs = 
  case List.filter isSome (List.map f xs) of
      SOME v :: _ => v
     | _=> raise NoAnswer;

fun all_answers f xs =
  let fun aux(xs, acc) =
    case xs of
         [] => SOME acc
       | SOME v :: xs => aux(xs, acc @ v)
       | _ => NONE
  in
    aux(List.map f xs, [])
  end;

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

val count_wildcards =
  g (fn x => 1) (fn y => 0);

val count_wild_and_variable_lengths =
  g (fn x => 1) (fn s => String.size s);

fun count_some_var (s,p) =
   g (fn x => 0) (fn s' =>  if s = s' then 1 else 0) p

fun check_pat p =
  let fun var_names_to_lst p =
    case p of
         Variable s => [s]
       | TupleP ps => List.foldl (fn (p, lst) => ((var_names_to_lst  p) @ lst)) [] ps
       | ConstructorP(_, p') => var_names_to_lst p'
       | _ => []

  in List.foldl
      (fn (s, flag) => (count_some_var(s, p) = 1 andalso flag))
      true
      (var_names_to_lst p)
  end;

fun match (v, p) =
  case (v, p) of
       (_,Wildcard) => SOME []
     | (_, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const c', ConstP c) => if c = c' then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if (List.length ps <> List.length vs) then NONE else
                  all_answers match (ListPair.zip(vs, ps))
     | (Constructor(s', p'), ConstructorP(s, p)) => if s=s' then match(p', p)
                                                    else NONE
     | _ => NONE

fun first_match v ps =
  SOME (first_answer (fn p => match(v, p)) ps) handle NoAnswer => NONE;
