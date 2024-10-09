(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

fun contains(s, ss) =
  case ss of
       [] => false
     | s'::ss' => same_string(s,s') orelse contains(s, ss');

fun rev xs =
  let fun aux(lst, acc) = 
    case lst of
         [] => acc
       | x::xs => aux(xs, x::acc)
  in
    aux(xs, [])
  end;

fun all_except_option(s, ss) =
  if contains(s, ss) = false then NONE else
    let fun trusted (s', ss') = 
      case ss' of
           [] => []
         | t'::tt' => if same_string(t', s) then trusted(s, tt') else t'::trusted(s, tt');
    in
      SOME(trusted(s, ss))
    end;

fun get_substitutions1(ss, s) = 
  case ss of
    [] => []
     | s'::ss' => case all_except_option(s, s') of
              NONE => get_substitutions1(ss', s)
            | SOME subs => subs @ get_substitutions1(ss', s);

fun get_substitutions2(ss, s) =
  let fun aux(ss', acc) = 
    case ss' of
         [] => acc
       | s'::ss' => case all_except_option(s, s') of
                         NONE => aux(ss', acc)
                       | SOME sbs => aux(ss', acc @ sbs)
  in
    aux(ss, [])
  end;

fun similar_names(lst, {first=f, middle=m, last=l}) =
  let fun aux(ss, acc) =
    case ss of
         [] => rev acc
       | s'::ss' => aux(ss', {first=s', last=l, middle=m}::acc)
  in
    aux(get_substitutions1(lst, f), [{first=f, last=l, middle=m}])
  end;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun contains_card(cs, c) =
  case cs of
       [] => false
     | c'::cs' => (c' = c) orelse contains_card(cs', c);

fun card_color (s,_) = 
  case s of
       Clubs => Black
     | Spades => Black
     | _ => Red;

fun card_value (_, r) =
  case r of
       Num i => i
     | Jack => 10
     | Queen => 10
     | King => 10
     | Ace => 11

fun remove_card (cs, c, e) =
  if contains_card(cs, c) = false then raise e else
    let fun aux(cs, acc) =
      case cs of
           [] => acc
         | c'::cs' => if (c' = c) then acc @ cs' else aux(cs', acc @ [c'])
    in
      aux(cs, [])
    end;

fun better_remove_card (cs, c, e) =
  if contains_card(cs, c) = false then raise e else
    let fun helper(cs, c, f) =
      case cs of
           [] => []
         | c'::cs' => if (c' = c) andalso f = true then helper(cs',c,false) else
           c'::helper(cs',c,f)
    in
      helper(cs, c, true)
    end;

fun all_same_color cs = 
  case cs of
       c1::(c2::cs') => card_color(c1) = card_color(c2) andalso
       all_same_color(c2::cs')
     | _ => true;

fun sum_cards cs =
  let fun aux (cs, acc) = 
    case cs of
         [] => acc
       | c'::cs' => aux(cs', acc + card_value(c'))
  in
    aux(cs, 0)
  end

fun score(cs, g) =
  let val s = sum_cards cs
  in
    let val preliminary =
      if s > g then 3 * (s - g) else g - s
    in
      if all_same_color(cs) then (preliminary div 2) else preliminary
    end
  end;

fun officiate(cs, ms, g) =
  let fun move(hc, cs, ms) =
    case ms of
         [] => score(hc, g)
       | m'::ms' => case m' of
                         Discard c => move(better_remove_card(hc, c,
                         IllegalMove), cs, ms')
                       | Draw => case cs of
                                      [] => score(hc, g)
                                    | c'::cs' => let val curr_score = score(c'::hc,
                                    g)
                                                 in
                                                   if curr_score > g then curr_score
                                                   else move(c'::hc, cs', ms') 
                                                 end

  in
    move([], cs, ms)
  end;
