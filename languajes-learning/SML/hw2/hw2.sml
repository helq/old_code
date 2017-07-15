
(* 1 *)
fun all_except_option (_, [])    = NONE
  | all_except_option (s, l::ls) =
        if same_string(s, l)
        then SOME ls
        else case all_except_option(s,ls) of
                  SOME ls' => SOME (l::ls')
                | NONE     => NONE

fun get_substitutions1([], _) = []
  | get_substitutions1(ls::lss, s) =
        case all_except_option(s, ls) of
              NONE     => get_substitutions1(lss, s)
            | SOME ls' => ls' @ get_substitutions1(lss, s)

fun get_substitutions2(lss, s) = let
    fun maker ([], accu) = accu
      | maker (ls::lss', accu) =
           case all_except_option(s, ls) of
               NONE     => maker (lss', accu)
             | SOME ls' => maker (lss', accu @ ls')
    in
        maker (lss, [])
    end

fun similar_names(similar_lists, {first=f, middle=m: string, last=l: string}) = let
    fun adding_complete_names [] = []
      | adding_complete_names (f::fs) = 
          {first=f, middle=m, last=l} :: (adding_complete_names fs)
    in
        {first=f, middle=m, last=l} ::
            adding_complete_names (get_substitutions1(similar_lists, f))
    end



(* 2 *)

fun card_color ((s, _): card) =
    case s of
          Clubs  => Black
        | Spades => Black
        | _      => Red

fun card_value ((_, Ace): card) = 11
  | card_value (_, Num n) = n
  | card_value _ = 10

fun remove_card (cs, c: card, e) =
    case cs of
          [] => raise e
        | c'::cs' => if c' = c
                     then cs'
                     else c' :: remove_card (cs', c, e)

fun all_same_color (cs: card list) =
    case cs of
          []  => true
        | [_] => true
        | c::c'::cs' => card_color c = card_color c' andalso (all_same_color cs')

fun sum_cards cs = let
    fun sum ([], accu) = accu
      | sum (c::cs, accu) = sum (cs, card_value c + accu)
    in
        sum(cs, 0)
    end

fun score (held_cards, goal) = let
    val sum_held = sum_cards held_cards
    val preliminary = if sum_held > goal
                      then 3*(sum_held - goal)
                      else goal - sum_held
    in
        if all_same_color held_cards
        then preliminary div 2
        else preliminary
    end

fun officiate_general(cs: card list, ms: move list, goal: int, score) = let
    fun helper(held_cards, ms, cs) =
        case ms of
              [] => score (held_cards, goal)
            | (Discard c)::ms' => helper(remove_card(held_cards, c, IllegalMove), ms', cs)
            | Draw :: ms' => case cs of
                                 [] => score (held_cards, goal)
                               | c::cs' => let val new_held_cards = c::held_cards
                                           in
                                             if (sum_cards new_held_cards) > goal
                                             then score (new_held_cards, goal)
                                             else helper(new_held_cards, ms', cs') 
                                           end
    in
        helper([], ms, cs)
    end

fun officiate(cs: card list, ms: move list, goal: int) = 
        officiate_general(cs: card list, ms: move list, goal: int, score)

(* 3 - challenge *)

fun score_challenge (cs, goal) = let
    fun extract_Aces(cs, accu, cs') = 
        case cs of
            []           => (accu, cs')
          | (s, Ace)::cs => extract_Aces(cs, s::accu, cs')
          |        c::cs => extract_Aces(cs, accu, c::cs')

    fun create_optimal_weight_aces(ss', n) =
        case ss' of
              s::ss => (s, Num n) :: create_optimal_weight_aces(ss, 0)
            | [] => []

    val (aces, held_cards) = extract_Aces(cs, [], [])
    val number_Aces = length aces
    in
    if number_Aces > 0
        then let
            val sum_held_cards = sum_cards held_cards
            val optimal_number_for_aces =
                if sum_held_cards + number_Aces <= goal
                then if goal <= sum_held_cards + (number_Aces*11)
                    then goal - sum_held_cards
                    else number_Aces*11
                else number_Aces
             val cs' = create_optimal_weight_aces(aces, optimal_number_for_aces)
             in
                score(cs' @ cs, goal)
             end
        else score(cs, goal)
    end

fun officiate_challenge(cs: card list, ms: move list, goal: int) = 
      officiate_general(cs: card list, ms: move list, goal: int, score_challenge)

(*
use "hw2provided.sml";
use "hw2.sml";
*)
