
(*
use "hw2provided.sml";
use "hw2.sml";
use "myTest.sml";
*)

fun test_01 () = let
    val part1 = all_except_option("a",["u","e","i","o","a","s","t"])
    val part2 = all_except_option("a",["u","e","i","o","n","s","t"])
    in
            part1 = SOME ["u","e","i","o","s","t"]
        andalso
            part2 = NONE
    end

fun test_02 () = let
    val part1 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
    val part2 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
    in
            part1 = ["Fredrick","Freddie","F"]
        andalso
            part2 = ["Jeffrey","Geoff","Jeffrey"]
    end

fun test_03 () = let
    val part = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})
    in
        part = [{first="Fred", last="Smith", middle="W"},
                {first="Fredrick", last="Smith", middle="W"},
                {first="Freddie", last="Smith", middle="W"},
                {first="F", last="Smith", middle="W"}]
    end








