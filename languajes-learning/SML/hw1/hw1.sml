(* use "hw1.sml"; *)

fun is_older ( (y1:int, m1:int, d1:int)
             , (y2:int, m2:int, d2:int)) =
            y1 < y2
    orelse (y1 = y2 andalso m1 < m2)
    orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)

(* using tail recursion *)
fun number_in_month ( dates : (int * int * int) list , m : int ) =
    let fun iter(dates_remain : (int * int * int) list , accu : int) =
            if null dates_remain
            then accu
            else let val new_accu = if #2 (hd dates_remain) = m
                                   then accu + 1
                                   else accu
                 in iter(tl dates_remain, new_accu)
                 end
    in
        iter(dates, 0)
    end

fun number_in_months ( dates : (int * int * int) list , ms : int list ) =
    let fun iter(months : int list, accu : int) =
            if null months
            then accu
            else iter(tl months, accu + number_in_month(dates, hd months))
    in
        iter(ms, 0)
    end

fun dates_in_month( dates : (int * int * int) list , m : int ) =
    if null dates
    then []
    else if #2 (hd dates) = m
         then hd dates :: dates_in_month(tl dates, m)
         else dates_in_month(tl dates, m)

fun dates_in_months( dates : (int * int * int) list , ms : int list ) =
    if null ms
    then []
    else dates_in_month(dates, hd ms) @ dates_in_months(dates, tl ms)

fun get_nth(string_list :string list, nth :int) =
    if nth = 1
    then hd string_list
    else get_nth(tl string_list, nth - 1)

val monthsString = [ "January", "February", "March", "April" , "May" 
                   , "June" , "July", "August" , "September" 
                   , "October", "November", "December" ]

fun date_to_string(y :int, m :int, d :int) =
    let
        val year = Int.toString y
        val month = get_nth(monthsString, m)
        val day = Int.toString d
    in
        month ^ " " ^ day ^ ", " ^ year
    end

fun number_before_reaching_sum(sum :int, numbers :int list) =
    let val fst_num = hd numbers
    in
        if sum <= fst_num
        then []
        else fst_num :: number_before_reaching_sum(sum - fst_num, tl numbers)
    end

fun what_month(day :int) =
    let val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        length ( number_before_reaching_sum(day, days_per_month) ) + 1
    end

fun month_range(day1 :int, day2: int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1+1, day2)

fun oldest(dates: (int * int * int) list) =
    if null dates
    then NONE
    else
        let fun older(d1 :(int*int*int), d2 :(int*int*int)) =
                if is_older(d1, d2)
                then d1
                else d2

            fun iter( dates_remain: (int * int * int) list
                    , old_temp: (int * int * int)) =
                if null dates_remain
                then old_temp
                else iter(tl dates_remain, older(old_temp, hd dates_remain))
        in
            SOME (iter(tl dates, hd dates))
        end

fun without_repeated(ls: int list) =
    let fun elem(num :int, numbers: int list) =
            if null numbers
            then false
            else if num = hd numbers
                 then true
                 else elem(num, tl numbers)
        fun iter(remain_nums :int list, dont_repeated :int list) =
            if null remain_nums
            then dont_repeated
            else iter( tl remain_nums,
                       if elem(hd remain_nums, dont_repeated)
                       then dont_repeated
                       else dont_repeated @ [hd remain_nums]
                     )
    in
        iter(ls, [])
    end

fun number_in_months_challenge
            ( dates : (int * int * int) list, ms : int list ) =
    number_in_months(dates, without_repeated(ms))

fun dates_in_months_challenge
            ( dates : (int * int * int) list, ms : int list ) =
    dates_in_months(dates, without_repeated(ms))


fun is_leap_year(year :int) = 
    let fun divisible(a :int, b :int) = a mod b = 0
        val isYearDiv4 = divisible(year, 4)
        val isYearDiv400 = divisible(year, 400)
        val isYearDiv100 = divisible(year, 100)
    in
               isYearDiv400
        orelse (isYearDiv4 andalso (not isYearDiv100))
    end

fun get_nth_ints(int_list :int list, nth :int) =
    if nth = 1
    then hd int_list
    else get_nth_ints(tl int_list, nth - 1)

fun days_in_month(m :int, y :int) =
    let val days_per_month = [ 31, if is_leap_year y then 29 else 28
                             , 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        get_nth_ints(days_per_month, m)
    end

fun reasonable_date(y :int, m :int, d :int) =
    y > 0 andalso m >= 1 andalso m <= 12
          andalso d >= 1 andalso d <= days_in_month(m, y)

(* use "hw1_test.sml"; *)
