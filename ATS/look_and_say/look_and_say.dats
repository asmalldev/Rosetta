(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** look_and_say.dats
** produces the next item of the look and say sequence given a starting sequence
**
*)

(* ****** INCLUDES ****** *)
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
(* ****** ****** *)

(* ****** FUNCTIONS ****** *)
fun look_and_say(start: string): string = let

    val seq = list0_extend(string_explode(start), ' ')
    val res = list0_nil()
    
    fun aux(seq: list0(char), res: list0(char),
        count: int, digit: char): string =
        if list0_is_empty(seq) then string_implode(res)
        else
            if digit != seq[0]
            then
                let
                    val scount = tostring_int(count)
                    val sdigit = tostring_char(digit)
                    val result = list0_append(res, string_explode(scount + sdigit))
                in
                    aux(list0_tail_exn(seq), result, 1, seq[0])
                end
            else
                aux(list0_tail_exn(seq), res, count+1, digit)
in
    aux(list0_tail_exn(seq), res, 1, seq[0])
end

fun print_n_look_say(n: int, func: (string) -> string): void = let
    fun loop(i: int, seq: string): void = let
            val s = func(seq)
            val _ = println! s
        in
            if i < n then loop(i+1, s)
        end
in
    loop(0, "1")
end

(* ****** ****** *)

(* ****** MAIN ****** *)
implement main0 () =
{

val s0 = "11"
val s1 = "1211"
val s2 = "312211"

val _ = println! "Testing look_and_say() ..."

// should be true
val () = (print(s0 + "\t-->\t"); println!(look_and_say(s0)))
val () = assertloc(look_and_say(s0) = "21")
val () = (print(s1 + "\t-->\t"); println!(look_and_say(s1)))
val () = assertloc(look_and_say(s1) = "111221")
val () = (print(s2 + "\t-->\t"); println!(look_and_say(s2)))
val () = assertloc(look_and_say(s2) = "13112221")

val _ = println! "\nFirst 10 of \"look and say\" sequence (after 1):"
val _ = print_n_look_say(10, look_and_say)

val _ = println! "\nAll tests passed!"

}
(* ****** ****** *)
