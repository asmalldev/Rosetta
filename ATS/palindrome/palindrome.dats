(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** palindrome.dats
** palindrome_check checks if a sequence of characters (string) is a palindrome
**
*)

(* ****** INCLUDES ****** *)
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
(* ****** ****** *)

(* ****** FUNCTIONS ****** *)
fun palindrome_check(str: string): bool =
let
    fun aux(sl: list0(char)): bool =
        if (list0_is_empty(sl)) || (length(sl) <= 1)
        then true
        else
            if list0_head_exn(sl) != list0_last_exn(sl)
            then false
            else
                let
                    fn{a:t@ype} list0_remove_ends(l: list0(a)): list0(a) =
                    list0_remove_at_exn(list0_remove_at_exn(l, length(l)-1),0)
                in
                    aux(list0_remove_ends(sl))
                end
in
    aux(string_explode(str))
end
(* ****** ****** *)

(* ****** MAIN ****** *)
implement main0 () =
{
val s0 = "hello"                // false
val s1 = "racecar"              // true
val s2 = "atoyota"              // true
val s3 = "toohottohoot"         // true
val s4 = "madam"                // true
val s5 = "neveroddoreven"       // true
val s6 = "notapalindrome"       // false
val s7 = "racecare"             // false
val s8 = "willow"               // false

val _ = println!("Testing palindrome_check() ...")

// should be true
val () = (print("\t\"" + s1 + "\":\t\t"); println!(palindrome_check(s1)))
val () = assertloc(palindrome_check(s1))
val () = (print("\t\"" + s2 + "\":\t\t"); println!(palindrome_check(s2)))
val () = assertloc(palindrome_check(s2))
val () = (print("\t\"" + s3 + "\":\t\t"); println!(palindrome_check(s3)))
val () = assertloc(palindrome_check(s3))
val () = (print("\t\"" + s4 + "\":\t\t"); println!(palindrome_check(s4)))
val () = assertloc(palindrome_check(s4))
val () = (print("\t\"" + s5 + "\":\t"); println!(palindrome_check(s5)))
val () = assertloc(palindrome_check(s5))

// should be false
val () = (print("\t\"" + s0 + "\":\t\t"); println!(palindrome_check(s0)))
val () = assertloc(palindrome_check(s0) = false)
val () = (print("\t\"" + s6 + "\":\t"); println!(palindrome_check(s6)))
val () = assertloc(palindrome_check(s6) = false)
val () = (print("\t\"" + s7 + "\":\t\t"); println!(palindrome_check(s7)))
val () = assertloc(palindrome_check(s7) = false)
val () = (print("\t\"" + s8 + "\":\t\t"); println!(palindrome_check(s8)))
val () = assertloc(palindrome_check(s8) = false)

val _ = println!("All tests passed!")
}
(* ****** ****** *)
