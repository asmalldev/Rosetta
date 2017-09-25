(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** balanced_brackets.dats
** tests whether a string of brackets is balanced
**
*)

(* ****** INCLUDES ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)

(* ****** FUNCTIONS ****** *)
//

fn is_balanced(brackets: string): bool = let
    val l0 = string_explode(brackets)
    val open_string = "["
    val close_string = "]"
    val open = open_string[0]
    val close = close_string[0]
    fun aux(l0: list0(char), count:int): bool =
        if (count < 0) then false else
        (
        case l0 of
        | list0_nil() => if (count = 0) then true else false
        | list0_cons(x, xs) => if (x = open) then aux(xs, count+1)
                                else
                                    if (x = close) then aux(xs, count-1)
                                    else aux(xs, count)
        )
in
    aux(l0, 0)
end

//
(* ****** ****** *)


(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing is_balanced() ...\n")

val t0 = "[]"
val t1 = "[][]"
val t2 = "[[][]]"
val t3 = "[[[]]][][]"


val f0 = "]["
val f1 = "][]["
val f2 = "[]][[]"
val f3 = "]][[[]]["

// should be true
val _ = (print(t0 + " --> "); println!(is_balanced(t0)))
val _ = assertloc(is_balanced(t0))
val _ = (print(t1 + " --> "); println!(is_balanced(t1)))
val _ = assertloc(is_balanced(t1))
val _ = (print(t2 + " --> "); println!(is_balanced(t2)))
val _ = assertloc(is_balanced(t2))
val _ = (print(t3 + " --> "); println!(is_balanced(t3)))
val _ = assertloc(is_balanced(t3))

val _ = println!("")

// should be false
val _ = (print(f0 + " --> "); println!(is_balanced(f0)))
val _ = assertloc(not(is_balanced(f0)))
val _ = (print(f1 + " --> "); println!(is_balanced(f1)))
val _ = assertloc(not(is_balanced(f1)))
val _ = (print(f2 + " --> "); println!(is_balanced(f2)))
val _ = assertloc(not(is_balanced(f2)))
val _ = (print(f3 + " --> "); println!(is_balanced(f3)))
val _ = assertloc(not(is_balanced(f3)))

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
