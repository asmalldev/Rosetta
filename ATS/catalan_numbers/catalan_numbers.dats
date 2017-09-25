(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** catalan_numbers.dats
** prints the first 15 catalan numbers
**
** makes a stream of the catalan numbers
** prints the first 15
**
*)

(* ****** INCLUDES ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
//
(* ****** ****** *)

(* ****** FUNCTIONS ****** *)
//

// returns a stream of the catalan numbers
fun catalans(): stream(int) = let
    fun aux(n: int, p: int): stream(int) = let
        val c = if n = 0 then 1 else (((4*n)-2) * p) / ( n+1)
    in
        $delay(stream_cons(c, aux(n+1, c)))
    end
in
    aux(0, 1)
end

(* ****** ****** *)


(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing catalan_numbers.dats ...\n")

val catalan_numbers = catalans()

val _ = assertloc(stream_nth_exn(catalan_numbers, 10) = 16796)
val _ = assertloc(stream_nth_exn(catalan_numbers, 13) = 742900)

val first_fifteen = stream_take_exn(catalan_numbers, 16)
val _ = print("First 15 catalan numbers: ")
val _ = println! first_fifteen
val _ = list_vt_free(first_fifteen)

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
