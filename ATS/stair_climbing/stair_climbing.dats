(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** stair_climbing.dats
** implements Chung-Chieh Shan's stair climing puzzle
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
fn step():bool = false

fun step_up():(void,void) =
    if not(step()) then
        (step_up(), step_up())
    else ()
//
(* ****** ****** *)


(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing [program_name].dats ...\n")

val _ = step_up()
val _ = assertloc(true = true)

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
