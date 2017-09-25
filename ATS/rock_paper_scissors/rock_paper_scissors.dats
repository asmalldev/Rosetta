(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** rock_paper_scissors.dats
** implementation of the rock paper scissors game with a simple AI opponent
**
*)

(* ****** INCLUDES ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)


(* ****** DEFINITIONS ****** *)
//
datatype
move =
    | rock of ()
    | paper of ()
    | scissors of ()
    | move_nil of ()
//
(* ****** ****** *)
(*
fn movelt(x: move, y: move): bool =
(case x of
| rock =>
    (case y of
    | rock => false
    | paper => true
    | scissors => false
    )
| paper =>
    (case y of
    | rock => false
    | paper => false
    | scissors => true
    )
| scissors =>
    (case y of
    | rock => true
    | paper => false
    | scissors => false
    )
)


*)
(* ****** FUNCTIONS ****** *)
//
fn movelt(x: move, y: move): bool =
(case- (x,y) of
| (rock, paper) => true
| (paper, scissors) => true
| (scissors, rock) => true
| (_,_) => false
)

//fn movegt(move, move): bool

//fn moveeq(move, move): bool


//
(* ****** ****** *)


(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing [program_name].dats ...\n")

val t = movelt(move_nil(),move_nil())
val _ = println!(t)

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
