(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** tree_visualize.dats
** visualize a tree in a human-readable manner
**
*)

(* ****** ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
//
(* ****** ****** *)


(* ****** DEFINITIONS ******* *)

datatype
tree0(a:t@ype) =
    | tree0_nil of ()
    | tree0_cons of (tree0(a), a, tree0(a))

(* ****** ****** *)

(* ****** FUNCTIONS ****** *)

fun{a:t@ype} visualize(t0: tree0(a)): void =
(
case t0 of
| tree0_nil() => print("Null tree")
| tree0_cons(tl,n,tr) => (print n; print '\n'; visualize(tl); visualize(tr))

)

(* ****** MAIN ****** *)

implement main0 () =
{
val t0 = tree0_nil()
val t1 = tree0_cons{int}(t0, 1, t0)
val t2 = tree0_cons{int}(t1, 2, t0)
//
val t3 = tree0_cons{int}(t1, 3, t1)
val t4 = tree0_cons{int}(t2, 3, t1)
val t5 = tree0_cons{int}(t2, 3, t2)
//
val t8 = tree0_cons{int}(t3, 8, t4)

val () = print("Hello")
//val () = visualize(t8)

}

