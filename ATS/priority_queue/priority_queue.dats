(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** priority_queue.dats
** implements a priority queue: a queue where items are ordered by
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
absvtype pqueue_vtype(@(a:vt@ype, int(*priority*)), int(*id*)) = ptr
vtypedef pqueue(id:int, a:vt@ype, priority:int) = pqueue_vtype((a,priority) id)
vtypedef pqueue (a:vt@ype) = [id: int, priority: int] pqueue(a, id, priority)

//
(* ****** ****** *)

(* ****** FUNCTIONS ****** *)
//

absprop ISNIL(id:int, b:bool)
fun {a:vt@ype}
pqueue_isnil{id:int}(!pqueue(a, id, priority)): [b:bool] (ISNIL(id,b) | bool(b))

(*
// removes and the rightmost
fun{a:t@ype}
pQueue0_pop(q: pQueue0(a)) =
//
(* ****** ****** *)
*)

(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing [program_name].dats ...\n")

//val q = pQueue0_cons(('a',1), pQueue0_nil())

val p = get_priority(('c', 3))
val _ = println!(p)

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
