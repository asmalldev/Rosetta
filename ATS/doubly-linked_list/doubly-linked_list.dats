(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** doubly-linked_list.dats
** implements a doubly-linked list
**
*)

(* ****** INCLUDES ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
//
(* ****** ****** *)

(* ****** DEFINITIONS ****** *)
//
datatype
linked0(a:t@ype) =
    | linked0_nil of ()
    | linked0_cons of (linked0(a), a, linked0(a))

exception LinkedSubscriptExn of ()
//
(* ****** ****** *)


(* ****** FUNCTIONS ****** *)
//
// appends item to left (the head)
fun{a:t@ype} linked0_append_left(l0: linked0(a), i0: a) =
    linked0_cons(linked0_nil(), i0, l0)

// appends item to right (the tail)
fun{a:t@ype} linked0_append_right(l0: linked0(a), i0: a) =
    linked0_cons(l0, i0, linked0_nil())

// gets next node to the right
fun{a:t@ype} linked0_succ(l0: linked0(a)): linked0(a) =
    (
    case l0 of
    | linked0_nil() => linked0_nil()
    | linked0_cons(ll, l, lr) => lr
    )

// gets previous node to the left
fun{a:t@ype} linked0_prev(l0: linked0(a)): linked0(a) =
    (
    case l0 of
    | linked0_nil() => linked0_nil()
    | linked0_cons(ll, l, lr) => ll
    )

// appends item at nth position starting from the head
fun{a:t@ype} linked0_append_exn(l0: linked0(a), i0: a, n: int) = let
    fun aux(l0: linked0(a), i0: a, n: int, i: int) =
        if i < n-1 then
            (
            case l0 of
            | linked0_nil() => $raise LinkedSubscriptExn()
            | linked0_cons(ll, l, lr) => aux(lr, i0, n, i+1)
            )
        else
            (
            case l0 of
            | linked0_nil() => $raise LinkedSubscriptExn()
            | linked0_cons(ll, l, lr) =>
                linked0_cons(ll, l, linked0_cons(l0, i0, lr))
            )
in
    aux(l0, i0, n, 0)
end

//
(* ****** ****** *)


(* ****** MAIN ****** *)
implement main0 () =
{

val l0 = linked0_nil()
val l1 = linked0_append_left<int>(l0, 1)
val l2 = linked0_append_left<int>(l1, 2)
val l3 = linked0_append_left(l2, 3)

//val () = print_linked0<int>(l2)

}

