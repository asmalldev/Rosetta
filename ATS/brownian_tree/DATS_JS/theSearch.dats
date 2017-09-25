(* ****** ****** *)
(*
** BFS Search
*)
(* ****** ****** *)
//
#include "./theParams.hats"
//
#include
"{$LIBATSCC2JS}/staloadall.hats"
//
(* ****** ****** *)
//
#staload "./main.sats"
//
(* ****** ****** *)
//
fun
x_dec(x: int): int =
  if x > 0
    then (x-1) else (XLEN-1)
  // end of [if]
fun
x_inc(x: int): int =
  if x+1 < XLEN then (x+1) else 0
//
(* ****** ****** *)
//
fun
y_dec(y: int): int =
  if y > 0
    then (y-1) else (YLEN-1)
  // end of [if]
fun
y_inc(y: int): int =
  if y+1 < YLEN then (y+1) else 0
//
(* ****** ****** *)
//
#include
"{$LIBATSCC2JS}/DATS\
/BUCS320/GraphStreamize/GraphStreamize_bfs.dats"
//
(* ****** ****** *)
//
assume node_type = list0(pos)
assume nodelst_vtype = list0(node)
//
(* ****** ****** *)
//
implement
{}(*tmp*)
theStreamizeStore_insert_lst(nxs) =
(
nxs
).foreach()(lam nx => theStreamizeStore_insert(nx))
//
(* ****** ****** *)
//
implement
node_get_neighbors<>
  (nx0) = res where
{
//
val-
list0_cons(xy, _) = nx0
//
val
theBoard = theBoard_get()
//
val x = xy.0 and y = xy.1
//
val res = list0_nil()
//
macdef
nodext(x, y) =
list0_cons($tup(,(x), ,(y)), nx0)
//
val res = let
  val x = x_dec(x)
  val knd = theBoard[x, y]
in
  if (knd = 0 || knd = PRIZE)
    then list0_cons(nodext(x, y), res) else res
end : nodelst
//
val res = let
  val x = x_inc(x)
  val knd = theBoard[x, y]
in
  if (knd = 0 || knd = PRIZE)
    then list0_cons(nodext(x, y), res) else res
end : nodelst
//
val res = let
  val y = y_dec(y)
  val knd = theBoard[x, y]
in
  if (knd = 0 || knd = PRIZE)
    then list0_cons(nodext(x, y), res) else res
end : nodelst
//
val res = let
  val y = y_inc(y)
  val knd = theBoard[x, y]
in
  if (knd = 0 || knd = PRIZE)
    then list0_cons(nodext(x, y), res) else res
end : nodelst
//
} (* end of [node_get_neighbors] *)
//
(* ****** ****** *)

fun{}
theSearch_streamize
(
(*void*)
) : stream(node) = let
//
val
theVisited =
mtrxszref_make_elt
  {int}(XLEN, YLEN, 0)
//
implement
node_mark<>(nx) = let
//
val-
list0_cons
  ($tup(x, y), _) = nx
//
in
  theVisited[x, y] := 1
end // end of [node_mark]
//
implement
node_unmark<>(nx) = ()
//
implement
node_is_marked<>(nx) = let
//
val-
list0_cons
  ($tup(x, y), _) = nx
//
in
  (theVisited[x, y] = 1)
end // end of [node_is_marked]
//
val
store =
qlistref_make_nil{node}()
//
val
theWorm = theWorm_get((*void*))
//
val () =
qlistref_insert
  (store, list0_sing(theWorm.pos()))
//
in
  GraphStreamize_bfs<>(store)
end (* end of [theSearch_streamize] *)

(* ****** ****** *)

implement
theAllAct_Search_do() = let
//
val
theBoard = theBoard_get()
//
val nxs = theSearch_streamize<>()
//
in
//
// Please finish implementing it!!!
//
end // end of [theAllAct_Search_do]

(*
implement
theAllAct_Search_do() =
(
  alert("theAllAct_Search_do: yet to be implemented!!!")
)
*)
(* ****** ****** *)

(* end of [theSearch.dats] *)
