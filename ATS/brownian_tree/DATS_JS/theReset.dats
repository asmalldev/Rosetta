(* ****** ****** *)
(*
** Scene setup
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

typedef point = @{ x=int, y=int }

implement
theBoard_initialize
  ((*void*)) = let
//
val
theBoard = theBoard_get()
//
val N = min(XLEN, YLEN)
//
val scene = double2int(N*JSmath_random())
//

fun
add_blocks() =
(N).foreach()
(
lam(i) =>
if scene > N/2 then (
if i < N-5 && i > 5 then
   (
   theBoard[i,5] := BLOCK;

   if i > 5 then
   ( theBoard[N-i, i] := BLOCK;

   if i < N-10 && i > 18 then
   (theBoard[i,20] := BLOCK;
   )))
) (* add_blocks *)
else (
//if (i**2)/5 < N && i*10 < N then
if i < N -15 && i > 15 then
    (
    theBoard[26, i] := BLOCK;
    //theBoard[(i-1**2)/5,i*10] := BLOCK;
    )
else (
    if i > 18 && (~((i**2)/50)) < N && (N-((i**2)/50)) > 0 then
        (
        theBoard[(N-((i**2/50))),i] := BLOCK;
        )
    )
)
)

// checks if list contains elemetn
fn {a: t@ype} list0_contains(l0: list0(a), i: a): bool = let
    fun aux(l0: list0(a), item: a) =
    (
    case l0 of
    | list0_nil() => false
    | list0_cons(x, xs) => if geq_val_val<a>(x,i) then true else aux(xs, i)
    )
in
    aux(l0,i)
end

// generate random int within XLEN
fun xrand(): int = double2int(XLEN*JSmath_random())

// generate random int within YLEN
fun yrand(): int = double2int(YLEN*JSmath_random())

//
fun add_particle (tree: list0(point)) : int = let
    val x = xrand()
    and y = yrand()
    val up = point(x, y+1)
    val down = point(x, y-1)
    val left = point(x-1, y)
    val right = point(x+1, y)
    val p = point(x,y) 
    val knd = theBoard[x, y]
in
    if knd = 0
    then
        if (list0_contains<point>(tree, up) ||
            list0_contains<point>(tree, down ||
            list0_contains<point>(tree, left ||
            list0_contains<point>(tree, right)
            )
        then theBoard[x,y] := PRIZE; 0
        else 1
    else 1
end

//theBoard[x, y] := PRIZE else add_particle()
end // end of [add_particle]

// add 50 particles
fun add_particles() = let
    fun aux(i: int, tree: list0(point) =
        if i > 50 then ()
        else
            if 
in
    aux(0, list0_nil)
end

fun add_particles() = (50).repeat()(lam() => add_particle())
in
    add_particles()
end // end of [theBoard_initialize]

(* end of [theReset.dats] *)
