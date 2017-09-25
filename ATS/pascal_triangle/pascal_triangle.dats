(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** pascal_triangle.dats
** prints the first n rows of pascal's triangle
** uses as stream of lists to represent rows
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

// gets the next row of pascal's triangle given the previous row
fn row_succ(prev:list0(int)): list0(int) = let
    val len = length(prev)
    fun aux(p: list0(int), i: int, n: list0(int)): list0(int) =
        if (i = len) then list0_extend(n, 1)
        else if (i = 0) then aux(p, i+1, list0_extend(n, 1))
        else aux(p, i+1, list0_extend(n, prev[i-1] + prev[i]))            
in
    (
    case prev of
    | list0_nil() => list0_make_sing<int>(1)
    | list0_cons(_,_) => aux(prev, 0, list0_nil())
    )
end
        
// returns a stream of the rows of pascal's triangle
fn rows(): stream(list0(int)) = let
    fun aux(p: list0(int)) = let
        val n = row_succ(p)
    in
        $delay(stream_cons(p, aux(n)))
    end
in
    aux(list0_make_sing<int>(1))
end

// prints the first n rows of the triangle
fn pascal_print_exn(n): void = let
    val triangle = rows()
    //val n = g1int_succ(n)
    val nrows = stream_take_exn(triangle, n)
    val _ = list_vt_foreach_fun<list0(int)> (nrows, lam (x) =<1>
            (print_list0<int> (x); println!("")))
    val _ = list_vt_free(nrows)
in

end

//
(* ****** ****** *)


(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing [program_name].dats ...\n")

val _ = println!("Printing first 8 rows...")
val _ = pascal_print_exn(8)

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
