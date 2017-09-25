(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** knuth_shuffle.dats
** randomly shuffles an array by swapping a random element
** with the ith element (i goes from len(array)-1 to 0
**
** NOTE: the randomness of the random_int_under() function is not very random.
** This stems from the fact that the random numbers are asked for in quick
** succession. The random function uses the current time as the seed, and
** without allowing this time to change, the same integer is produced for each
** iteration of the loop.
**
** To account for this, I build a list where each successive random number is
** regenerated if it is equal to the number that came before it. This is very
** inefficient and requires many regenerations. However, it results in a more
** pseudorandom list of integers rather than a list of the same integer. The
** poor running-time is due to the generation of random numbers in this way, not
** the shuffling of elements.
**
*)

(* ****** INCLUDES ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
staload STDLIB = "libats/libc/SATS/stdlib.sats"
staload UN     = "prelude/SATS/unsafe.sats"
staload TIME   = "libats/libc/SATS/time.sats"
//
(* ****** ****** *)

(* ****** FUNCTIONS ****** *)
//
// gets a random integer less than n using unsafe function/ c-style
fun random_int_under(n: int) = let
    val _ = $STDLIB.srand($UN.cast {uint} ($TIME.time_get()))
    val r = $STDLIB.rand()
    //var tv: $TIME.timespec // uninitialized
    //var tv = $UN.cast {int} (1)
    //val _ = $TIME.nanosleep_null(tv)
in
    r mod n
end

// returns a list of l random numbers all under n
fun random_list0_under(n: int, l: int): list0(int) = let
    val init = random_int_under(n)
    fun random_next(prev: int): int = let
        val r = random_int_under(n)
    in
        if r = prev then random_next(prev)
        else r
    end
    fun build(l0: list0(int)) = 
        if length(l0) = l then l0
        else
            (
            case l0 of
            | list0_nil() => build(list0_sing(random_next(init)))
            | list0_cons(x, xs) => build(list0_extend(l0,random_next(list0_last_exn(l0))))
            )
in
    build(list0_nil())
end

// for each element array[i], swaps with random element array[j]
fun{a: t@ype} knuth_shuffle(arr: array0(a)): array0(a) = let
    val len = sz2i(array0_get_size(arr)) - 1
    val _ = println!("generating randoms numbers...")
    val randoms = random_list0_under(len, len+1)
    val _ = println!("shuffling array...")
    fun aux(arr: array0(a), i: int): array0(a) = let
        val j = randoms[i]
        val ie = arr[i]
        val je = arr[j]
        val () = arr[i] := je
        val () = arr[j] := ie
        
    in
        if i = 0 then
            arr
        else
            aux(arr, i-1)
    end
in
    aux(arr, len)
end
//
(* ****** ****** *)


(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing knuth_shuffle.dats ...\n")

val A = (array0)$arrpsz{int}(0,1,2,3,4,5,6,7,8,9)

val _ = print("Original array: ")
val _ = array0_foreach<int> (A, lam(x) => print_int(x))
val _ = print_newline()

val S = knuth_shuffle<int>(A)

val _ = print("Random shuffle: ")
val _ = array0_foreach<int> (S, lam(x) => print_int(x))
val _ = print_newline()

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
