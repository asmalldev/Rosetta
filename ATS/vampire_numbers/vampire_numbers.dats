(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** vampire_numbers.dats
** prints the first 25 vampire numbers + their fangs
** where a vampire number is a nat with an even number of digits,
** which can be factored into tow numbers (fangs) which:
**      - each contain half the number of digits of the original
**      - and only one at most has a trailing zero
*)

(* ****** INCLUDES ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
staload MATH = "libats/libc/SATS/math.sats"
staload _(*MATH*) = "libats/libc/DATS/math.dats"
//
(* ****** ****** *)



(* ****** DEFINITIONS ****** *)
//
macdef sqrt(x) = $MATH.sqrt(,(x))
//
(* ****** ****** *)

(* ****** FUNCTIONS ****** *)
//

// from lecture 06/07
fun sieve(xs: stream(int)): stream(int) = $delay
let
    val-stream_cons(x0, xs) = !xs
    val xs = stream_filter_cloref(xs, lam(x) => x % x0 > 0)
in
  stream_cons(x0, sieve(xs))
end


fun ints_from(n: int): stream(int) =
    $delay(stream_cons(n, ints_from(n+1)))

// stream of the prime numbers
val primes = sieve(ints_from(2))

// returns a list of n's prime factors
fun pfac(n: int): list0(int) = let
    fun aux(n: int, pfacs: list0(int), div: int): list0(int) = let
        fun aux2(n: int, pfacs: list0(int), div: int): list0(int) =
            if (n % div) = 0 then
                aux2(n/div, list0_extend(pfacs, div), div)
            else
                pfacs
    in
        if (div * div) <= n then let
                val pfacs = aux2(n, pfacs, div)
                val pfacs = aux(n, pfacs, div+1)
            in
                pfacs
            end
        else
            if n > 1 then
                list0_extend(pfacs, n)
            else
                pfacs 
    end
        
in
    aux(n, list0_nil(), 2)
end

//
(* ****** ****** *)


(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing [program_name].dats ...\n")

val primesfirst = stream_take_exn(primes, 8)
val _ = println! primesfirst
val _ = list_vt_free(primesfirst)

val pfs = pfac(600)
val _ = println! pfs

val _ = assertloc(true = true)

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
