(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** happy_numbers.dats
** finds the first 8 happy numbers
**
** 1) implements a function is_happy(n:int): bool using recursion + functional
**    sets to determine if an integer is happy
** 2) constructs a stream (lazy) of the happy numbers
** 3) prints the first 8 elements of that stream
**
*)

(* ****** INCLUDES ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"

// include myfunset datatype + functions
typedef elt = int
local
staload FS = "libats/ML/SATS/funset.sats"
implement $FS.compare_elt_elt<elt>(x, y) = compare(x, y)
in
    #include "libats/ML/HATS/myfunset.hats"
end


(* ****** FUNCTIONS ****** *)
//

// from http://ats-lang.sourceforge.net/LIBRARY/prelude/SATS/DOCUGEN/HTML/string.html#string_foreach_env
fun atoi (str: string): int = let
    val str = g1ofg0_string (str)
    var env: int = 0
    implement
    string_foreach$fwork<int> (c, env) = env := 10 * env + (c - '0')
    val _ = string_foreach_env<int> (str, env)
in
    env
end

// from https://groups.google.com/forum/#!topic/ats-lang-users/Zv-Ev4yWIS4
fun make_string (c: char) = let
    val c = g1ofg0 (c)
in
    if iseqz (c) then "" else string_sing(c)
end

// squares an integer
fn square (x: int):<cloref> int = x * x

// adds two integers
fn add (x: int, y: int):<cloref> int = x + y

// uses functional sets + recursion to determine if n is a happy number
fun is_happy(n: int): bool = let
    val checked = myfunset_nil()
    fun aux(n: int, checked: myset): bool =
        if n = 1 then
            true
        else
            let
                val s = itoa(n)
                val l = string_explode(s)
                val h = list0_map<char><int>
                    (l, lam(x) => square(atoi(make_string(x))))
                val sum = list0_foldleft(h, 0, lam(res, x) => add(res,x))
            in
                if funset_is_member(checked, sum) then
                    false
                else
                    let
                        var checked = checked
                        val-false = checked.insert(sum)
                    in
                        aux(sum, checked)
                    end
            end
in
    aux(n, checked)
end

// gets next happy number after i
fun happy_succ(i: int): int = let
    val n = i +1
in
    if is_happy(n) then
        n
    else
        happy_succ(n)
end

// returns a stream of the happy numbers starting from n
fun happy_from(n): stream(int) =
    if is_happy(n) then
        $delay(stream_cons(n, happy_from(happy_succ(n))))
    else
        let
            val v = happy_succ(n)
        in
            $delay(stream_cons(v, happy_from(happy_succ(v))))
    end


(* ****** MAIN ****** *)

implement main0 () =
{

val _ = println!("Testing happy_numbers.dats ...\n")

val _ = assertloc(is_happy(1))
val _ = assertloc(is_happy(7))
val _ = assertloc(not(is_happy(8)))
val _ = assertloc(not(is_happy(20)))

val _ = print "The first eight happy numbers are: "
val happy_numbers = happy_from(0)
val first_eight = stream_take_exn(happy_numbers, 8)
val _ = println! first_eight
val _ = list_vt_free(first_eight)

val _ = println!("\nAll tests passed!")

}
