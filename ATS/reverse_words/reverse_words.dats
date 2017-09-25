(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** reverse_words.dats
** reverse the order of all tokens in a given string
**
** 1) splits string into a list of strings based " " delimiter
** 2) reverses the position of all words in the list
** 3) collapses the resulting list back into a string
**
** NOTE: steps 2 and 3 are accomplished simultaneously using list0_foldleft()
**
*)

(* ****** INCLUDES ****** *)
//
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"


(* ****** FUNCTIONS ****** *)
//

// from https://groups.google.com/forum/#!topic/ats-lang-users/Zv-Ev4yWIS4
fun make_string (c: char) = let
    val c = g1ofg0 (c)
in
    if iseqz (c) then "" else string_sing(c)
end

// splits a string into a list based on the delimiter parmeter
fun split(str: string, delim: char): list0(string) = let
    val src = string_explode(str)
    val res = list0_nil()
    val len = sz2i(length(str))
    fun aux(src: list0(char), delim: char, pos: int, wrd: string,
            res: list0(string)): list0(string) =
        if pos >= len then
            list0_append(res, list0_make_sing
                (string_append(wrd, make_string(delim))))
        else
            if src[pos] = delim then
                aux(src, delim, pos+1, "",
                    list0_append(res, list0_make_sing(
                        string_append(wrd,make_string(delim)))))
            else
                aux(src, delim, pos+1, wrd + make_string(src[pos]), res)
in
    aux(src, ' ', 0, "", res)
end

// reverse positions of all words (using list0_foldleft())
fn reverse_words(str: string): string = let
    val ls = split(str, ' ')
    val lr = list0_reverse(ls)
in
    list0_foldleft(lr, "", lam(res, x) => string_append(res, x))
end

// custom map function only used for mapping reverse_words(non-closure) to list
fun {a: t@ype}{b: t@ype} map_to_list0(lst: list0(a), f: a -> b): list0(b) =
    (
    case lst of
    | list0_nil() => list0_nil()
    | list0_cons(s, lr) => list0_cons(f(s), map_to_list0(lr, f))
    )

// prints each string in a list of strings
fun println_list0_string(xs: list0(string)) = let
    fun f (x: string) :<cloref1> void = let
        val () = println!(x)
    in
    end
val _ = list0_foreach (xs, f)
in
end

// returns list version of frost poem
fun frost(): list0(string) =
list0_cons(
"---------- Ice and Fire ------------",
list0_cons(
" ",
list0_cons(
"fire, in end will world the say Some",
list0_cons(
"ice. in say Some",
list0_cons(
"desire of tasted I've what From",
list0_cons(
"fire. favor who those with hold I",
list0_cons(
" ",
list0_cons(
"... elided paragraph last ...",
list0_cons(
" ",
list0_cons(
"Frost Robert -----------------------",
list0_nil))))))))))


(* ****** MAIN ****** *)
//
implement main0 () =
{

val _ = println!("Testing reverse_words.dats ...\n")

val s0 = "Hey you, Bub! "
val s1 = frost()

// map reverse_words to list of poem lines
val s2 = map_to_list0<string><string>(s1, reverse_words)

val () = (print( s0 + "->"); println!(reverse_words(s0)))
val () = assertloc(reverse_words(s0) = " Bub! you, Hey ")
val () = println! ""
val () = println_list0_string(s2)

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
