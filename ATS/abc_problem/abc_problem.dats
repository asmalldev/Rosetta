(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** abc_problem.dats
** given a collection of 20 ABC blocks with 2 letters each,
** determines if a word can be made using each block at most once
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

// returns uppercase version of str
fn toupper_string(str: string): string = let
    val l = string_explode(str)
    val l = list0_map<char><char>(l, lam(x) => toupper_char(x))
in
    string_implode(l)
end

// checks if any tuple in list contains element
fun {a: t@ype} list0_contains_intup(l0: list0(@(a,a)), i: a): bool = let
    fun aux(l0: list0(@(a,a)), item: a) =
    (
    case l0 of
    | list0_nil() => false
    | list0_cons((x,y), xs) =>
        if (geq_val_val<a>(x,i) || geq_val_val<a>(y,i))
            then true
        else aux(xs,i)
    )
in
    aux(l0,i)
end

// removes item in list if it contains tuple with char i
fun {a: t@ype} list0_remove_intup(l0: list0(@(a,a)), i: a): list0(@(a,a)) = let
    fun aux(l0: list0(@(a,a)), item: a) =
    (
    case l0 of
    | list0_nil() => list0_nil()
    | list0_cons((x,y), xs) =>
        if (geq_val_val<a>(x,i) || geq_val_val<a>(y,i))
            then xs
        else list0_cons((x,y), aux(xs,i))
    ) 
in
    aux(l0,i)
end

// returns a list of char tuples representing letter blocks
fun blocks(): list0(@(char,char)) = let
val blocklist = split(
    "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC SM", ' ')
in
    list0_map<string><(char,char)>(blocklist, lam(x) =>
        let
            val s = string_explode(x)
        in
            (s[0],s[1])
        end
    )
end

// determines if a word is constructible using the set of blocks
fun constructible(word: string): bool = let
    val word = toupper_string(word)
    val letters = string_explode(word)
    val blocks = blocks()
    fun aux(letters: list0(char), blocks: list0(@(char,char))): bool =
        if list0_is_empty(letters) then true else
            if list0_is_empty(blocks) then false else
                if list0_contains_intup<char>(blocks, list0_head_exn(letters)) then
                    aux(list0_tail_exn(letters), list0_remove_intup<char>(blocks, list0_head_exn(letters)))
                else
                    false
in
    aux(letters, blocks)
end

//
(* ****** ****** *)


(* ****** MAIN ****** *)
//
implement main0 () = 
{

val _ = println!("Testing abc_problem.dats ...\n")

val s1 = "a"
val s2 = "BaRk"
val s3 = "TreAt"
val s4 = "squAd"
val s5 = "conFuSE"

val s6 = "BooK"
val s7 = "ComMoN"

// should be true
val () = (print("\t" + s1 + ":\t\t"); println!(constructible(s1)))
val () = assertloc(constructible(s1))
val () = (print("\t" + s2 + "\t\t"); println!(constructible(s2)))
val () = assertloc(constructible(s2))
val () = (print("\t" + s3 + ":\t\t"); println!(constructible(s3)))
val () = assertloc(constructible(s3))
val () = (print("\t" + s4 + ":\t\t"); println!(constructible(s4)))
val () = assertloc(constructible(s4))
val () = (print("\t" + s5 + ":\t"); println!(constructible(s5)))
val () = assertloc(constructible(s5))

// should be false
val () = (print("\t" + s6 + ":\t\t"); println!(constructible(s6)))
val () = assertloc(not(constructible(s6)))
val () = (print("\t" + s7 + ":\t\t"); println!(constructible(s7)))
val () = assertloc(not(constructible(s7)))

val _ = println!("\nAll tests passed!")

}
//
(* ****** ****** *)
