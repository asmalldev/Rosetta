(*
**
** John A Small
** CS 320, Summer 2017
** Professor Hongwei Xi
**
** bulls_and_cows.dats
** implements the old game "bulls and cows" with the following rules:
**  1) the player must try to guess a 4 digit number made up of 1 - 9 (no dups)
**  2) if the guess is right, the player wins
**  3) a bull is given for each digit that matches
**  4) a cow is given for each digit that is present but in the wrong position
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

(* ****** DEFINITIONS ****** *)
//
exception Malformed of (string)
//
(* ****** ****** *)

(* ****** FUNCTIONS ****** *)
//

// checks if list contains element
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

// validates input (raises exception upon failure)
// input must not have duplicates, only contain 1-9, and be 4 digits long
fn validate(str: string): void =
let
    (*val digits = list0_make_intrange(1, 10)
    val digits = list0_map<int><char>
        (digits, lam(x) => int2digit(x))*)
    val digits = list0_cons('1',list0_cons('2',list0_cons('3',list0_cons('4',
                list0_cons('5',list0_cons('6',list0_cons('7', list0_cons('8',
                list0_cons('9', list0_nil())))))))))
    fun check_digits(l0: list0(char)): void =
        (
        case l0 of
        | list0_nil() => ()
        | list0_cons(x, xs) => if not(list0_contains<char>(digits, x)) then
                $raise Malformed("\nInvalid guess: digits must be from 1 through 9.")
            else check_digits(xs)
        )
    fn check_duplicates(l0: list0(char)): void = let
        fun aux(seen: list0(char), l0: list0(char)): void =
            (
            case l0 of
            | list0_nil() => ()
            | list0_cons(x, xs) => if list0_contains<char>(seen, x) then
                $raise Malformed("\nInvalid guess: digits must be unique.")
                else aux(list0_extend(seen, x), xs)
            )
    in
        aux(list0_nil, l0)
    end
in
    let
        val _ = check_digits(string_explode(str))
        val _ = check_duplicates(string_explode(str))
    in
        if ((length(str)) != 4) then
            $raise Malformed("\nInvalid guess: please input 4 digits.")
        else ()
    end
end

// given a guess and the actual number (represented as strings)
// returns the number of bulls (correctly placed digits)
fn count_bulls(guess: string, actual: string): int = let
    val guess = string_explode(guess)
    val actual = string_explode(actual)
    fun aux(bulls: int, pos: int): int =
        if (pos >= 4) then bulls // guarenteed to be 4 digits
        else
            if (guess[pos] = actual[pos]) then
                aux(bulls +1, pos +1)
            else
                aux(bulls, pos +1)
in
    aux(0, 0)
end

// given a guess and the actual number (represented as strings)
// returns the number of cows (wrongly placed digits)
fn count_cows(guess: string, actual: string): int = let
    val guess = string_explode(guess)
    val actual = string_explode(actual)
    fun aux(cows: int, pos:int): int =
            if (pos >= 4) then cows  // guarenteed to be 4 digits
            else
                if guess[pos] = actual[pos] then
                    aux(cows, pos +1)
                else
                    if list0_contains<char>(actual, guess[pos])
                        then aux(cows +1, pos +1)
                    else
                        aux(cows, pos +1)
in
    aux(0, 0)
end

// gets a random integer between x, n using unsafe function/ c-style
fun random_int_btw(x: int, n: int) = let
    val _ = $STDLIB.srand($UN.cast {uint} ($TIME.time_get()))
    val r = $STDLIB.rand()
    val r = r mod n
in
    if r < x then random_int_btw(x, n) else r
end

// generates a string of 4 digits that fits the bulls and cows requirements
fn generate_digits(): string = let
    fun build(seen: list0(char)): string =
        let
            val r = tostring_int(random_int_btw(1, 10))
            val r = string_explode(r)
            val r = r[0]
        in
            if length(seen) = 4 then string_implode(seen)
            else
                if list0_contains<char>(seen, r) then build(seen)
                else build(list0_extend(seen,r))
        end 
in
    build(list0_nil())
end

implement
main0() =
let
    val _ = println!("Generating digits...")
    val generated = generate_digits()
    val _ = println!("I'm thinking of a number whose digits are between 1 and 9, with no duplicates, what is it?")
    fun loop(i:int): void =
    let
        val _ = print("Enter Guess: ")
        val noteof = fileref_isnot_eof(stdin_ref)
    in
        if noteof then
            try
            let
                val line = fileref_get_line_string(stdin_ref)
                val _ = validate(line)
            in
                if strcmp(line, generated) = 0 then
                    let
                        val () = println!("That's it, you win! It took you " +
                                 tostring_int(i) + " tries.")
                    in
                        // nothing
                    end
                else
                    let
                        val () = println!("Nope, that's not it.")
                        val () =println!("Bulls: " +
                                tostring_int(count_bulls(line, generated)))
                        val () = println!("Cows:  " +
                                tostring_int(count_cows(line, generated)) + "\n")
                    in
                        loop(i+1)
                    end
            end
            with ~Malformed(str) => (println!(str); loop(i))
        else()
    end
in
    loop(0)
end
