(*

  TAUTOLOGY. A brute-force tautology checker for propositional logic.

    James Moen
    17 Oct 21

  This is the tautology checker that was discussed in the last few CSci 2041
  lectures, with a few trivial changes. It includes some code that you may not
  have seen before.

*)

open List   ;;  (* List operations. *)
open Printf ;;  (* Functions that print things. *)

(* AL GET. Return the value of KEY from the association list PAIRS. We raise an
   exception NO SUCH KEY if KEY is not a key in PAIRS. *)

exception NoSuchKey ;;

let alGet pairs key =
  let rec alGetting pairs =
    match pairs
    with [] ->
           raise NoSuchKey |
         (otherKey, otherValue) :: otherPairs ->
           if key = otherKey
           then otherValue
           else alGetting otherPairs
  in alGetting pairs ;;

(* AL PUT. Return a new association list that's like PAIRS, but which maps KEY
   to VALUE. We assume KEY is not already a key in PAIRS, though AL PUT will
   still work if it is. *)

let alPut pairs key value =
  (key, value) :: pairs ;;

(* PROPOSITION. An expression in propositional logic. *)

type proposition =
  False |                                (* false *)
  True |                                 (* true *)
  Var of string |                        (* a, b, c ... *)
  Not   of proposition |                 (* ¬ R *)
  And   of proposition * proposition |   (* L ∧ R *)
  Or    of proposition * proposition |   (* L ∨ R *)
  Imply of proposition * proposition |   (* L → R *)
  Equiv of proposition * proposition ;;  (* L ↔ R *)

(* For example, this PROPOSITION represents (a ∧ b) ↔ ¬ (¬ a ∨ ¬ b), which is a
   version of DE MORGAN'S law. We'll use it later. *)

let deMorgan's =
  Equiv (
   And (Var "a", Var "b"),
   Not (Or (Not (Var "a"), Not (Var "b")))) ;;

(* EVALUATE. Evaluate a PROPOSITION to an OCaml Boolean, given the bindings of
   its variable names in PAIRS. Raise NO SUCH KEY if we find a variable name
   that isn't in PAIRS. *)

let evaluate proposition pairs =
  let rec evaluating proposition =
    match proposition
    with False ->
           false |
         True ->
           true |
         Var name ->
           alGet pairs name |
         Not right ->
           not (evaluating right) |
         And (left, right) ->
           evaluating left && evaluating right |
         Or (left, right) ->
           evaluating left || evaluating right |
         Imply (left, right) ->
           not (evaluating left) || evaluating right |
         Equiv (left, right) ->
           evaluating left = evaluating right
  in evaluating proposition ;;

(* PRINT BOOLS. Print a list of OCaml Booleans BOOLS as a series of T's and F's
   followed by a newline. *)

let rec printBools bools =
  match bools
  with [] ->
         printf "\n" |
       firstBool :: otherBools ->
         printf "%s" (if firstBool then "T" else "F") ;
         printBools otherBools ;;

(* For example, this expression prints FFT. *)

printBools [false ; false ; true] ;;

(* PRINT PAIRS. Print an association list PAIRS that associates STRINGs with
   BOOLs, followed by a newline. STRINGs are printed without quotes and BOOLs
   are printed as T's or F's. *)

let rec printPairs pairs =
  match pairs
  with [] ->
         printf "\n" |
       (key, value) :: pairs ->
         printf "%s → %s " key (if value then "T" else "F") ;
         printPairs pairs ;;

(* For example, this expression prints c → T b → F a → T. *)

let l = alPut [] "a" true
in let l = alPut l "b" false
   in let l = alPut l "c" true
      in printPairs l ;;

(* We'll now begin developing some generators that produce all possible lists
   of FALSEs and TRUEs. Each time one of them generates such a list, it calls
   its continuation ETC on that list. *)

(* GENERATE BOOLS. Generate all possible lists of BOOLs, whose length is N. We
   assume N ≥ 0. *)

let generateBools etc n =
  let rec generatingBools bools n =
    match n
    with 0 ->
           etc bools |
         _ ->
           generatingBools (false :: bools) (n - 1) ;
           generatingBools (true  :: bools) (n - 1)
  in generatingBools [] n ;;

(* For example, this expression prints FFF, TFF, FTF, TTF, FFT, TFT, FTT, and
   TTT. *)

generateBools printBools 3 ;;

(* GENERATE PAIRS. We'll change GENERATE BOOLS slightly so it makes association
   lists. Generate all possible association lists that associate STRINGs in the
   list NAMES with BOOLs. *)

let generatePairs etc names =
  let rec generatingPairs names pairs =
    match names
    with [] ->
           etc pairs |
         name :: names ->
           generatingPairs names (alPut pairs name false) ;
           generatingPairs names (alPut pairs name true)
  in generatingPairs names [] ;;

(* For example, this expression prints:

c → F b → F a → F
c → T b → F a → F
c → F b → T a → F
c → T b → T a → F
c → F b → F a → T
c → T b → F a → T
c → F b → T a → T
c → T b → T a → T

*)

generatePairs printPairs ["a" ; "b" ; "c"] ;;

(* GENERATE AND TEST PAIRS. We'll change GENERATE PAIRS slightly again. Not it
   not only generates association lists, but also tests if each generated list
   makes its continuation ETC return TRUE. All we did is change the function's
   name, and change its ";" to "&&". *)

let generateAndTestPairs etc names =
  let rec generatingAndTestingPairs names pairs =
    match names
    with [] ->
           etc pairs |
         name :: names ->
           generatingAndTestingPairs names (alPut pairs name false) &&
           generatingAndTestingPairs names (alPut pairs name true)
  in generatingAndTestingPairs names [] ;;

(* We need a function that returns a list of all STRING names in a PROPOSITION.
   To start, we'll define some helpers. *)

(* IS MEMBER. Test if THING is a member of the list THINGS. *)

let isMember thing things =
  let rec isMembering things =
    match things
    with [] ->
           false |
         firstThing :: otherThings ->
           (thing = firstThing) || (isMembering otherThings)
  in isMembering things ;;

(* UNION. Assume that LEFT THINGS and RIGHT THINGS are lists in which no member
   appears more than once. Return a list BOTH THINGS, containing all members of
   LEFT THINGS and RIGHT THINGS, in which no member appears more than once. The
   order of THINGS in BOTH THINGS doesn't matter. *)

let union leftThings rightThings =
  let rec unioning bothThings rightThings =
    match rightThings
    with [] ->
           bothThings |
         rightThing :: rightThings ->
           if isMember rightThing bothThings
           then unioning bothThings rightThings
           else unioning (rightThing :: bothThings) rightThings
  in unioning leftThings rightThings ;;

(* NAMES. Return a list of STRING names in PROPOSITION. No STRING appears more
   than once in the list. The order of STRINGs in the list doesn't matter. *)

let rec names proposition =
  match proposition
  with False |
       True ->
        [] |
      Var name ->
        [name] |
      Not right ->
        names right |
      And (left, right) |
      Or (left, right) |
      Imply (left, right) |
      Equiv (left, right) ->
        union (names left) (names right) ;;

(* For example, this returns ["b" ; "a"]. *)

names deMorgan's ;;

(* The big finish! *)

(* IS TAUTOLOGY. Test if PROPOSITION is a tautology. *)

let isTautology proposition =
  generateAndTestPairs
   (fun pairs -> evaluate proposition pairs)
   (names proposition) ;;

(* For example, this returns TRUE. *)

isTautology deMorgan's ;;

(* But p ∧ q isn't a tautology, so this returns FALSE. *)

isTautology (And (Var "p", Var "q")) ;;
