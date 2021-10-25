(*
  OOP STACK. Object oriented stacks as higher order functions.

    24 Oct 21
*)

(* STACK ERROR. Raised if we try to PEEK or POP an empty STACK. The STRING will
   explain why the error occurred. *)

exception StackError of string ;;

(* STACK LIST. A singly linked linear list containing elements of type BASE. *)

type 'base stackList =
  EmptyStack |
  NonemptyStack of 'base * 'base stackList ;;

(* STACK OPERATION. Operations on STACKs whose elements have type BASE. *)

type 'base stackOperation =
  IsEmpty |
  Peek |
  Pop |
  Push of 'base ;;

(* STACK RESULT. The results of operations on STACKs whose elements have type
   BASE. *)

type 'base stackResult =
  BoolResult of bool |
  BaseResult of 'base |
  NoResult ;;

(* STACK. A STACK whose elements have type BASE. It's actually a function that
   maps STACK OPERATIONs to STACK RESULTs, as the arrow -> shows. *)

type 'base stack =
  'base stackOperation -> 'base stackResult ;;

(* MAKE STACK. Return a function of type STACK. It accepts a STACK OPERATION as
   its argument and returns a STACK RESULT as its value. The list that contains
   the STACK's elements is in the variable TOP. *)

let makeStack () =
  let top = ref EmptyStack
  in

(* IS EMPTY. Test if the STACK is empty. *)

  let isEmpty () =
    BoolResult (! top = EmptyStack)
  in

(* PEEK. Return the top element of the STACK, or raise STACK ERROR. *)

  let peek () =
    match ! top
    with EmptyStack ->
           raise (StackError "Can't PEEK into an empty stack.") |
         NonemptyStack (atTop, _) ->
           BaseResult atTop
  in

(* POP. Delete the STACK's top element, or raise STACK ERROR. *)

  let pop () =
    match ! top
    with EmptyStack ->
           raise (StackError "Can't POP an empty stack.") |
         NonemptyStack (_, underTop) ->
           top := underTop ;
           NoResult
  in

(* PUSH. Add BASE as the STACK's new top element. *)

  let push base =
    top := NonemptyStack (base, ! top) ;
    NoResult
  in

(* DISPATCH. A function that represents a STACK. It executes a STACK OPERATION
   and returns a STACK RESULT. *)

  let dispatch operation =
    match operation
    with IsEmpty ->
           isEmpty () |
         Peek ->
           peek () |
         Pop ->
           pop () |
         Push base ->
           push base

(* Return DISPATCH. It can see the variable TOP, and the functions IS EMPTY,
   PEEK, POP, and PUSH, but the rest of the program can't see them. *)

  in dispatch ;;

(* Examples. We make a STACK S. OCaml doesn't know the type of S's emenents, so
   it uses the weak type '_A as a placeholder for them. ('_A may be something
   like '_WEAK1 on your system.) *)

let s = makeStack () ;;
(* val s : '_a stackOperation -> '_a stackResult = <fun> *)

(* Test if S is empty. It is. OCaml doesn't know the type of S's elements. *)

s IsEmpty ;;
(* - : '_a stackResult = BoolResult true *)

(* Push "C" on S. *)

s (Push "C") ;;
(* - : string stackResult = NoResult *)

(* OCaml now knows that S is a STACK of STRINGs, because we PUSHed the STRING
   "C" on it. The weak type is replaced by STRING. *)

s ;;
(* - : string stackOperation -> string stackResult = <fun> *)

(* Is "C" really on top of S? It is. *)

s Peek ;;
(* - : string stackResult = BaseResult "C" *)

(* A couple more PUSHes. *)

s (Push "B") ;;
(* - : string stackResult = NoResult *)

s (Push "A") ;;
(* - : string stackResult = NoResult *)

(* Now "A" is on top of S. *)

s Peek ;;
(* - : string stackResult = BaseResult "A" *)

(* And of course S is not empty. *)

s IsEmpty ;;
(* -: string stackResult = StackResult false *)

(* Pop "A" off the top of S. *)

s Pop ;;
(* - : string stackResult = NoResult *)

(* Now "B" is on top of S. *)

s Peek ;;
(* - : string stackResult = BaseResult "B" *)

(* Pop "B" off the the top of S. *)

s Pop ;;
(* - : string stackResult = NoResult *)

(* Now "C" is on top of S again. *)

s Peek ;;
(* - : string stackResult = BaseResult "C" *)

(* Pop "C" off the top of S. *)

s Pop ;;
(* - : string stackResult = NoResult *)

(* And now S is empty.*)

s IsEmpty ;;
(* - : string stackResult = BoolResult true *)
