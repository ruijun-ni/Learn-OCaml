(*
  Tests for CSci 2041 Computer Laboratory 1
  James Moen
  25 Jan 21

  This is worth 30 points.
*)

 open List

 let rec howMany elem lst =
    if lst = [] then 0
    else
        if elem = (hd lst) then
            1 + howMany elem (tl lst)
        else
            howMany elem (tl lst)
;;

let rec delete elem lst = 
    if lst = [] then
        []
    else
        if elem = hd lst then
            delete elem (tl lst)
        else 
            (hd lst)::(delete elem (tl lst))
;;

let rec mean lst =
    let rec sum lst =
        if lst = [] then 0.0
        else
            (hd lst) +. sum (tl lst)
        in

    let rec length lst =
        if lst = [] then 0.0
        else
            1.0 +. length (tl lst)
        in

    (sum lst) /. (length lst)
;;


open Printf ;;

(* PRINT THINGS. Print a list L. You don't have to know how this works! *)

let printThings f l =
  let rec printingThings l =
    match l with
      [] -> () |
      h :: t -> printf " ; " ; printf f h ; printingThings t
  in printf "[" ;
     (match l with
        [] -> () |
        h :: t -> printf f h ; printingThings t) ;
     printf "]\n" ;;

(* Tests for HOW MANY. *)

printf "%i\n" (howMany 1 []) ;;                      (* 2 pt: 0 *)
printf "%i\n" (howMany 1 [1]) ;;                     (* 2 pt: 1 *)
printf "%i\n" (howMany 2 [1; 2; 3]) ;;               (* 2 pt: 1 *)
printf "%i\n" (howMany 5 [2; 4; 6]) ;;               (* 2 pt: 0 *)
printf "%i\n" (howMany "c" ["c"; "b"; "c"; "d"]) ;;  (* 2 pt: 2 *)
printf "%i\n" (howMany "x" ["a"; "b"; "c"; "d"]) ;;  (* 2 pt: 0 *)

(* Tests for DELETE. *)

printThings "%i" (delete 1 []) ;;                     (* 2 pt: [] *)
printThings "%i" (delete 1 [1]) ;;                    (* 2 pt: [] *)
printThings "%i" (delete 1 [1; 2; 3]) ;;              (* 2 pt: [2 ; 3] *)
printThings "%i" (delete 4 [1; 2; 3]) ;;              (* 2 pt: [1 ; 2 ; 3] *)
printThings "%i" (delete 1 [1; 2; 1; 3; 1; 4]) ;;     (* 2 pt: [2 ; 3 ; 4] *)
printThings "%s" (delete "a" ["x"; "a"; "y"; "a"]) ;; (* 2 pt: [x ; y] *)

(* Tests for MEAN. *)

printf "%f\n" (mean [1.0]) ;;                         (* 2 pts: 1.000000 *)
printf "%f\n" (mean [1.0; 2.0]) ;;                    (* 2 pts: 1.500000 *)
printf "%f\n" (mean [1.0; 0.0; -1.0; 1.0]) ;;         (* 2 pts: 0.250000 *)

(* ********************Test result******************
val howMany : 'a -> 'a list -> int = <fun>
val delete : 'a -> 'a list -> 'a list = <fun>
val mean : float list -> float = <fun>
val printThings : ('a -> 'b, out_channel, unit) format -> 'a list -> unit =
  <fun>
0
- : unit = ()
1
- : unit = ()
1
- : unit = ()
0
- : unit = ()
2
- : unit = ()
0
- : unit = ()
[]
- : unit = ()
[]
- : unit = ()
[2 ; 3]
- : unit = ()
[1 ; 2 ; 3]
- : unit = ()
[2 ; 3 ; 4]
- : unit = ()
[x ; y]
- : unit = ()
1.000000
- : unit = ()
1.500000
- : unit = ()
0.250000
- : unit = () 
**************************************************)


