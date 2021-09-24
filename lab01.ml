
let num = fst ;; 
let den = snd ;;



let rec gcd i j = 
    if i <> 0 
    then if j > i 
        then gcd i (j - i) 
        else gcd (i - j) j 
    else j ;;


let rat n d =
    ((n / (gcd n d)), (d / (gcd n d))) ;;

let ratAdd a b =
    rat ((num a) * (den b) + (den a) * (num b)) ((den a) * (den b));;

let ratMul a b =
    rat ((num a) * (num b)) ((den a) * (den b));;

let ratDiv a b =
    rat ((num a) * (den b)) ((den a) * (num b));;

let ratGt a b = 
    ((num a) * (den b)) > ((den a) * (num b));;



let euler () =
    let rec eulering s t c =
        if (ratGt (rat 1 100000) t)
        then s
        else let s = ratAdd s t
            in let t = ratDiv t c
                in let c = ratAdd c (rat 1 1)
                    in eulering s t c
        in eulering (rat 0 1) (rat 1 1) (rat 1 1)
;;
