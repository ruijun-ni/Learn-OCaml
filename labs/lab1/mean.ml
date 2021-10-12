open List
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
