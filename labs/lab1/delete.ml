open List
let rec delete elem lst = 
    if lst = [] then
        []
    else
        if elem = hd lst then
            delete elem (tl lst)
        else 
            (hd lst)::(delete elem (tl lst))
;;