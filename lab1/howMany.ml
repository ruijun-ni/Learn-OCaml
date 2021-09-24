 open List
 let rec howMany elem lst =
    if lst = [] then 0
    else
        if elem = (hd lst) then
            1 + howMany elem (tl lst)
        else
            howMany elem (tl lst)
;;