open Lib.Ls


let lul = "kitten"
let lol = "sitting"


let x = make_str_matrix lol lul;;
x.(1).(1) <- lsd_calc_min x 1 1 lul lol;;
print_matrix x;;
