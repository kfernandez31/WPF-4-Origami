(************************************************************)
(* Author: Kacper Kramarz-Fernandez, index: 429629, gr: IV *)
(* Reviewer: Radosław Maksymiuk, gr: V *)
(************************************************************)


(* supplies `fold_left` *)
open List
;;

type point = float * float
;;

type kartka = point -> int
;;

(* Epsilon constant for float comparison *)
let eps = 1e-9
;;

(* Float square function *)
let sq x =
    x *. x
;;

let prostokat ((a_x, a_y) : point) ((b_x, b_y) : point) : kartka =
    fun (p_x, p_y) ->
        if (a_x -. p_x <= eps) && (p_x -. b_x <= eps) && 
           (a_y -. p_y <= eps) && (p_y -. b_y <= eps) then 1
        else 0
;;

let kolko (p : point) r : kartka =
    let dist_sq (a_x, a_y) (b_x, b_y) = 
        sq (a_x -. b_x) +. sq (a_y -. b_y)
    in fun x ->
        if (dist_sq p x) -. sq r <= eps then 1
        else 0
;;

let zloz (a : point) (b : point) (krtk : kartka) =
    let direction (a_x, a_y) (b_x, b_y) (c_x, c_y) =  (* Returns how point c is in relation to line AB *)
        let dir = (b_y -. a_y) *. (c_x -. b_x) -. (c_y -. b_y) *. (b_x -. a_x) 
        in
            if eps < dir then -1
            else if abs_float dir <= eps then 0
            else 1
            
    in let reflect p (a_x, a_y) (b_x, b_y) = (* Reflects a point across a line *)
        let aux (x, y) (m, c) = 
            if m <> max_float then 
                let h = sq m +. 1.
                in
                    (((1. -. sq m) *. x +. 2. *. m *. y -. 2. *. m *. c) /. h, 
                    ((sq m -. 1.) *. y +. 2. *. m *. x +. 2. *. c) /. h)
            else (x +. 2. *. (c -. x), y)  
        in let make_line (a_x, a_y) (b_x, b_y) =   (* Produces a line y=mx+c represented as (m,c) based on two points that it passes through *)
            if a_x <> b_x then
                let m = (a_y -. b_y) /. (a_x -. b_x)
                in let c = a_y -. m *. a_x
                in (m, c)
            else (max_float, a_x)
        in let line = make_line (a_x, a_y) (b_x, b_y) 
        in aux p line

    in fun p ->
        let cur_dir = direction a b p 
        in
            (*  
                folding the sheet is equivalent to "doubling" the point 
                (checking its position as it is and as it would be on the right side of the sheet) 
            *)
            if cur_dir = 1 then krtk p + krtk (reflect p a b)
            else if cur_dir = 0 then krtk p (* point is on the sheet's border *)
            else 0 (* point is outside (to the right) of the sheet *)
;;

let skladaj lst krtk = 
    let f krtk (p1, p2) = zloz p1 p2 krtk
    in fold_left f krtk lst  
;;
