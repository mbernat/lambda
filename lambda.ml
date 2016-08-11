type exp =
  | Var of string
  | Abs of string * exp
  | App of exp * exp
;;


let const = Abs ("x", Abs ("y", Var "x"));;
let app_const = App (const, Var "y");;
let id = Abs ("x", Var "x") ;;
let app_id = App (id, id) ;;
  
(*
Generate fresh names in f for all free variables in e.
Then replace all occurences of x in f with e.

The reason for this complication is to avoid the variable capture.
So what exactly can be captured?
The free variables of e can become bound in f.
E.g. (\x -> \y -> x) y would evaluate to \y -> y which is incorrect.
We want instead (\x -> \z -> x) y --> (\z -> y)

 *)

let rec elem x = function
  | [] -> false
  | y :: ys -> if x = y then true else elem x ys
;;
  
let rec get_free bound = function
  | Var x -> if (elem x bound) then [] else [x]
  | Abs (x, e) -> get_free (x :: bound) e
  | App (f, e) -> (get_free bound f) @ (get_free bound e)
;;

let rep_var x y z = if x = z then y else z;;
  
let rec replace x y = function
  | Var z -> Var (rep_var x y z)
  | Abs (z, e) -> Abs (rep_var x y z, replace x y e)
  | App (e, f) -> App (replace x y e, replace x y f)
;;

let rec sub_pure x e f =
  match f with
  | Var y -> if x = y then e else Var y
  | Abs (y, f') -> Abs (y, sub_pure x e f')
  | App (f', f'') -> App (sub_pure x e f', sub_pure x e f'')
;;

(* 
BIG TODO generate actual fresh names.
 *)
let sub x e f =
  let free = get_free [] e in
  let fresh = "q" in
  let freshen x e = replace x fresh e in
  let f' = List.fold_right freshen free f in
  sub_pure x e f'
;;

let is_value = function
  | Var _ -> true
  | Abs _ -> true
  | App _ -> false
;;
  
let rec step_by_value = function
  | Var x -> Var x
  | Abs (x, e) -> Abs (x, e)
  | App (Abs (x, f), e) ->
     let e' = eval_by_value e
     in sub x e' f
  | App (f, e) -> App ((step_by_value f), e)
and eval_by_value e =
  if is_value e then e else eval_by_value (step_by_value e)
;;
  
let rec step_by_name = function
  | Var x -> Var x
  | Abs (x, e) -> Abs (x, e)
  | App (Abs (x, f), e) -> sub x e f
  | App (f, e) -> App (step_by_name f, e)
;;
		      
let rec eval_by_name e =
  if is_value e then e else eval_by_name (step_by_name e)
;;

