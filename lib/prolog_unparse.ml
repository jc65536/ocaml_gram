open CCFormat
open Syntax

let pp_pl_list pp =
  CCList.pp ~pp_start:(return "[@,") ~pp_stop:(return "]") pp

let rec pp_expr f = function
  | EVar v -> fprintf f "var(%a)" string v
  | EInt i -> fprintf f "int(%a)" int i
  | EBool b -> fprintf f "bool(%a)" bool b
  | EIfThenElse (c, a, b) ->
      fprintf f "@[<v 2>if(%a,@ %a,@ %a)@]" pp_expr c pp_expr a pp_expr b
  | EList l -> fprintf f "@[<hv 2>list(@,%a@,)@]" (pp_pl_list pp_expr) l
  | ETuple t -> fprintf f "@[<hv 2>tuple(%a)@]" (pp_pl_list pp_expr) t
  | EAbs (p, e) ->
      fprintf f "@[<hv 2>@[<hv 2>fun(%a@],@ %a)@]" pp_patt p pp_expr e
  | EBop (a, o, b) ->
      fprintf f "@[bop(@,%a,@ %a,@ %a@,)@]" pp_expr a pp_bop o pp_expr b
  | ECons (x, xs) ->
      fprintf f "@[<hv 2>cons(@,%a,@ %a@])" pp_expr x pp_expr xs
  | ELet (r, p, e, b) ->
      fprintf f "@[<hov 2>let(%a,@ %a,@ %a,@ %a@,)@]" bool r pp_patt p pp_expr
        e pp_expr b
  | EMatch (e, pl) ->
      fprintf f "@[<v 2>match(%a, %a)@]" pp_expr e
        (pp_pl_list (pp_match pp_patt pp_expr))
        pl
  | EApp (fn, xs) ->
      fprintf f "@[<hv 2>app(%a,@ %a)@]" pp_expr fn (pp_pl_list pp_expr) xs

and pp_match p e f (a, b) = fprintf f "@[<hv 2>branch(%a,@ %a)@]" p a e b

and pp_patt f = function
  | PVar v -> fprintf f "var(%a)" string v
  | PInt i -> fprintf f "int(%a)" int i
  | PBool b -> fprintf f "bool(%a)" bool b
  | PUnderscore -> fprintf f "any"
  | PTuple t -> fprintf f "@[<hv 2>tuple(@,%a@,)@]" (pp_pl_list pp_patt) t
  | POr (a, b) -> fprintf f "@[or(%a,@ %a)@]" pp_patt a pp_patt b
  | PCons (x, xs) ->
      fprintf f "@[<hv 2>cons(@,%a,@ %a@])" pp_patt x pp_patt xs
  | PList l -> fprintf f "@[list(%a)@]" (pp_pl_list pp_patt) l

and pp_bop f = function
  | BEq -> fprintf f "%a" string_quoted "="
  | BLe -> fprintf f "%a" string_quoted "<"
  | BOr -> fprintf f "%a" string_quoted "||"
  | BAnd -> fprintf f "%a" string_quoted "&&"
  | BPlus -> fprintf f "%a" string_quoted "+"
  | BTimes -> fprintf f "%a" string_quoted "*"

let unparse e = printf "%a\n" pp_expr e
