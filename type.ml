type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

(* get type name *)
let get_typ_name = function
  | Unit -> "u"
  | Bool -> "b"
  | Int -> "i"
  | Float -> "d"
  | Fun _ -> "f"
  | Tuple _ -> "t"
  | Array _ -> "a"
  | Var _ -> "v"
