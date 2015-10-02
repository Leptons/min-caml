type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

(* show Id.t * Type.t *)
let print_id s (a, b) =
    print_string (s^a^": ");
    print_endline (Type.get_typ_name b)

let rec d s e =
    print_string s;
    let s2 = s^"  " in
    match e with
    | Unit -> print_endline ("()")
    | Bool b -> print_endline (if b then ("true") else ("false"))
    | Int i -> (print_int i; print_newline ())
    | Float f -> (print_float f; print_newline ())
    | Not a -> (print_endline "NOT"; d s2 a)
    | Neg a -> (print_endline "NEG"; d s2 a)
    | Add (a, b) -> (print_endline "ADD"; d s2 a; d s2 b)
    | Sub (a, b) -> (print_endline "SUB"; d s2 a; d s2 b)
    | FNeg a -> (print_endline "FNEG"; d s2 a)
    | FAdd (a, b) -> (print_endline "FADD"; d s2 a; d s2 b)
    | FSub (a, b) -> (print_endline "FSUB"; d s2 a; d s2 b)
    | FMul (a, b) -> (print_endline "FMUL"; d s2 a; d s2 b)
    | FDiv (a, b) -> (print_endline "FDIV"; d s2 a; d s2 b)
    | Eq (a, b) -> (print_endline "EQ"; d s2 a; d s2 b)
    | LE (a, b) -> (print_endline "LE"; d s2 a; d s2 b)
    | If (a, b, c) -> (print_endline "IF"; d s2 a; d s2 b; d s2 c)
    | Let ((a, b), c, f) -> (print_string "LET "; print_endline a; d s2 c; d s2 f)
    | Var a -> (print_string "VAR "; print_endline a)
    | LetRec (a, f) -> (print_string "LETREC "; print_id "" a.name; List.iter (print_id s2) a.args; d s2 a.body; d s2 f)
    | App (a, b) -> (print_endline "APP"; d s2 a; List.iter (d s2) b)
    | Tuple a -> (print_endline "TUPLE"; List.iter (d s2) a)
    | LetTuple (a, b, c) -> (print_endline "LETTUPLE"; List.iter (print_id s2) a; d s2 b; d s2 c)
    | Array (a, b) -> (print_endline "ARRAY"; d s2 a; d s2 b)
    | Get (a, b) -> (print_endline "GET"; d s2 a; d s2 b)
    | Put (a, b, c) -> (print_endline "PUT"; d s2 a; d s2 b; d s2 c)
    | _ -> print_newline ()

(* dump Syntax.t *)
let dump e = d "" e
