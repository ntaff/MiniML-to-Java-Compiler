(* Instructions of the CAM *)

open Miniml;;

type instr =
  PrimInstr of primop
| Cons
| Push
| Swap
| Return
| Quote of value
| Cur of code
| App
| Branch of code * code
(* new for recursive calls *)
| Call of var
| AddDefs of (var * code) list
| RmDefs of int
and value =
  NullV 
| VarV of Miniml.var
| IntV of int
| BoolV of bool
| PairV of value * value
| ClosureV of code * value
and code = instr list
  
type stackelem = Val of value | Cod of code


let rec exec = function
  (PairV (x,y), (PrimInstr (UnOp Fst))::inslist, stack) -> exec (x, inslist, stack)
| (PairV (x,y), (PrimInstr (UnOp Snd))::inslist, stack) -> exec (y, inslist, stack)

| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAadd)))::inslist, stack) -> exec (IntV (x + y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAsub)))::inslist, stack) -> exec (IntV (x - y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAmul)))::inslist, stack) -> exec (IntV (x * y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAdiv)))::inslist, stack) -> exec (IntV (x / y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAmod)))::inslist, stack) -> exec (IntV (x mod y), inslist, stack)

| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCeq)))::inslist, stack) -> exec (BoolV (x = y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCge)))::inslist, stack) -> exec (BoolV (x >= y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCgt)))::inslist, stack) -> exec (BoolV (x > y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCle)))::inslist, stack) -> exec (BoolV (x <= y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BClt)))::inslist, stack) -> exec (BoolV (x < y), inslist, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCne)))::inslist, stack) -> exec (BoolV (x <> y), inslist, stack)

| (x, (Quote v)::inslist, stack) -> (v, inslist, stack)
| (x, Cons::inslist, (Val y)::stack) -> exec (PairV (x, y), inslist, stack)

| (x, Push::inslist, stack) -> (x, inslist, (Val (x))::stack)
| (x, Swap::inslist, (Val y)::stack) -> (y, inslist, (Val x)::stack)

| (x, (Cur code)::inslist, stack) -> (ClosureV (code, x), inslist, stack)
| (PairV (ClosureV (code, value), arg), App::inslist, stack) -> (PairV (value, arg), code, (Cod inslist)::stack)
| (x, Return::inslist, (Cod newInsList)::stack) -> (x, newInsList, stack)

| (BoolV (true), (Branch (t, e))::inslist, (Val x)::stack) -> (x, t, (Cod inslist)::stack)
| (BoolV (false), (Branch (t, e))::inslist, (Val x)::stack) -> (x, e, (Cod inslist)::stack)

| config -> config
;;
