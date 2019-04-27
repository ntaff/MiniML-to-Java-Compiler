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
  (PairV (x,y), (PrimInstr (UnOp Fst))::instructionsList, stack) -> exec (x, instructionsList, stack)
| (PairV (x,y), (PrimInstr (UnOp Snd))::instructionsList, stack) -> exec (y, instructionsList, stack)

| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAadd)))::instructionsList, stack) -> exec (IntV (x + y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAsub)))::instructionsList, stack) -> exec (IntV (x - y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAmul)))::instructionsList, stack) -> exec (IntV (x * y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAdiv)))::instructionsList, stack) -> exec (IntV (x / y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAmod)))::instructionsList, stack) -> exec (IntV (x mod y), instructionsList, stack)

| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCeq)))::instructionsList, stack) -> exec (BoolV (x = y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCge)))::instructionsList, stack) -> exec (BoolV (x >= y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCgt)))::instructionsList, stack) -> exec (BoolV (x > y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCle)))::instructionsList, stack) -> exec (BoolV (x <= y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BClt)))::instructionsList, stack) -> exec (BoolV (x < y), instructionsList, stack)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCne)))::instructionsList, stack) -> exec (BoolV (x <> y), instructionsList, stack)

| (x, (Quote v)::instructionsList, stack) -> (v, instructionsList, stack)
| (x, Cons::instructionsList, (Val y)::stack) -> exec (PairV (x, y), instructionsList, stack)

| (x, Push::instructionsList, stack) -> (x, instructionsList, (Val (x))::stack)
| (x, Swap::instructionsList, (Val y)::stack) -> (y, instructionsList, (Val x)::stack)

| (x, (Cur code)::instructionsList, stack) -> (ClosureV (code, x), instructionsList, stack)
| (PairV (ClosureV (code, value), arg), App::instructionsList, stack) -> (PairV (value, arg), code, (Cod instructionsList)::stack)
| (x, Return::instructionsList, (Cod newinstructionsList)::stack) -> (x, newinstructionsList, stack)

| (BoolV (true), (Branch (t, e))::instructionsList, (Val x)::stack) -> (x, t, (Cod instructionsList)::stack)
| (BoolV (false), (Branch (t, e))::instructionsList, (Val x)::stack) -> (x, e, (Cod instructionsList)::stack)

| config -> config
;;
