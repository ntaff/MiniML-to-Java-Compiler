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

let rec chop n l =
	if n = 0 then
		l
	else
		chop (n - 1) (List.tl l)
;;

let rec exec = function
  (PairV (x,y), (PrimInstr (UnOp Fst))::instructionsList, stack, fds) -> exec (x, instructionsList, stack, fds)
| (PairV (x,y), (PrimInstr (UnOp Snd))::instructionsList, stack, fds) -> exec (y, instructionsList, stack, fds)

| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAadd)))::instructionsList, stack, fds) -> exec (IntV (x + y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAsub)))::instructionsList, stack, fds) -> exec (IntV (x - y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAmul)))::instructionsList, stack, fds) -> exec (IntV (x * y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAdiv)))::instructionsList, stack, fds) -> exec (IntV (x / y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAmod)))::instructionsList, stack, fds) -> exec (IntV (x mod y), instructionsList, stack, fds)

| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCeq)))::instructionsList, stack, fds) -> exec (BoolV (x = y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCge)))::instructionsList, stack, fds) -> exec (BoolV (x >= y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCgt)))::instructionsList, stack, fds) -> exec (BoolV (x > y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCle)))::instructionsList, stack, fds) -> exec (BoolV (x <= y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BClt)))::instructionsList, stack, fds) -> exec (BoolV (x < y), instructionsList, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCne)))::instructionsList, stack, fds) -> exec (BoolV (x <> y), instructionsList, stack, fds)

| (x, (Quote v)::instructionsList, stack, fds) -> (v, instructionsList, stack, fds)
| (x, Cons::instructionsList, (Val y)::stack, fds) -> exec (PairV (x, y), instructionsList, stack, fds)

| (x, Push::instructionsList, stack, fds) -> (x, instructionsList, (Val (x))::stack, fds)
| (x, Swap::instructionsList, (Val y)::stack, fds) -> (y, instructionsList, (Val x)::stack, fds)

| (x, (Cur code)::instructionsList, stack, fds) -> (ClosureV (code, x), instructionsList, stack, fds)
| (PairV (ClosureV (code, value), arg), App::instructionsList, stack, fds) -> (PairV (value, arg), code, (Cod instructionsList)::stack, fds)
| (x, Return::instructionsList, (Cod newinstructionsList)::stack, fds) -> (x, newinstructionsList, stack, fds)

| (BoolV (true), (Branch (t, e))::instructionsList, (Val x)::stack, fds) -> (x, t, (Cod instructionsList)::stack, fds)
| (BoolV (false), (Branch (t, e))::instructionsList, (Val x)::stack, fds) -> (x, e, (Cod instructionsList)::stack, fds)

| (x, (Call (f))::instructionsList, stack, fds) -> (x, (List.assoc f fds)@instructionsList, stack, fds)
| (x, (AddDefs (defs))::instructionsList, stack, fds) -> (x, instructionsList, stack, defs@fds)
| (x, (RmDefs(n))::instructionsList, stack, fds) -> (x, instructionsList, stack, chop n fds)

| config -> config
;;


let rec access (v : var)  = function
  x::envt ->
	if v = x then
		[PrimInstr (UnOp (Snd))]
	else
		(PrimInstr (UnOp (Fst)))::(access v envt)
| _ -> failwith "Undefinited variable"
;;

let rec compile env = function
  Bool(b) -> [Quote(BoolV(b))]
| Int(i) -> [Quote(IntV(i))]
| Var(v) -> (access v env)
| Pair (e1, e2) -> [Push] @ (compile env e1) @ [Swap] @ (compile env e2) @ [Cons]
| App (PrimOp (p), e) -> (compile env e) @ [PrimInstr (p)]
| Fn (v, e) -> [Cur ((compile (v::env) e) @ [Return])]
| App (f, a) -> [Push] @ (compile env f) @ [Swap] @ (compile env a) @ [Cons; App]
| Cond (i, t, e) -> [Push] @ (compile env i) @ [Branch ((compile env t) @ [Return], (compile env e) @ [Return])]
| Fix (_, _) -> failwith "Not implemented."
| _ -> failwith "Syntax error"
;;

let compile_prog = function
	Prog (_, t) -> compile [] t
;;


(*Convertit une liste d'instruction en chaîne de caractères*)
let rec print_gen_class_to_java_aux = function
    (PrimInstr(UnOp(op)))::configs -> "\nLLE.add_elem("^
                                        (match op with
                                            Fst -> "new Fst()"
                                            |Snd -> "new Snd()")^","^(print_gen_class_to_java_aux configs)^")"

    |(PrimInstr(BinOp(BArith(op))))::configs -> "\nLLE.add_elem(new BinOp(BinOp.operateur."^
                                        (match op with
                                            BAadd -> "Add"
                                            |BAsub -> "Sub"
                                            |BAmul -> "Mult"
                                            |BAdiv -> "Div"
                                            |BAmod -> "Mod")^"),"^(print_gen_class_to_java_aux configs)^")"

    |(PrimInstr(BinOp(BCompar(op))))::configs -> "\nLLE.add_elem(new BinOp(BinOp.operateur."^
                                        (match op with
                                            BCeq -> "Eq"
                                            |BCge -> "Ge"
                                            |BCgt -> "Gt"
                                            |BCle -> "Le"
                                            |BClt -> "Lt"
                                            |BCne -> "Ne")^"),"^(print_gen_class_to_java_aux configs)^")"

    | Cons::configs -> "\nLLE.add_elem(new Cons(),"^(print_gen_class_to_java_aux configs)^")"
    | Push::configs -> "\nLLE.add_elem(new Push(),"^(print_gen_class_to_java_aux configs)^")"
    | App::configs -> "\nLLE.add_elem(new App(),"^(print_gen_class_to_java_aux configs)^")"
    | Return::configs -> "\nLLE.add_elem(new Return(),"^(print_gen_class_to_java_aux configs)^")"
    | Swap::configs -> "\nLLE.add_elem(new Swap(),"^(print_gen_class_to_java_aux configs)^")"
    | (Quote v)::configs -> "\nLLE.add_elem(new Quote("^(print_values v)^"),"^(print_gen_class_to_java_aux configs)^")"
    | (Cur c)::configs -> "\nLLE.add_elem(new Cur("^(print_gen_class_to_java_aux c)^"),"^(print_gen_class_to_java_aux configs)^")"
    | (Branch(c1,c2))::configs -> "\nLLE.add_elem(new Branch("^(print_gen_class_to_java_aux c1)^","^(print_gen_class_to_java_aux c2)^"),"^(print_gen_class_to_java_aux configs)^")"
    | (Call v)::configs -> "\nLLE.add_elem(new Call(\""^v^"\"),"^(print_gen_class_to_java_aux configs)^")"
    | (AddDefs(defs))::configs -> "\nLLE.add_elem(new AddDefs("^(print_defs defs)^"),"^(print_gen_class_to_java_aux configs)^")"
    | (RmDefs(n))::configs -> "\nLLE.add_elem(new RmDefs("^(string_of_int n)^"), "^(print_gen_class_to_java_aux configs)^")"
    | [] -> "LLE.empty()"
(*Convertit les valeurs en chaînes de caractères*)
and print_values = function
    NullV -> "new NullV()"
    | (IntV v) -> "new IntV("^(string_of_int v)^")"
    | (BoolV b) -> "new BoolV("^(string_of_bool b)^")"
    | (PairV(x,y)) -> "new PairV("^(print_values x)^","^(print_values y)^")"
    | (ClosureV(c,v)) -> "new ClosureV("^(print_gen_class_to_java_aux c)^","^(print_values v)^")"
and print_defs = function
    (v,c)::defs -> "LLE.add_elem(new Pair(\""^v^"\","^(print_gen_class_to_java_aux c)^"), "^(print_defs defs)^")"
| [] -> "LLE.empty()";;

let print_gen_class_to_java = function
    cfg -> "import java.util.*;"^"\n\n"
            ^"public class Gen {"^"\n"
            ^"public static LinkedList<Instr> code ="
            ^(print_gen_class_to_java_aux cfg)^";\n"
            ^"}"
;;
