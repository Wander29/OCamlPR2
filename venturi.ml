type ide = string;;
(*espressioni: ciò che l'utente può usare per utilizzare il linguaggio interpreato qui definito*)
type exp = Eint of int 	| Ebool of bool | Den of ide 	| Prod of exp * exp | Sum of exp * exp | Diff of exp * exp |
	Eq of exp * exp 	| Minus of exp 	| IsZero of exp | Or of exp * exp 	| And of exp * exp | Not of exp |
	Ifthenelse of exp * exp * exp 		| Let of ide * exp * exp 			| Fun of ide * exp | FunCall of exp * exp |
	Letrec of ide * exp * exp 			| Dict of (ide * exp)list 			| Insert of ide * exp * exp |
	Delete of  ide * exp 				| HasKey of ide * exp				| Iterate of exp * exp	|
	Fold of exp * exp					| Filter of ide list * exp  		| PrintDictValues of exp;;

(*ambiente polimorfo: nel nostro caso di tipo evT*)
type 't env = ide -> 't;;	(*l'ambiente è definito come una funzione!*)
let emptyenv (v : 't) : 't env = function x -> v;;
(*ambiente vuoto: funzione che restituisce sempre il valore v per ogni input*)
let applyenv (r : 't env) (i : ide) = r i;;
(*applyenv: applica la funzione ambinente, ovvero dato un identificatore restituisce il valore cui è legato*)
(*ambiente: pila di funzioni 'congelate' necessarie per risalire alla ricerca del legame ide-valore*)
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;
(*bind: aggiunge la funzione definita sopra alla pila delle chiamate che vanno a formare l'ambiente*)

(*tipi esprimibili*)
type evT = 	Int of int | Bool of bool | Unbound | FunVal of evFun | RecFunVal of ide * evFun |
			DictVal of (ide * evT)list 
and evFun = ide * exp * evT env

(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
	"int" -> (match v with
		Int(_) 	-> true |
		_ 		-> false) |
	"bool" -> (match v with
		Bool(_) -> true |
		_ 		-> false) |
	_ -> failwith("not a valid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n*u))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n+u))
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n-u))
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Bool(n=u))
	else if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(n), Bool(u)) -> Bool(n=u))
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then (match x with
	   	Int(n) -> Int(-n))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
		Int(n) -> Bool(n=0))
	else failwith("Type error");;

let __or x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> (Bool(b||e)))
	else failwith("Type error");;

let __and x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> Bool(b&&e))
	else failwith("Type error");;

let __not x = if (typecheck "bool" x)
	then (match x with
		Bool(true) -> Bool(false) |
		Bool(false) -> Bool(true))
	else failwith("Type error");;



(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
	Eint n -> Int n |
	Ebool b -> Bool b |
	IsZero a -> iszero (eval a r) |
	Den i -> applyenv r i |
	Eq(a, b) -> eq (eval a r) (eval b r) |
	Prod(a, b) -> prod (eval a r) (eval b r) |
	Sum(a, b) -> sum (eval a r) (eval b r) |
	Diff(a, b) -> diff (eval a r) (eval b r) |
	Minus a -> minus (eval a r) |
	And(a, b) -> __and (eval a r) (eval b r) |
	Or(a, b) -> __or (eval a r) (eval b r) |
	Not a -> __not (eval a r) |
	Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard") |
	Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) |
	Fun(i, a) -> FunVal(i, a, r) |
	FunCall(f, eArg) -> 
		let fClosure = (eval f r) in
			(match fClosure with
				FunVal(arg, fBody, fDecEnv) -> 
					eval fBody (bind fDecEnv arg (eval eArg r)) |
				RecFunVal(g, (arg, fBody, fDecEnv)) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv |
				_ -> failwith("non functional value")) |
    Letrec(f, funDef, letBody) ->
    		(match funDef with
        		Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                     			                eval letBody r1 |
        		_ -> failwith("non functional def")) |
	Dict(li) -> DictVal(evalDict li r) |
	Iterate(f, d) -> (match eval d r with
						DictVal(l) -> DictVal(iterateDict l f r) |
						_ -> failwith("Not a Dictionary")) | 
	PrintDictValues(d) -> (match eval d r with
							DictVal(l) -> printDictValues l |
							_ -> failwith("non dict"))


and  evalDict (l : (ide * exp) list ) (rr : evT env) : (ide * evT)list = match l with
		[] -> [] |
		(i, e)::t -> (i, (eval e rr))::(evalDict t rr)

and iterateDict (l : (ide * evT)list ) (f : exp) (r : evT env) : (ide * evT)list = match l with
		[] -> [] |
		(i, v)::t -> match eval f r with
						FunVal(arg, body, r_static) -> let x = eval body (bind r_static arg v) in 
									(i, x)::(iterateDict t f r) |
						_ -> failwith("Not a fuction to iterate with")
and printDictValues (l : (ide * evT)list ) = match l with
		[] -> Unbound |
		(ide, v)::t -> match v with
				Int(u) -> Printf.printf "%s, %i; " ide u;
		printDictValues t;;
	
let int_of_evT (x : evT) = match x with
	| Int(u) -> u
	| _ -> failwith("Tipo non intero");;
(* =============================  TESTS  ================= *)

let env0 : ide -> evT = emptyenv Unbound;;

(*

eval (PrintDictValues (Dict([("gatto", Eint(3));("micio", Eint 45);("micetti belli", Eint(17))]))) env0;;

eval (PrintDictValues(Let("x", Fun("y", Prod(Den "y", Eint 2)), Iterate(Den "x", Dict([("gatto", Eint(3));("micino", Eint(45))]))))) env0;;

let e1 = FunCall(Fun("y", Sum(Den "y", Eint 1)), Eint 3);;


*)