exception InvalidDictionaryException 	of string;;
exception InvalidFunctionException 		of string;;
exception InvalidArgumentException 		of string;;

(* ####### ESTENSIONE LINGUAGGIO: DIZIONARI #######
	@OVERVIEW:  Un dizionario è una collezione di valori identificati univocamente da una chiave
				ovvero una collezione di coppie chiave-valore dove la chiave è unica.
				Si assume che tutti i valori in un dizionario siano dello stesso tipo.
				Non sono ammessi elementi Unbound(= null); nello specifico sono ammessi 
				solo valori booleani e interi.
*)
type ide = string;;
(*espressioni: ciò che l'utente può usare per utilizzare il linguaggio interpreato qui definito*)
type exp = Eint of int 	| Ebool of bool | Den of ide 	| Prod of exp * exp | Sum of exp * exp | Diff of exp * exp |
	Eq of exp * exp 	| Minus of exp 	| IsZero of exp | Or of exp * exp 	| And of exp * exp | Not of exp |
	Ifthenelse of exp * exp * exp 		| Let of ide * exp * exp 			| Fun of ide * exp | FunCall of exp * exp list |
	Letrec of ide * exp * exp 			| CreateDict of (ide * exp)list 	| Insert of ide * exp * exp |
	Delete of  ide * exp 				| HasKey of ide * exp				| Iterate of exp * exp	|
	Fold of exp * exp					| Filter of ide list * exp  		| PrintDictValues of exp |
	FunArg of ide list * exp;;

type 't env = ide -> 't;;	(*l'ambiente è definito come una funzione!*)
(*ambiente polimorfo: nel nostro caso lo useremo di tipo evT*)
let emptyenv (v : 't) : 't env = function x -> v;;
(*ambiente vuoto: funzione che restituisce sempre il valore v per ogni input*)
let applyenv (r : 't env) (i : ide) = r i;;
(*applyenv: applica la funzione ambiente, ovvero dato un identificatore restituisce il valore cui è legato*)
(*ambiente, implementazione: 
	pila di funzioni 'congelate' necessarie per risalire nella ricerca del legame ide-valore*)
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;
(*bind: aggiunge la funzione definita sopra alla pila delle chiamate che vanno a formare l'ambiente*)

(*tipi esprimibili*)
type evT = 	Int of int | Bool of bool | Unbound | FunVal of evFun | RecFunVal of ide * evFun |
			DictVal of (ide * evT)list | FunArgVal of ide list * exp * evT env
and evFun = ide * exp * evT env;;

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

(*restituisce il tipo (sottoforma di stringa) del valore evT passato;
	utilizzato per effettuare type-checking nel dizionario *)
let getType (v : evT) : string = match v with
		Int(_) -> "int" |
		Bool(_) -> "bool" | 
		_ -> "other";;

(*restituisce il valore di default associato al tipo passato per argomento;
	utilizzato per inizializzare l'accumulatore della Fold*)
let getDefaultValOfType (s : string) = match s with
	"int" -> Int(0) |
	"bool" -> Bool(false) |
	_ -> Unbound;;

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

let int_of_evT (x : evT) = match x with
	| Int(u) -> u
	| _ -> failwith("Tipo non intero");;

(*cerca una stringa in una lista di stringhe restituendo un Bool*)
let rec isIdeIn (i : ide) (l : ide list) : evT = match l with
	[] -> Bool(false) | 
	h::t -> if i=h then Bool(true) else isIdeIn i t;;

(*valutatore*)
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
	FunArg(l, body) -> FunArgVal (l, body, r) |
	FunCall(f, eArg) -> 
		let fClosure = (eval f r) in
			(match fClosure with
				FunVal(arg, fBody, fDecEnv) -> (match eArg with 
					h::[] -> eval fBody (bind fDecEnv arg (eval h r)) |
					_ -> raise (InvalidArgumentException "expected exactly 1 argument")) |
				RecFunVal(g, (arg, fBody, fDecEnv)) -> (match eArg with 
					h::[] -> let aVal = (eval h r) in
								let rEnv = (bind fDecEnv g fClosure) in
									let aEnv = (bind rEnv arg aVal) in
										eval fBody aEnv |
					_ -> raise (InvalidArgumentException "expected exactly 1 argument")) |		
				FunArgVal(l, body, r_static) -> let l1 = List.length eArg in let l2 = List.length l in 
					if l1=l2 then
						eval body (bindArgs l eArg r_static r)
					else
						raise (InvalidArgumentException ("the function expects " ^ string_of_int l2 ^ 
								" arguments, while " ^ string_of_int l1 ^ " arguments are passed") ) |
				_ -> failwith("non functional value")) |

    Letrec(f, funDef, letBody) ->
    		(match funDef with
        		Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                     			                eval letBody r1 |
        		_ -> failwith("non functional def")) |
    (*il primo valore determina il tipo del dizionario*)
	CreateDict(li) -> ( match li with
			[] -> DictVal([]) |
			(i, e)::t -> let v = eval e r in 
							match getType v with
								"int" -> DictVal( (i, v)::(evalDict t r "int") )| 
								"bool" -> DictVal( (i, v)::(evalDict t r "bool") )|
								_ -> raise (InvalidDictionaryException "invalid dictionary values type")) |

	PrintDictValues(d) -> (match eval d r with
							DictVal(l) -> printDictValues l |
							_-> raise (InvalidDictionaryException "not a Dictionary")) |
	(*inserisce una coppia nel dizionario sse la chaive non è già presente E il tipo del valore è consistente*)
	Insert (i, e, d) -> (match eval d r with
							DictVal(l) -> let x = eval e r in 
								(match l with
									[] -> DictVal(insertEntry l i x)|
									(_, v)::t -> 
										if getType x = getType v then DictVal(insertEntry l i x)
										else raise (InvalidDictionaryException "inconsistent type") ) |
							_-> raise (InvalidDictionaryException "not a Dictionary")) |
	(*elimina la coppia (i, v) dal dizionario se la chaive è presente*)
	Delete(i, d) -> (match eval d r with
						DictVal(l) -> DictVal(deleteEntry l i) |
						_-> raise (InvalidDictionaryException "not a Dictionary")) |
	(*restituisce true se la chiave è presente nel dizionario*)
	HasKey(i, d) -> (match eval d r with
						DictVal(l) -> hasKeyDict l i |
						_-> raise (InvalidDictionaryException "not a Dictionary")) |
	(*applica la funzione f ad ogni valore presente nel dizionario restituendo un dizionario di coppie (i, v')*)
	Iterate(f, d) -> (match eval d r with
						DictVal(l) -> let funEv = eval f r in (match funEv with
							FunVal(arg, body, r_static) -> DictVal(iterateDict l funEv r) |
							_ -> raise (InvalidFunctionException "not a fuction to iterate with")) |
						_ -> raise (InvalidDictionaryException "not a Dictionary") ) |
	(*applica sequenzialmente la funzione f su ogni valore del dizionario, utilizzando un accumulatore
		da utilizzare come primo input per la successiva applicazione della funzione*)
	Fold(f, d) -> (match eval d r with
						DictVal(l) -> let funEv = eval f r in (match funEv with
							FunArgVal(arg, body, r_static) -> (match l with
								(i, v)::t ->  (match arg with
									_::_::[] -> let typ = getType v in
										foldDict l funEv (getDefaultValOfType typ) r |
									_ -> raise (InvalidArgumentException "not a valid fold function: not 2 args"))|
								_ -> Unbound) |
							_ -> raise (InvalidFunctionException "not a valid fold function")) |
						_ -> raise (InvalidDictionaryException "not a Dictionary") ) |
	(*restituisce il dizionario composto solamente dalle coppie i cui identificatori sono presenti in ide_list*)
	Filter(ide_list, d) -> (match eval d r with
						DictVal(l) ->  DictVal( filterDict l ide_list ) |
						_ -> raise (InvalidDictionaryException "not a Dictionary") )

and bindArgs (ide_list : ide list) (argExp_list : exp list) (r_st : evT env) (r_curr : evT env) : evT env = 
	(match ide_list with
		[] -> r_st | (*ho già controllato che la lunghezza dei parametri formali ed attuali sia uguale*)
		h::t -> (match argExp_list with
			a::b -> bindArgs t b (bind r_st h (eval a r_curr)) r_curr))

and evalDict (l : (ide * exp) list ) (r : evT env) (typ : string) : (ide * evT)list = match l with
		[] -> [] |
		(i, e)::t -> let x = eval e r in let typ1 = getType x in
							if typ1=typ then (i, x)::(evalDict t r typ)
							else 	raise (InvalidDictionaryException "inconsistent type")

and printDictValues (l : (ide * evT)list ) = match l with
		[] -> Unbound |
		(ide, v)::t -> (match v with
				Int(u) -> Printf.printf "[%s]-> %i; " ide u |
				Bool(z) -> 	Printf.printf "[%s]-> %B; " ide z);
		printDictValues t

and insertEntry (l : (ide * evT)list) (i : ide) (x : evT) : (ide * evT)list = match l with
		[] -> [(i, x)] |
		(a, v)::t -> if a=i then raise (InvalidArgumentException "key already present") 
							else (a,v)::(insertEntry t i x)

and deleteEntry (l : (ide * evT)list) (x : ide) : (ide * evT)list = match l with
		[] -> raise (InvalidArgumentException "key not present") |
		(i, v)::t -> if x=i then t else (i,v)::(deleteEntry t x)

and hasKeyDict (l : (ide * evT)list) (x : ide) : evT = match l with
		[] -> Bool(false) |
		(i, v)::t -> if x=i then Bool(true) else hasKeyDict t x

and iterateDict (l : (ide * evT)list ) (f : evT) (r : evT env) : (ide * evT)list = match l with
		[] -> [] |
		(i, v)::t -> match f with
						FunVal(arg, body, r_static) -> let x = eval body (bind r_static arg v) in 
									(i, x)::(iterateDict t f r)

and foldDict (l : (ide * evT)list ) (f : evT) (acc : evT) (r : evT env) : evT = match l with
		[] -> acc |
		(i, v)::t -> match f with
						FunArgVal(arg, body, r_static) -> match arg with
						a::b::[] -> let envAcc = bind r_static a acc in
										let x = eval body (bind envAcc b v) in 
											(foldDict t f x r)

and filterDict (l : (ide * evT)list ) (ide_l : ide list) : (ide * evT)list = match l with
		[] -> [] | 
		(i, v)::t -> 	if (isIdeIn i ide_l = Bool(true)) then (i, v)::(filterDict t ide_l)
						else filterDict t ide_l;;

(* =============================  ENVs_for_TESTS  ================= *)

let env0 : ide -> evT = emptyenv Unbound;;

let env1 = bind env0 "intDict" (eval (CreateDict([("Birman", Eint(3));("Mainecoon", Eint(13));
										("Siamese", Eint(17));("Foldex", Eint(21))])) env0);;

let env2 = bind env1 "boolDict" (eval (CreateDict(
		[("Birman", Ebool(true));("Mainecoon", Ebool(false));("Siamese", Ebool(false));("Foldex", Ebool(false))])) env0);;

let env3 = bind env2 "emptyDict" (eval (CreateDict([])) env1);;