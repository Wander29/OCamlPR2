(*CREATE*)
eval (CreateDict([("Test1", Eint(0));("Test2", Eint(1))])) env3;;
	(*eccezioni*)
	(*1: primo valore di tipo non valido*) 	
		eval (CreateDict([("Test1", CreateDict([]));("Test2", Eint(1))])) env3;;
		eval (CreateDict([("Test1", Fun( "x", Sum(Den "x", Eint(3))) );("Test2", Eint(1))])) env3;;
	(*2: tipo di un valore successivo al primo inconsistente*) 
		eval (CreateDict([("Test1", Eint(0));("Test2", Ebool(true))])) env3;;
		eval (CreateDict([("Test1", Eint(0));("Test2", Fun( "x", Sum(Den "x", Eint(3))) )])) env3;;

(*INSERT*)
eval (Insert("Ragdoll", Eint(111), Den "intDict")) env3;;
	(*eccezioni*)
	(*1: Dizionario non valido/non presente*) 
		eval (Insert("Ragdoll", Eint(111), Den "invalidDict")) env3;;
	(*2: Chiave già presente*) 
		eval (Insert("Siamese", Eint(111), Den "intDict")) env3;;
	(*3: Tipo del valore da inserire inconsistente*) 
		eval (Insert("Ragdoll", Ebool(true), Den "intDict")) env3;;
		eval (Insert("Ragdoll", Fun( "x", Sum(Den "x", Eint(3))), Den "intDict")) env3;;

(*DELETE*)
eval (Delete("Siamese", Den "intDict")) env3;;
	(*eccezioni*)
	(*1: chiave NON presente*) 
		eval (Delete("Ragdoll", Den "intDict")) env3;;

(*HAS_KEY*)
eval (HasKey("Birman", Den "intDict")) env3;; (*-> true*)
eval (HasKey("Birman", Den "emptyDict")) env3;; (*-> false*)
eval (HasKey("Ragdoll", Den "intDict")) env3;; (*-> false*)

(*ITERATE*)
eval (Let("sum2each", Fun("z", Sum(Den "z", Eint(2))), 
	Let ("y", Insert("Forest Cat", Eint(11), Den "intDict"), Iterate(Den "sum2each", Den "y"))
	)) env3;;

let env4 = bind env3 "negateAll" (eval (Fun("x", Not(Den "x")) ) env3);;
eval (Iterate(Den "negateAll", Den "boolDict")) env4;;
eval (Iterate(Den "negateAll", Den "emptyDict")) env4;;
	(*eccezioni*)
	(*1: passare una funzione non unaria*) 	
		eval (Let("sum2each", (FunArg(["z"; "w"], Sum(Den "z", Den "w"))), Iterate(Den "sum2each", Den "intDict"))) env3;;
	(*2: typechecking della funzione non rispettato*)
		eval (Iterate(Den "negateAll", Den "intDict")) env4;;

(**********)
(*MULTI_ARGs_functions*)
let env_multiArg = bind env3 "fma" (eval 
		(FunArg(["a";"b";"c";"d"], Diff(Prod(Den "a", Sum(Den "b", Den "c")), Den "d"))) env3);;

eval (FunCall( Den "fma", [Eint(2); Eint(11); Eint(45); Eint(4)])) env_multiArg;;
	(*eccezioni*)
	(*1: numero parametri formali != da numero parametri attuali*)
		eval (FunCall( Den "fma", [Eint(11); Eint(45); Eint(4)])) env_multiArg;;
		eval (FunCall( Den "fma", [Eint(11); Eint(45); Eint(4); Eint(3); Eint(87)])) env_multiArg;;
	(*2: typechecking della funzione non rispettato*)
		eval (FunCall( Den "fma", [Ebool(true); Eint(11); Ebool(true); Eint(4)])) env_multiArg;;
(**********)

(*FOLD*)
let env5 = bind env3 "foldFun" (eval (FunArg(["x"; "y"], Prod(Sum(Den "x", Den "y"), Eint 3) )) env3 );;
eval (Fold(Den "foldFun", Den "intDict")) env5;;
(*esempio nella descrizione del progetto*)
eval (Let("magazzino", CreateDict( [("mele", Eint(430)); ("banane", Eint(312)); ("arance", Eint(525)); ("pere", Eint(217))] ), 
	Let("ff", FunArg(["x"; "y"], Sum(Sum(Den "x", Den "y"), Eint(1))), Fold(Den "ff", Den "magazzino")))) env3;;
(*fold su booleani*)
eval (Fold(
			FunArg(["a"; "b"], Or(Not(Den "a"), Den "b")),
			Den "boolDict"  
		)) env3;;
(*Dizionario Vuoto*)
eval (Fold(
			FunArg(["a"; "b"], Or(Not(Den "a"), Den "b")),
			Den "emptyDict"  
		)) env3;; (*-> Unbound*)
	(*eccezioni*)
	(*1: funzione non binaria*)
		eval (Fold(
			FunArg(["a"; "b"; "w"], And(Or(Not(Den "a"), Den "b"), Den "w")),
			Den "boolDict"  
		)) env3;;

		eval (Let("ff", Fun("z", Sum(Den "z", Eint(1))), Fold(Den "ff", Den "intDict")) ) env3;;
	(*2: typechecking della funzione non rispettato*)
		eval (Fold(
			FunArg(["a"; "b"], Or(Not(Den "a"), Den "b")),
			Den "intDict"  
		)) env3;;
(*FILTER*)
eval(Filter(["Siamese";"Birman"], Den "boolDict")) env4;;
	(*se una chiave non è presente non genera eccezioni, semplicemente la salta*)
		eval(Filter(["Gattino";"Prova"], Den "boolDict")) env4;;
		eval(Filter(["Siamese";"Gattino";"Birman";"Prova"], Den "boolDict")) env4;;

(*PRINT_DICT_VALUES*)
eval (PrintDictValues(Den "intDict")) env3;;
