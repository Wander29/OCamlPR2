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
eval (HasKey("Ragdoll", Den "intDict")) env3;; (*-> false*)

(*ITERATE*)
eval (Let("sum2each", Fun("z", Sum(Den "z", Eint(2))), 
	Let ("y", Insert("Forest Cat", Eint(11), Den "intDict"), Iterate(Den "sum2each", Den "y"))
	)) env3;;

let env4 = bind env3 "negateAll" (eval (Fun("x", Not(Den "x")) ) env3);;
eval (Iterate(Den "negateAll", Den "boolDict")) env4;;


(*PRINT_DICT_VALUES*)
eval (PrintDictValues(Den "intDict")) env3;;


(*********************)


eval (Iterate(Fun("a", And(Den "a", Ebool(true))), Den "x")) env1;;

eval (Delete("Siamese", Den "x")) env1;;

eval (Insert("Siamese", Eint(54), Den "x")) env1;;

eval (HasKey("Gatto", Den "x")) env1;;

eval (Fold(
			FunArg(["x"; "y"], Prod(Sum(Den "x", Den "y"), Eint 3) ),
			Den "x"  
		)) env1;;

eval (CreateDict([("Birman", Ebool(true));("Mainecoon", Eint(13));("Siamese", Eint(17));("Foldex", Eint 21)])) env0;;

eval (FunCall(
				FunArg(["a";"b";"c";"d"], Diff(Prod(Den "a", Sum(Den "b", Den "c")), Den "d")), 
				[Eint(2); Eint(11); Eint(45)]
			)) env1;;

eval (PrintDictValues(Den "boolDict")) env2;;

eval (Fold(
			FunArg(["a"; "b"], Or(Not(Den "a"), Den "b")),
			Den "boolDict"  
		)) env2;;

eval (Fold(
			FunArg(["a"; "b"], Or(Not(Den "a"), Den "b")),
			Den "x"  
		)) env2;;

eval(Filter(["Siamese";"Gattino";"Birman"], Den "boolDict")) env2;;