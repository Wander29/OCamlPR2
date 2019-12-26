eval (Let("sum2each", Fun("z", Sum(Den "z", Eint(2))), 
	Let ("y", Insert("Forest Cat", Eint(11), Den "x"), Iterate(Den "sum2each", Den "y"))
	)) env1;;

eval (Iterate(Fun("a", And(Den "a", Ebool(true))), Den "x")) env1;;

eval (Delete("Siamese", Den "x")) env1;;

eval (Insert("Siamese", Eint(54), Den "x")) env1;;

eval (HasKey("Siamese", Den "x")) env1;;

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