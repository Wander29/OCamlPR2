let env0 : ide -> evT = emptyenv Unbound;;

let env1 = bind env0 "x" (eval (CreateDict([("Birman", Eint(3));("Mainecoon", Eint(13));("Siamese", Eint(17));("Foldex", Eint 21)])) env0);;

eval (Let("sum2each", Fun("z", Sum(Den "z", Eint(2))), 
	Let ("y", Insert("Forest Cat", Eint(11), Den "x"), Iterate(Den "sum2each", Den "y"))
	)) env1;;

eval (Delete("Siamese", Den "x")) env1;;

eval (Insert("Siamese", Eint(54), Den "x")) env1;;

eval (HasKey("Siamese", Den "x")) env1;;

eval (HasKey("Gatto", Den "x")) env1;;

eval (Fold(
			FunArg(["x"; "y"], Prod(Sum(Den "x", Den "y"), Eint 3) ),
			Den "x"  
		)) env1;;