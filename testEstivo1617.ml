(************************
*	Nome:Francesco		*
*	Cognome:Vatteroni 	*
*	Matricola:468134	*
*	Corso:A				*
*************************)

#use "InterpreteEstivo1617.ml";;

(* Int 4 *)
let test01 = sem (
	Let("f", (EFun("x", (Op("+", (EIde("x")), (EInt(1)) )))), 	
		Appl((EIde("f")), (EInt(3)) ))
) newEmptyEnv ;;

(* Int 5 *)
let test02 = sem (
	Let("f", (EFun("x", (Op("-", (Op("*",(Op("+", (EIde("x")), (EInt(1)) )),(EInt(2)))), (EInt(3))))) ), 
		(Appl((EIde("f")), (EInt(3)) ))) 
) newEmptyEnv ;;

(* Bool true *)
let test03 = sem (
	Let("x", (EBool(true)), 
		(Let("y", (EBool(false)) , 
			(Let("z", (EBool(true)) , 
				(And((EIde("x")),(Not(Or((EIde("y")),(And((EIde("z")),(Not(Or((EIde("y")),(EIde("x")))))))))))))))))
) newEmptyEnv ;;

(* Int 2 *)
let test04 = sem (
	Let("f", (EFun("x", (Ifthenelse((Op("=", (EIde("x")), (EInt(0)))), (Op("+", (EIde("x")), (EInt(2)) )), (EInt(2)))))), 
		(Appl((EIde("f")), (EInt(0)))))
) newEmptyEnv ;;

(* [DictElem ("", Unbound)]*)
let test05 = sem (
	EDict([])
)  newEmptyEnv;;

(* Int 2 *)
let test06 = sem (
	At_Dict ((EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))])), "b")
) newEmptyEnv;;

(* Unbound *)
let test07 = sem (At_Dict ((EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))])), "x"))  newEmptyEnv;;

(* Bool true *)
let test08 = sem (Equal_Dict ((EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))])), (EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))]))))  newEmptyEnv;;

(* Bool false *)
let test09 = sem (Equal_Dict ((EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))])), (EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(7)));EDictElem("c",(EInt(3)))]))))  newEmptyEnv;;

(* (a,2);(b,3);(c,4) *)
let test10 = sem (Appl_Dict ((EFun("x", (Op("+", (EIde("x")), (EInt(1)) )))), (EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))]))))  newEmptyEnv;;

(* (a,101);(b,102);(c,103) *)
let test11 = sem (
	Let( "f", (EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))])),
		Let( "g", (EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))])),
			Let ( "h", (EFun("x", (Op("+", (EIde("x")), (EInt(100)) )))),
				Ifthenelse(
					(Equal_Dict( (EIde("f")),(EIde("g")) )),
					(Appl_Dict ( (EIde("h")),(EIde("f")) )),
					(EBool(false))
				)
			)
		)
	)
) newEmptyEnv;;

(* (x,9);(y,8);(z,7) *)
let test12 = sem (
	Let( "f", (EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("c",(EInt(3)))])),
		Let( "f", (EDict([EDictElem("x",(EInt(9)));EDictElem("y",(EInt(8)));EDictElem("z",(EInt(7)))])),
			(EIde("f"))
		)
	)
) newEmptyEnv;;

(* Invalid EDict *)
let test12 = sem (EDict([EDictElem("a",(EInt(1)));EDictElem("b",(EInt(2)));EDictElem("a",(EInt(3)))]))  newEmptyEnv;;
