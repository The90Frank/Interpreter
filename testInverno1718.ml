(************************
*	Nome:Francesco		*
*	Cognome:Vatteroni 	*
*	Matricola:468134	*
*	Corso:A				*
*************************)

#use "InterpreteInverno1718.ml";;

let test01 = sem (
	Let("f", (EFun("x", (Op("+", (EIde("x")), (EInt(1)) )))), 	
		Appl((EIde("f")), (EInt(3)) ))
) newEmptyEnv ;;

let test02 = sem (
	Let("f", (EFun("x", (Op("-", (Op("*",(Op("+", (EIde("x")), (EInt(1)) )),(EInt(2)))), (EInt(3))))) ), 
		(Appl((EIde("f")), (EInt(3)) ))) 
) newEmptyEnv ;;

let test03 = sem (
	Let("x", (EBool(true)), 
		(Let("y", (EBool(false)) , 
			(Let("z", (EBool(true)) , 
				(And((EIde("x")),(Not(Or((EIde("y")),(And((EIde("z")),(Not(Or((EIde("y")),(EIde("x")))))))))))))))))
) newEmptyEnv ;;

let test04 = sem (
	Let("f", (EFun("x", (Ifthenelse((Op("=", (EIde("x")), (EInt(0)))), (Op("+", (EIde("x")), (EInt(2)) )), (EInt(2)))))), 
		(Appl((EIde("f")), (EInt(0)))))
) newEmptyEnv ;;

let test05 = sem (
	ETree(Empty)
)  newEmptyEnv;;

let test06 = sem (
    (ETree(
        Node(
            "nodo0",
            EInt(0),
            Empty,
            Node(
                "nodo1",
                EInt(10),
                Node(
                    "nodo3",
                    EInt(777),
                    Empty,
                    Empty
                    ),
                Node(
                    "nodo2",
                    EInt(100),
                    Empty,
                    Empty
                    )
                )
            )
        )
    )
) newEmptyEnv;;

let test07 = sem (
    Let("f", (EFun("x", (Op("-", (Op("*",(Op("+", (EIde("x")), (EInt(1)) )),(EInt(2)))), (EInt(13))))) ), 
		    (ApplyOver(
                (EIde("f")), 
                (ETree(
                    Node(
                        "nodo0",
                        EInt(0),
                        Empty,
                        Node(
                            "nodo1",
                            EInt(10),
                            Node(
                                "nodo3",
                                EInt(777),
                                Empty,
                                Empty
                                ),
                            Node(
                                "nodo2",
                                EInt(100),
                                Empty,
                                Empty
                                )
                            )
                        )
                      )
                )
            )
        )
    ) 
) newEmptyEnv;;

let test08 = sem (
    Let("f", (EFun("x", (Op("-", (Op("*",(Op("+", (EIde("x")), (EInt(1)) )),(EInt(2)))), (EInt(13))))) ), 
		    (Update(
                ([("nodo0");("nodo1")]),
                (EIde("f")), 
                (ETree(
                    Node(
                        "nodo0",
                        EInt(0),
                        Empty,
                        Node(
                            "nodo1",
                            EInt(10),
                            Node(
                                "nodo3",
                                EInt(777),
                                Empty,
                                Empty
                                ),
                            Node(
                                "nodo2",
                                EInt(100),
                                Empty,
                                Empty
                                )
                            )
                        )
                      )
                )
              ) 
            )
    ) 
) newEmptyEnv;;

let test09 = sem (
    Let("f", (EFun("x", (Op("<", EInt(5), EIde("x"))))), 
		    (Select(
                ([("nodo0");("nodo1")]),
                (EIde("f")), 
                (ETree(
                    Node(
                        "nodo0",
                        EInt(0),
                        Empty,
                        Node(
                            "nodo1",
                            EInt(10),
                            Node(
                                "nodo3",
                                EInt(777),
                                Empty,
                                Empty
                                ),
                            Node(
                                "nodo2",
                                EInt(100),
                                Empty,
                                Empty
                                )
                            )
                        )
                      )
                )
              ) 
            )
    ) 
) newEmptyEnv;;