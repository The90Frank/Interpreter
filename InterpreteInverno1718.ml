(************************
*	Nome:Francesco		*
*	Cognome:Vatteroni 	*
*	Matricola:468134	*
*	Corso:A				*
************************)

(* Progettazione e sviluppo di un interprete in Ocaml *)

(* Tipi *)
type d_Ide = string

and exp = 								(* Espressioni da valutare *)
	| EIde of d_Ide						(* Identificatori *)
	| EInt of int						(* Interi *)
	| EBool	of bool						(* Booleani *)
	| EString of string					(* Stringhe *)
	| EFun of d_Ide * exp				(* Funzione con un parametro, non ricorsiva *)
	| And of exp * exp					(* And logico *)
	| Or of exp * exp					(* Or logico *)
	| Not of exp						(* Not logico *)
	| Op of string * exp * exp			(* Espressioni su interi con OP /in {"+", "-", "*", "=", "<="} *)
	| Ifthenelse of exp * exp * exp 	(* Condizionale *)
	| Let of d_Ide * exp * exp 			(* Dichiarazione di Identificatore : modifica ambiente *)
    | Appl of exp * exp
    | ETree of tree                     (* Gli alberi sono anche espressioni *)
    | ApplyOver of exp * exp            (* Applicazione di funzione ai nodi *)
    | Update of (d_Ide list) * exp * exp(* Aggiornamento di un nodo *)
    | Select of (d_Ide list) * exp * exp(* Selezione condizionale di un nodo *)

and tree = 
    | Empty                             (* Albero vuoto *)
    | Node of d_Ide * exp * tree * tree (* Albero binario *)

and e_Val = 							(* Valori risultato di un espressione *)
	| Int of int						(* Interi *)
	| Bool of bool						(* Booleani *)
	| String of string					(* Stringhe *)
	| Fun of d_Ide * exp * env 			(* Funzione: una tripla Parametro Codice Ambiente *)
	| Unbound							(* Valore speciale *)
	| VTree of vtree     				(* Albero *)

and vtree =
    | VEmpty                                    (* Albero vuoto *)
    | VNode of d_Ide * e_Val * vtree * vtree	(* Albero binario *)


(* Ambiente *)

and env = (d_Ide * e_Val) list ;;		(* Ambiente: Una collezione di coppie Identificatore-Valore *)

let newEmptyEnv = [("",Unbound)] ;;		(* Ambiente Vuoto *)

let rec applyEnv (x: env) (y: d_Ide) = 	(* Ricerca nell'ambiente*)
	match x with
		| (i,v)::o when y = i -> v
		| [("",Unbound)] -> Unbound
		| x::o -> applyEnv o y
		| [] -> failwith "Errore in ApplyEnv: Ambiente errato"
;;
										(* Inserimento nell'ambiente *)
let bind (r: env) (l: d_Ide) (e: e_Val) = (l,e)::r ;;


(* Operatori *)

let and_Op a b = match (a,b) with		(* And *)
	| ( Bool(a) , Bool(b) ) -> Bool(a && b)
	| _ -> failwith "Errore in And_Op: Wrong type"
;;

let or_Op a b = match (a,b) with		(* Or *)
	| ( Bool(a) , Bool(b) ) -> Bool(a || b)
	| _ -> failwith "Errore in Or_Op: Wrong type"
;;

let not_Op a = match a with				(* Not *)
	| Bool(a) -> Bool(not a)
	| _ -> failwith "Errore in Not_Op: Wrong type"
;;
	
let equal_Op a b = match (a,b) with 	(* Not Xor *)
	| ( Bool(true) , Bool(true) ) -> Bool(true)
	| ( Bool(true) , Bool(false) ) -> Bool(false)
	| ( Bool(false) , Bool(true) ) -> Bool(false)
	| ( Bool(false) , Bool(false) ) -> Bool(true)
	| _ -> failwith "Errore in equal_Op: Wrong type"
;;

let int_Op s a b = match (s,a,b) with	(* +,-,*,=,<=, < tra Interi *)
	| ( "+" , Int(a) , Int(b) ) -> Int(a+b)
	| ( "-" , Int(a) , Int(b) ) -> Int(a-b)
	| ( "*" , Int(a) , Int(b) ) -> Int(a*b)
	| ( "=" , Int(a) , Int(b) ) -> Bool(a=b)
	| ( "<" , Int(a) , Int(b) ) -> Bool(a<b)
	| ( "<=" , Int(a) , Int(b) ) -> Bool(a<=b)
	| ( _ , Int(a) , Int(b)) -> failwith "Errore in Int_Op: Wrong operator"
	| _ -> failwith "Errore in Int_Op: Wrong type"
;;
	
let rec tree_Val x = match x with		(* Controllo validità dell' albero *)
	| ETree(Empty) -> true
	| ETree(Node(_,_,ys,zs)) -> (tree_Val(ETree(ys)) && tree_Val(ETree(zs)))
	| _ -> false
;;

let rec map f r = function []->[] | hd::tl -> (f hd r)::(map f r tl)	(* Funzione map *)

and appl_Tree_Op (f: e_Val) (t: vtree) = match t with				            (* Applica una funzione "f" a tutti gli elementi dell' Albero "vtree" *)
	| VEmpty -> VEmpty                        		                            (* Non è necessario l'ambiente in quanto gli argomenti son già stati valutati *)
	| VNode(k,v,ls,rs) -> (						    		                    (* Utilizza lo stesso ragionamento di Appl *)
		match f with 
			| Fun(a, b, c) -> VNode(k,( sem b ( bind c a v )) , ( appl_Tree_Op f ls )  ,( appl_Tree_Op f rs ))
			| _ -> failwith "Errore in appl_Tree_Op sub: Wrong type"
		)
	| _ -> failwith "Errore in appl_Tree_Op: Operation not permitted"

and update_Tree_Op (p: (d_Ide list)) (f: e_Val) (t: vtree) = match (p,t) with   (* Applica una funzione "f" a tutti gli elementi dell' Albero "vtree" visitati seguendo la lista "p" *)
    | ([],v) -> v                                                               (* Non è necessario l'ambiente in quanto gli argomenti son già stati valutati *)
    | (_,VEmpty) -> VEmpty                                                      (* Utilizza lo stesso ragionamento di Appl *)
    | (y::ys,VNode(k,v,ls,rs)) -> if k=y then 
                                  VNode(k,
                                    (match f with
                                        | Fun(a, b, c) -> ( sem b ( bind c a v ))
                                        | _ -> failwith "Errore in update_Tree_Op: Wrong Type"
                                    ),
                                    (match ls with
                                        | VNode(ks,_,_,_) -> (match ys with
                                                                | j::_ -> if ks=j then (update_Tree_Op ys f ls)
                                                                    else ls
                                                                | [] -> ls)
                                        | VEmpty -> VEmpty),
                                    (match rs with
                                        | VNode(kr,_,_,_) -> (match ys with
                                                                | j::_ -> if kr=j then (update_Tree_Op ys f rs)
                                                                    else rs
                                                                | [] -> rs)
                                        | VEmpty -> VEmpty)
                                  )
                                  else t
    | (_,_) -> failwith "Errore in update_Tree_Op: Wrong type"

and select_Tree_Op (p: (d_Ide list)) (f: e_Val) (t: vtree) = match (p,t) with   (* Estrae un nodo(VNode o VEmpty) dell' Albero "vtree" selezionato visitando l'albero *)
    | ([],_) -> t                                                               (* seguendo la lista "p" la cui valutazione di "f" dia Bool(true) *)
    | (_,VEmpty) -> VEmpty                                                      (* Non è necessario l'ambiente in quanto gli argomenti son già stati valutati *)
    | (y::ys,VNode(k,v,ls,rs)) -> (
        match f with
            | Fun(a, b, c) -> (
                match ( sem b ( bind c a v )) with
                    | Bool(x) ->
                        if ((k=y) && (not x)) then (
                            match ((ls,rs),ys) with
                                | ((VNode(ks,_,_,_),VNode(kr,_,_,_)),j::_) -> 
                                        if ks = j then 
                                            let node = (select_Tree_Op ys f ls) in
                                            match node with
                                                | VEmpty -> if kr = j then (select_Tree_Op ys f rs)
                                                            else VEmpty 
                                                | _ -> node
                                        else if kr = j then (select_Tree_Op ys f rs)        
                                        else VEmpty
                                | ((VEmpty,VNode(kr,_,_,_)),j::_) ->
                                        if kr = j then (select_Tree_Op ys f rs)
                                        else VEmpty
                                | ((VNode(ks,_,_,_),VEmpty),j::_) -> 
                                        if ks = j then (select_Tree_Op ys f rs)
                                        else VEmpty
                                | ((_,_),_) -> t
                        )
                        else if ((k=y) && x) then t
                        else VEmpty
                    | _ -> failwith "Errore in select_Tree_Op: Wrong Function")
            | _ -> failwith "Errore in select_Tree_Op: Wrong Type")
    | (_,_) -> failwith "Errore in select_Tree_Op: Wrong type"

(* Semantica *)	
and sem (e: exp) (r: env) = match e with								(* val sem : exp -> env -> e_Val = <fun> *)	

	(********* Casi (tipi) Base *********)
	| EIde(x) -> applyEnv r x											(* Cerco il valore nell'ambiente *)
	| EInt(x) -> Int(x)													(* Intero come espressione *)
	| EBool(x) -> Bool(x)												(* Booleano come espressione *)
	| EString(x) -> String(x)											(* Stringa come espressione *)
	| EFun(x(*parametri*), y(*corpo*)) -> Fun(x, y, r)					(* Funzione come espressione *)

	(********* Operatori sui Booleani *********)
	| And(x, y) -> and_Op ( sem x r ) ( sem y r ) 						(* Chiamo and_Op passandogli la valutazione dei parametri *)
	| Or(x, y) -> or_Op ( sem x r ) ( sem y r)							(* Chiamo or_Op passandogli la valutazione dei parametri *)
	| Not(x) -> not_Op ( sem x r )										(* Chiamo not_Op passandogli la valutazione dei parametri *)
	
	(********* Operatori sugli interi *********)
	| Op(x, y, z) -> int_Op x ( sem y r ) ( sem z r )					(* Chiamo int_Op, che sceglierà quindi quale operazione effettuare, ricevendo i due parametri già valutati *)
	
	(********* Comando Condizionale *********)
	| Ifthenelse(x, y, z) -> ( match ( sem x r ) with 					(* Si valuta il primo parametro *)
		| Bool(true) -> sem y r | Bool(false) -> sem z r 				(* A seconda del risultato, si procede valutando il secondo o il terzo parametro. *)
		| _ -> failwith "Errore in Ifthenelse: Non Boolean Guard" )		(* Sempre che il risultato della valutazione del primo parametro sia di tipo Bool *)

	(********* Operatori sulle Funzioni *********)
	| Let(x, y, z) -> sem z ( bind r x ( sem y r ) )					(* Dichiarazione di Espressione. Si valuta y, si aggiunge nell'ambiente x = y, si valuta z *)
	| Appl(x(*funz.*), y(*arg.*)) -> ( match ( sem x r ) with			(* Applicazione di funzione.  valutata nell'ambiente corrente equivale a una chiusura contentente l'ambiente... *)					
			| Fun(a, b, c) -> sem b ( bind c a ( sem y r ) )			(* ...nel quale era stata valutata l'astrazione, il corpo e i parametri. Valutarla significa valutarne... *)
			| _ -> failwith "Errore in Appl: No function in Apply" )	(* ...il corpo nell'ambiente ottenuto legando i parametri formali ai valori dei parametri attuali *)
		
	(********* Operatori sugli Alberi *********)
    
	| ETree(x) -> if tree_Val(ETree(x)) = true then 	
            match ETree(x) with
            | ETree(Empty) -> VTree(VEmpty)
            | ETree(Node(k,v,ls,rs)) -> VTree(
                VNode(k,(sem v r),
                    (match (sem (ETree(ls)) r) with
                        | VTree(x) -> x
                        | _ -> failwith "Errore in Tree: Non Tree Node"
                    ),
                    (match (sem (ETree(rs)) r) with 
                        | VTree(x) -> x
                        | _ -> failwith "Errore in Tree: Non Tree Node"
                    )))
            | _ -> failwith "Invalid ETree"
		else failwith "Invalid ETree"

    | ApplyOver(x, y) -> (												(* Applica la funzione x all' Albero ( a tutti gli elementi (k,v,l,r) v del dizionario ) *)
		let f_val = ( sem x r ) in
			match f_val with 
				| Fun(a, b, c) -> (match ( sem y r ) with
                                    | VTree(xr) -> VTree(appl_Tree_Op f_val xr)
                                    | _ -> failwith "Errore in ApplOver: Invalid Tree")						
				| _ -> failwith "Errore in ApplOver: No function in Apply"
		)

    | Update(il,f,t) -> (											    (* Applica la funzione x ai Nodi dell'Albero definiti da il ( a tutti gli elementi (k,v,l,r) v del dizionario la cui k appartiene al path il ) *)
		let f_val = ( sem f r ) in
			match f_val with 
				| Fun(a, b, c) -> (match ( sem t r ) with
                                    | VTree(xr) -> VTree(update_Tree_Op il f_val xr)
                                    | _ -> failwith "Errore in ApplOver: Invalid Tree")						
				| _ -> failwith "Errore in ApplOver: No function in Apply"
		)

    | Select(il,f,t) -> (                                               (* Ritorna un Albero che ha come radice il Nodo appartenente a il che soddisfa la funzione x ( Nodo(k,v,l,r) dove k appartiene al path il e la valutazione di f con parametro reale v restituisce true ) *)
    let f_val = ( sem f r ) in
			match f_val with 
				| Fun(a, b, c) -> (match (sem t r) with
                                    | VTree(x) -> VTree( select_Tree_Op il f_val x )
                                    | _ -> failwith "Errore in Select: Invalid Type")
                | _ -> failwith "Errore in ApplOver: No function in Apply"
    )
	(********* Controllo di errore *********)
	| _ -> failwith "Errore in sem: Espressione non supportata"			(* Espressione errata *)
;;
