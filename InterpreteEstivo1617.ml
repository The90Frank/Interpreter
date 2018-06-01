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
	| EDictElem of d_Ide * exp			(* Elemento di Dizionario *)
	| EDict of exp list					(* Dizionario *)
	| And of exp * exp					(* And logico *)
	| Or of exp * exp					(* Or logico *)
	| Not of exp						(* Not logico *)
	| Op of string * exp * exp			(* Espressioni su interi con OP /in {"+", "-", "*", "=", "<="} *)
	| Ifthenelse of exp * exp * exp 	(* Condizionale *)
	| Let of d_Ide * exp * exp 			(* Dichiarazione di Identificatore : modifica ambiente *)
	| Appl of exp * exp					(* Applicazione Funzionale *)
	| At_Dict of exp * d_Ide			(* Accesso elemento Dizionario *)
	| Equal_Dict of exp * exp			(* Confronto tra Dizionari *)
	| Appl_Dict of exp * exp			(* Applicazione Funzionale su Dizionario *)

and e_Val = 							(* Valori risultato di un espressione *)
	| Int of int						(* Interi *)
	| Bool of bool						(* Booleani *)
	| String of string					(* Stringhe *)
	| Fun of d_Ide * exp * env 			(* Funzione: una tripla Parametro Codice Ambiente *)
	| Unbound							(* Valore speciale *)
	| Dict of e_Val list				(* Dizionario *)
	| DictElem of d_Ide * e_Val			(* Elemento di Dizionario *)

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

let dict_Op s a b = match (s,a,b) with	(* Append tra Dizionari *)
	| ( "::" , Dict(a) , Dict(b) ) -> Dict(a@b)
	| _ -> failwith "Errore in dict_Op: Wrong type"
	
let rec dict_Val x = match x with		(* Controllo validità del Dizionario *)
	| EDict[] -> true
	| EDict((EDictElem(k,_))::xs) -> ((dict_Val (EDict(xs))) && (let rec aux y = 
		match y with
			| EDict[] -> true
			| EDict((EDictElem(ky,_))::(ys)) -> if k=ky then false
				else aux (EDict(ys))
			| _ -> false
		in aux (EDict(xs))))
	| _ -> false

let rec map f r = function []->[] | hd::tl -> (f hd r)::(map f r tl)	(* Funzione map *)

and at_Dict_Op (dict: e_Val) (key: d_Ide) = match dict with				(* Accesso all'elemento "key" di "dict" *)
	| Dict[DictElem("",Unbound)] -> Unbound
	| Dict((DictElem(k,v))::xs) -> if k=key then v else at_Dict_Op (Dict(xs)) key
	| _ -> failwith "Errore in At_Dict_Op: invalid types for Dict subscript"
	
and appl_Dict_Op (f: e_Val) (dict: e_Val) = match dict with				(* Applica una funzione "f" a tutti gli elementi della Dizionario "dict" *)
	| Dict([DictElem("",Unbound)]) -> Dict([DictElem("",Unbound)])		(* Non è necessario l'ambiente in quanto gli argomenti son già stati valutati *)
	| Dict((DictElem(k,v))::xs) -> (									(* Utilizza lo stesso ragionamento di Appl *)
		match f with 
			| Fun(a, b, c) -> dict_Op "::" (Dict([DictElem(k,( sem b ( bind c a v ) ))])) ( appl_Dict_Op f (Dict(xs)) )
			| _ -> failwith "Errore in appl_Dict_Op sub: Wrong type"
		)
	| _ -> failwith "Errore in appl_Dict_Op: Operation not permitted"

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
		
	(********* Operatori sui Dizionari *********)
	| EDictElem(x,y) -> DictElem (x,(( sem y r )))						(* Elemento di Dizionario come espressione *)
	| EDict(x) -> if dict_Val(EDict(x)) = true then 					(* Dict come espressione. Dict(["",Unbound]) è l'elemento terminatore di un dizionario, equivale a dizionario vuoto. Deve essere quindi sempre presente alla fine di un dizionario. *)
		dict_Op "::" (Dict( map sem r x )) (Dict([DictElem("",Unbound)])) 
		else failwith "Invalid EDict"
	| At_Dict(x, y) -> at_Dict_Op ( sem x r ) y							(* Restituisce l'elemento legato alla chiave y nel dizionario x. Chiamo at_Dict_Op passandogli la valutazione dei parametri *)
	| Equal_Dict(x, y) -> if ( compare ( sem x r ) ( sem y r ) ) = 0 						(* Chiamo compare sui dei due argomenti, e ne controllo il risultato *)
							then Bool(true) else Bool(false)
	| Appl_Dict(x, y) -> (												(* Applica la funzione x al Dizionario ( a tutti gli elementi (k,v) v del dizionario ) *)
		let f_val = ( sem x r ) in
			match f_val with 
				| Fun(a, b, c) -> appl_Dict_Op f_val ( sem y r )							
				| _ -> failwith "Errore in Appl_Dict: No function in Apply"
		)
		
	(********* Controllo di errore *********)
	| _ -> failwith "Errore in sem: Espressione non supportata"			(* Espressione errata *)
;;
