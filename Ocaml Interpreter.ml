(*
	Secondo progetto di PR2 (anno 2020-2021): estensione del linguaggio funzionale didattico

	Russo Stefano, matricola 544341

*)


type ide = string;;

type exp =  
    | Estring of string (*aggiunto*)
    | Eint of int
    | Ebool of bool
    | Den of ide
    | Prod of exp * exp
    | Sum of exp * exp
    | Diff of exp * exp
    | Eq of exp * exp
    | Minus of exp
    | IsZero of exp
    | IsPositive of exp (*aggiunto per test*)
    | IsNegative of exp (*aggiunto per test*)
    | Or of exp * exp
    | And of exp * exp
    | Not of exp
    | Ifthenelse of exp * exp * exp
    | Let of ide * exp * exp
    | Fun of ide * exp
    | FunCall of exp * exp
    | Letrec of ide * exp * exp
    	 	

    (*Estensione di exp per operare con insiemi*)
    | Empty of  ide
    | Singleton of exp * ide
    | Insert of exp * exp
    | Delete of exp * exp
    | Unione of exp * exp
    | Contiene of exp * exp
    | Intersezione of exp * exp
    | Differenza of exp * exp
    | IsEmpty of exp
    | IsSubSet of exp * exp
    | MaxOf of exp
    | MinOf of exp
    | For_all of exp * exp
    | Exists of exp * exp
    | Filter of exp * exp
    | Map of exp * exp
;;

(* ambiente polimorfo *)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(* tipi esprimibili *)
type evT =
  | String of string (*aggiunto*)
  | Int of int
  | Bool of bool
  | Unbound
  | FunVal of evFun
  | RecFunVal of ide * evFun
  | ESetVal of evT list * ide (*aggiunto*)  
and evFun = ide * exp * evT env


(*rts*)
(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
	| "int" -> (match v with
    | Int(_) -> true
    |	_ -> false)
  | "bool" -> (match v with
    | Bool(_) -> true
    | _ -> false)
  | "setval" -> (match v with         (*Aggiunto setval al type checking*)
    | ESetVal(_,_) -> true
    | _ -> false)
  | "string" -> (match v with         (*Aggiunto string al type checking*)
    | String(_) -> true
    | _ -> false)
  | _ -> failwith("not a valid type");;



(* funzioni primitive *)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n*u)
     |_-> failwith("Invalid arguments"))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n+u)
     |_-> failwith("Invalid arguments"))
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n-u)
     |_-> failwith("Invalid arguments"))
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Bool(n=u)
     |_-> failwith("Invalid arguments"))
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then (match x with
	   	Int(n) -> Int(-n)
        |_-> failwith("Invalid arguments"))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
		Int(n) -> Bool(n=0)
     |_-> failwith("Invalid arguments"))
	else failwith("Type error");;

  let ispositive x = if (typecheck "int" x)
    then (match x with
      Int(n) -> Bool(n>0)
       |_-> failwith("Invalid arguments"))
    else failwith("Type error");;
  
    let isnegative x = if (typecheck "int" x)
      then (match x with
        Int(n) -> Bool(n<0)
         |_-> failwith("Invalid arguments"))
      else failwith("Type error");;
       

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> (Bool(b||e))
     |_-> failwith("Invalid arguments"))
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> Bool(b&&e)
     |_-> failwith("Invalid arguments"))
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then (match x with
    | Bool(true) -> Bool(false)
    | Bool(false) -> Bool(true)
     |_-> failwith("Invalid arguments"))
  else failwith("Type error");;



 (*Funzione di supporto che restituisce il valore più alto tra quelli passati per argomento*)
  let max x y = if ((typecheck "int" x) && (typecheck "int" y))	|| ((typecheck "string" x) && (typecheck "string" y))			(* Implementazione di 'isGreaterThan' *)
    then ( match (x,y) with
     | (Int(n),Int(u)) -> if(n>u) then Int(n) else Int(u)
     | (String(n),String(u)) -> if(n>u) then String(n) else String(u)
     |_-> failwith("Invalid arguments"))
    else failwith("Type error");;


(*Funzione di supporto che restituisce il valore più basso tra quelli passati per argomento*)
  let min x y = if ((typecheck "int" x) && (typecheck "int" y))	|| ((typecheck "string" x) && (typecheck "string" y))			(* Implementazione di 'isGreaterThan' *)
      then ( match (x,y) with
       | (Int(n),Int(u)) -> if(n<u) then Int(n) else Int(u)
       | (String(n),String(u)) -> if(n<u) then String(n) else String(u)
       |_-> failwith("Invalid arguments"))
      else failwith("Type error");;


(* Funzioni operanti su liste *)


(*Calcola se gli elementi passati per argomento coincidono*)
let rec eqgen x y = match (x,y) with
	
    | (Int(n),Int(u)) -> Bool(n=u)
    | (Bool(n),Bool(u)) -> Bool(n=u)
    | (String(n),String(u)) -> Bool(n=u)
    | (_,_) -> failwith("Invalid argumets")
   ;;


(*Controlla se un elemento è conenuto all'interno della lista*)
let rec exists k l = match l with
  | [] -> false
  | x::ls -> let b = eqgen x k in (match b with
	            |Bool(true)->true
              |Bool(false)->exists k ls
              |_-> failwith("Invalid arguments"));;
 

(*Aggiunge un elemento all'interno della lista*)
let add v l = if exists v l
  then failwith ("Elementa lready present")
  else l @ [v];;




(*Rimuove un elemento dalla della lista*)
let rec remove l v = match l with
  | [] -> failwith ("Element not found")
  | x::ls -> if v = x then (ls)
    else x::remove ls v ;;



(*Funzioni operanti su insiemi *)

(*Controlla se l'elemento è contenuto all'interno dell'insieme*)
let contiene v s = if typecheck "setval" s
  then (match s with
    | ESetVal(l,_) -> Bool( exists v l )
    | _ -> failwith("Invalid arguments")
   )    
  else failwith ("Type error");;


(*Inserisce un elemento all'interno dell'insieme*)
let insert v s = if typecheck "setval" s
  then (match s with
     | ESetVal(l,id) -> if (typecheck id v) then  ( ESetVal(add v l,id) )
			                	else failwith ("Incompatible types") 
     | _ -> failwith("Invalid arguments"))   
  else failwith ("Type error");;


(*Elimina (se c'è) l'elemento dall'insieme*)
let delete v s = if typecheck "setval" s
  then (match s with
    | ESetVal(l,id) -> ESetVal((remove l v),id) 
    | _ -> failwith("Invalid arguments")
    )   
  
  else failwith ("Type error");;



(*Effettua l'operazione di unione insiemistica tra due insiemi*)
let rec unione su sd = if (typecheck "setval" su) && (typecheck "setval" sd)
		then ( match (su,sd) with
			| ( ESetVal([],ideu), ESetVal([],ided) )->  if (ideu=ided) then (ESetVal([],ideu))
						      else failwith("Incompatible types")
			| ( ESetVal(x::xs,ideu), ESetVal(ys,ided) )-> if (ideu=ided) then (insert x ( unione  (ESetVal(xs,ideu)) (ESetVal(ys,ided)) ) )
						      else failwith("Incompatible types")
			| ( ESetVal(xs,ideu), ESetVal(y::ys,ided) )-> if (ideu=ided)  then (insert y (unione (ESetVal(ys,ided)) (ESetVal(xs,ideu))))	
					 		else failwith("Incompatible types")			
		   | (_,_) -> failwith("Invalid arguments")		
         )
		else failwith ("Type error");;



(*Controlla se un insieme è vuoto*)
let isempty s = if (typecheck "setval" s)
  then( match s with
       |ESetVal([],_)-> Bool(true)
       |_-> Bool(false)
       )
else failwith("Type error");;


(*Controlla se un insieme è un sottoinsieme di un altro*)
let rec issubset su sd =  if (typecheck "setval" su) && (typecheck "setval" sd)
  then ( match (su,sd) with
		
| ( ESetVal([],ideu),     ESetVal(_,ided) )->  if (ideu=ided) then  Bool(true)
                                                              else failwith("Incompatible types")
| ( ESetVal(_,ideu),     ESetVal([],ided) )->  if (ideu=ided) then Bool(false)
                                                              else failwith("Incompatible types")
| ( ESetVal(x::xs,ideu), ESetVal(ys,ided) )->  if (ideu=ided) then ( if ( exists x ys ) then ( issubset (ESetVal(xs,ideu)) (ESetVal(ys,ided)) )
                                                                                        else Bool(false))
                                                              else failwith("Incompatible types")	
| (_,_) -> failwith("Invalid arguments")	
     
)
else failwith ("Type error");;


(*Effettua l'operazione di intersezione tra due insiemi*)
let rec intersezione su sd = if (typecheck "setval" su) && (typecheck "setval" sd)
		then ( match (su,sd) with
		
			| ( ESetVal([],ideu), ESetVal(_,ided) )->  if (ideu=ided) then (
										 ESetVal([],ideu))
						      else failwith("Incompatible types")
			| ( ESetVal(_,ideu), ESetVal([],ided) )->  if (ideu=ided) then (
										 ESetVal([],ideu))
						      else failwith("Incompatible types")
			| ( ESetVal(x::xs,ideu), ESetVal(ys,ided) )-> if (ideu=ided) then ( if ( exists x ys ) then ( 
													let u = intersezione (ESetVal(xs,ided)) sd  in 
													if ( (contiene x u)=Bool(false) ) then ( 
																	insert x u ) 
													else u)
											   else intersezione (ESetVal(xs,ideu)) sd
											 )
								      else failwith("Incompatible types")
		  | (_,_) -> failwith("Invalid arguments")			
				   
	  )
		else failwith ("Type error");;




(*Effettua l'operazione di differenza tra due insiemi*)
    let rec differenza su sd = if (typecheck "setval" su) && (typecheck "setval" sd)
      then 
        ( match (su,sd) with
      
           | ( ESetVal([],ideu),    ESetVal(ys,ided) )-> if (ideu=ided) then  ESetVal([],ided)
                                                                        else failwith("Incompatible types")

           | ( ESetVal(xs,ideu),    ESetVal([],ided) )-> if (ideu=ided) then  ESetVal(xs,ideu)
                                                                        else failwith("Incompatible types")

           | ( ESetVal(x::xs,ideu), ESetVal(ys,ided) )-> if (ideu=ided) then ( 
                                                                          if ( exists x ys )  then 
                                                                                            differenza (ESetVal(xs,ideu)) (ESetVal((remove ys x),ideu))     
                                                                                              else  
                                                                                            insert x ( differenza (ESetVal(xs,ideu)) (ESetVal(ys,ided)) )         
                                                                             )
                                                                        else failwith("Incompatible types")		
             
           | (_,_) -> failwith("Invalid arguments")
        )
      else failwith ("Type error");;
  

(*Calcola l'elemento massimo all'interno dell'insieme*)
      let rec maxof x = if (typecheck "setval" x)
        then( match x with
      | ESetVal([],_)->failwith("Empty set")
      | ESetVal(x::[],ide) -> x
      | ESetVal(x::xs,ide) -> let g = maxof (ESetVal(xs,ide)) in max x g 
      |_-> failwith("Invalid arguments"))
    else failwith("Type error");;
        
    
    
  (*Calcola l'elemento minimo all'interno dell'insieme*)  
    let rec minof x = if (typecheck "setval" x)
      then( match x with
    | ESetVal([],_)->failwith("Empty set")
    | ESetVal(x::[],ide) -> x
    | ESetVal(x::xs,ide) -> let g = minof (ESetVal(xs,ide)) in min x g
    |_-> failwith("Invalid arguments"))
    else failwith("Type error");;
    
(*Inizializza un nuovo insieme del tipo passato per argomento*)
let singleton l id =insert l (ESetVal([],id));;



(*Interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with 
  | Estring s-> String s (*Aggiunto*)
  | Eint n -> Int n
  | Ebool b -> Bool b
  | IsZero a -> iszero (eval a r)
  | IsPositive a -> ispositive (eval a r)
  | IsNegative a -> isnegative (eval a r)
  | Den i -> applyenv r i
  | Eq(a, b) -> eq (eval a r) (eval b r)
  | Prod(a, b) -> prod (eval a r) (eval b r)
  | Sum(a, b) -> sum (eval a r) (eval b r)
  | Diff(a, b) -> diff (eval a r) (eval b r)
  | Minus a -> minus (eval a r)
  | And(a, b) -> et (eval a r) (eval b r)
  | Or(a, b) -> vel (eval a r) (eval b r)
  | Not a -> non (eval a r)
  | Ifthenelse(a, b, c) -> let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
        else failwith ("nonboolean guard")
  | Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r))
  | Fun(i, a) -> FunVal(i, a, r)
  | FunCall(f, eArg) -> let fClosure = (eval f r) in
			(match fClosure with
        | FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg (eval eArg r))
        | RecFunVal(g, (arg, fBody, fDecEnv)) -> let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
                eval fBody aEnv
        | _ -> failwith("non functional value"))
  | Letrec(f, funDef, letBody) ->
        		(match funDef with
            		| Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                                               eval letBody r1
                | _ -> failwith("non functional def"))


   (* Parte riguardante gli insiemi *)
  
  | Empty(id) -> if ( id="string" || id="int" ) then ( ESetVal ([],id) )
	                                            	else failwith("Type error")
  | Singleton(l,id) -> let e = eval l r in ( if  ( (id="string") || (id="int") ) && (typecheck id e) then (singleton e id )
                                                else failwith("Type error") )
  | Insert(v,s) -> let vv = eval v r in 
		if (typecheck "string" vv || typecheck "int" vv )then (insert vv (eval s r))
		else failwith("Type error")
  | Delete(v,s) -> delete (eval v r) ( eval s r)  
  | Unione(su,sd) -> unione (eval su r) (eval sd r)
  | Contiene(v,s) -> contiene (eval v r) (eval s r)
  | Intersezione(su,sd) -> intersezione (eval su r) (eval sd r)
  | Differenza (su,sd) -> differenza (eval su r) (eval sd r)
  | IsEmpty(s) -> isempty (eval s r)
  | IsSubSet(su,sd) -> issubset (eval su r) (eval sd r)
  | MaxOf(x) -> maxof (eval x r)
  | MinOf(x) -> minof (eval x r)

  | For_all(predicate, aSet) -> if (typecheck "setval" (eval aSet r)) then ( isforall predicate (eval aSet r) r  )
                                                                   else failwith("Type error")
  | Exists(predicate,aSet) -> if (typecheck "setval" (eval aSet r)) then (isforone predicate (eval aSet r) r)
                                                                else failwith("Type error")
  | Filter(predicate,aSet) -> if (typecheck "setval" (eval aSet r)) then (filter predicate (eval aSet r) r)
                                                                else failwith("Type error")                                                              
  | Map(func,aSet) -> if (typecheck "setval" (eval aSet r)) then (applyall func (eval aSet r) r)
                                                                else failwith("Type error")  
  


(*Restituisce l'insieme dei valori ottenuti applicando la funzione definita a tutti gli elementi dell'insieme*)
     and applyall func s r = match s with
    | ESetVal([],ide) -> ESetVal([],ide)
    | ESetVal(x::xs,ide) ->   insert (funcall2((eval func r),x,r)) (applyall func (ESetVal(xs,ide)) r) 
    | _ -> failwith("Invalid arguments")      

(*Controlla se esiste tutti gli elementi dell’insieme soddisfano la proprietà definita *)
     and isforall func s r = match s with
     | ESetVal([],ide) -> Bool(false)
     | ESetVal(x::[],ide) -> Bool((funcall2((eval func r),x,r))=Bool(true))
     | ESetVal(x::xs,ide) -> Bool(((funcall2((eval func r),x,r))=Bool(true)) && ((isforall func (ESetVal(xs,ide)) r)=Bool(true)))
     | _ -> failwith("Invalid arguments") 

(*Controlla se esiste almeno un elemento dell’insieme che soddisfa la proprietà definita *)
     and isforone func s r = match s with
     | ESetVal([],ide) -> Bool(false)
     | ESetVal(x::[],ide) -> Bool((funcall2((eval func r),x,r))=Bool(true))
     | ESetVal(x::xs,ide) -> if( (funcall2((eval func r),x,r))=Bool(true) ) then Bool(true) 
                            else isforone func (ESetVal(xs,ide)) r
     | _ -> failwith("Invalid arguments") 

(*Funzione che restituisce un insieme di valori che soddisfano la proprietà indicata *)
      and filter func s r = match s with
     | ESetVal([],ide) -> ESetVal([],ide)
     | ESetVal(x::xs,ide) -> if( (funcall2((eval func r),x,r))=Bool(true) ) then insert x  (filter func (ESetVal(xs,ide)) r) 
                            else filter func (ESetVal(xs,ide)) r  
     | _ -> failwith("Invalid arguments") 


 (* Funcall2 non è altro che la stessa Funcall di base ma con gli argomenti passati già valutati *)
   and funcall2 (f,eArg,r) =
         let fClosure = f in
           (match fClosure with
            | FunVal(arg, fBody, fDecEnv) ->
               eval fBody (bind fDecEnv arg eArg ) 
            | RecFunVal(g, (arg, fBody, fDecEnv)) ->
               let aVal = eArg in
                 let rEnv = (bind fDecEnv g fClosure) in
                   let aEnv = (bind rEnv arg aVal) in
                     eval fBody aEnv 
           |  _ -> failwith("non functional value"))


(*Funzione che restituisce una lista di elementi valutati*)
and evalList l r = match l with
  | [] -> []
  | x::ls -> (eval x r)::(evalList ls r)

;;


(* =============================  TESTS  ============================= *)

let env0 = emptyenv Unbound;;

                          (*Creazione di insiemi vuoti*)

let insiemeA0= Empty("string");;
(*output - : evT = ESetVal ([], "string")*)
eval insiemeA0 env0;;

let insiemeB0= Empty("string");;
(*output - : evT = ESetVal ([], "string")*)
eval insiemeB0 env0;;

let insiemeC0= Empty("int");;
(*output - : evT = ESetVal ([], "int")*)
eval insiemeC0 env0;;

let interi0=Empty("int");;
(*output - : evT = ESetVal ([], "int")*)
eval interi0 env0;;

                  (*Controllo se un insieme è vuoto*)

let isempty = IsEmpty(insiemeA0);;
(*output - : evT = Bool true*)
eval isempty env0;;

                  (*Inserimento di valori all'interno degli insiemi*)

let insiemeA1= Insert(Estring "primouno",insiemeA0);;
(*output - : evT = ESetVal ([String "primouno"], "string")*)
eval insiemeA1 env0;;

let insiemeB1= Insert(Estring "primodue",insiemeB0);;
(*output - : evT = ESetVal ([String "primodue"], "string")*)
eval insiemeB1 env0;;

let insiemeC1= Insert(Eint 1,insiemeC0);;
(*output - : evT = ESetVal ([Int 1], "int")*)
eval insiemeC1 env0;;

let interi0=Empty("int");;
(*output - : evT = ESetVal ([], "int")*)
eval interi0 env0;;

let interi1=Insert(Eint 5,interi0);;
(*output - : evT = ESetVal ([Int 5], "int")*)
eval interi1 env0;;

let interi2=Insert(Eint 7,interi1);;
(*output - : evT = ESetVal ([Int 5; Int 7], "int")*)
eval interi2 env0;;

let interi3=Insert(Eint (-5),interi2);;
(*output - : evT = ESetVal ([Int 5; Int 7; Int (-5)], "int")*)
eval interi3 env0;;

let interi4=Insert(Eint (1),interi3);;
(*output - : evT = ESetVal ([Int 5; Int 7; Int 1], "int")*)
eval interi4 env0;;

                          (*Operazione di unione tra insiemi*)

let insiemeunione=Unione(insiemeA1,insiemeB1);;
(*output - : evT = ESetVal ([String "primodue"; String "primouno"], "string")*)
eval insiemeunione env0;;

              (*Operazione di controllo di esistenza di un valore all'interno di un insieme*)

let contieneu1 = Contiene (Estring "primouno",insiemeunione);;
(*output - : evT = Bool true*)
eval contieneu1 env0;;

let contiene3C1= Contiene(Eint 1,insiemeC1);;
(*output - : evT = Bool true*)
eval contiene3C1 env0;;


                          (*Operazione di intersezione tra insiemi*)

let insiemeint1 = Intersezione (insiemeA1,insiemeB1);;
(*output - : evT = ESetVal ([], "string")*)
eval insiemeint1 env0;;

let insiemeint2 = Intersezione (insiemeA1,insiemeunione);;
(*output - : evT = ESetVal ([String "primouno"], "string")*)
eval insiemeint2 env0;;

let insiemeint3 = Intersezione (insiemeC1,insiemeint2);;
(*output Exception: Failure "Incompatible types".*)
eval insiemeint3 env0;;

              (*Operazione che trova il valore massimo all'interno dell'insieme*)

 let maxint = MaxOf(interi4);;
(*output - : evT = Int 7*)
 eval maxint env0;;

 let maxstr = MaxOf(insiemeunione);;
(*output - : evT = String "primouno"*)
 eval maxstr env0;;

             (*Operazione che trova il valore minimo all'interno dell'insieme*)

 let minint = MinOf(interi4);;
(*output - : evT = Int (-5)*)
eval minint env0;;

let minstr = MinOf(insiemeunione);;
(*output - : evT = String "primodue"*)
eval minstr env0;;

            (*Operazione che controlla se un insieme è sottoinsieme di un altro*)

let subset = IsSubSet(insiemeC1,interi4);;
(*output - : evT = Bool true*)
eval subset env0;;
          




                                (*FOR-ALL*)

(** Definisco una funzione che controlla se un valore è 0*)
let is0 = Fun("y", IsZero(Den "y"));;

let apply = For_all(is0,interi4);;
(*output - : evT = Bool false*)
eval apply env0;;

let apply = For_all(is0,Singleton(Eint 0,"int"));;
(*output - : evT = Bool true*)
eval apply env0;;


                                  (*EXISTS*)

let exists0 = Exists(is0, interi4);;
(*output - : evT = Bool false*)
eval exists0 env0;;

(** Definisco una funzione che controlla se un valore è 0*)
let isp = Fun("y", IsPositive(Den "y"));;

let existsp = Exists(isp, interi4);;
(*output - : evT = Bool true*)
eval existsp env0;;


                                  (*MAP*)

 (** Definisco una funzione che incrementa un certo valore di uno *)
let add_by_one = Fun("y", Sum(Den "y", Eint 1));;


let map = Map (add_by_one,interi4);;
(*output - : evT = ESetVal ([Int 2; Int (-4); Int 8; Int 6], "int")*)
eval map env0;;

                                (*FILTER*)

let filterp= Filter(isp, interi4);;
(*output - : evT = ESetVal ([Int 1; Int 7; Int 5], "int")*)
eval filterp env0;;



