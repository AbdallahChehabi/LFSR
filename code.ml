(*premiére implémentation *)
type binary = Zero | One  ;;
type poly = (int * binary) list;;

(**-------------------------------------------outils----------------------------------------------------------- *)

let int_of_binary b = 
    match b with 
    |Zero -> 0
    |One -> 1 
;;

let binary_of_int n =
    match n with
    |0 ->  Zero
    |1 ->  One
    |_ ->  failwith "Erreur type"
;;
 
 (** [a]p:={n∈Z∣n≡amodp} *)

let sumBinary (a : binary) (b : binary) : int = 
    match (a,b) with
    |(One,One) -> 0
    |(a,b) -> (int_of_binary a) + (int_of_binary b) 
;;

let multBinary (a : binary) (b : binary) : int = 
    match (a,b) with
    |(a,_) -> (int_of_binary a)
    |(_,b) -> (int_of_binary b)
    |(a,b) -> ((int_of_binary a) * (int_of_binary b))
;;

let somme_poly_in_z2 = fun a b ->
	(a + b) mod 2 
;;

let sousrtr_in_z = fun a b ->
	(a - b) mod 2
;;

let multi_in_z2 = fun a b -> 
    (a * b) mod 2 
;; 
 
let d = somme_poly_in_z2 4 8 ;;
print_endline "-------------------------------------";
Printf.printf "%i\n" d ;;

let multBinary2 = fun a b ->
 ((int_of_binary a) * (int_of_binary b))
;;

let rec print_tuples =
  function
  | [] -> ()
  | (a, b) :: rest ->
    Printf.printf "(%i, %i)" a (int_of_binary b);
    print_tuples rest;;

let p = [(1, Zero);(1, One); (2, One); (3, Zero); (4, One)];;
print_endline "---------------test print_tuples-----------------------" ;; 
print_tuples p ;;
print_endline "" ;;
print_endline "-------------------------------------------------------" ;;

(** 1-1 = 0 --> -1 mod 2 = 1 *)

let oppositFunction p  = 
  let rec inverse_poly p  acc =
  match p with 
    |[] -> List.rev acc 
    |(degree,coeff) :: q -> if coeff = One then inverse_poly q ((degree,One) :: acc)
    else inverse_poly q ((degree,One) :: acc)
  in inverse_poly p [];;

let p = [(0, One);(1, One); (2, One)];;

print_endline "------------------------Test oppositFunction------------------------------" ;;
print_tuples p ;;
print_tuples (oppositFunction p);;
print_endline "" ;;
print_endline "-------------------------------------------------------" ;;

let print_poly (p : poly * poly) =
  match p with
  | [],[] -> print_tuples []
  | a,b  ->
    print_endline "A0";
    print_tuples a;
    print_endline "";
    print_endline "A1";
    print_tuples b;;

(**fonction pour le degree de polynôme *)

let rec degre (p: poly) = 
  match p with
  | [] -> 0
  | (degree, coeff)::[]-> degree
  | (_, _) :: q-> degre q;;

let p = [(0, Zero);(1, One); (2, One); (3, Zero); (4, One)];;
Printf.printf "fonction de degre";;
Printf.printf "\n";;
Printf.printf "%i" (degre p);;
Printf.printf "\n";;

(**-------------------------------------------------------Fin Outils----------------------------------------------------- *)

(**-----------------------------------------------------la Somme de deux Pôlynome Début---------------------------------- *)

let somme_poly (p:poly) (q:poly) : poly = 
  let rec aux p q acc = match p, q with
    | [], [] -> List.rev acc
    | [], f -> List.append (List.rev acc) f 
    | f, [] -> List.append (List.rev acc) f
    | (degree, coeff) :: f1, (degree2, coeff2) :: f2 when degree < degree2 ->
      aux f1 ((degree2, coeff2)::f2)  ((degree, coeff)::acc)
    | (degree, coeff) :: f1, (degree2, coeff2) :: f2 when degree > degree2 ->
      aux ((degree, coeff) :: f1) f2  ((degree2, coeff2)::acc)
    | (degree, coeff) :: f1, (degree2, coeff2) :: f2 when degree = degree2 ->
    (*aux f1 f2  ((degree2,(binary_of_int(sumBinary coeff coeff2)))::acc)*)
      if (sumBinary coeff coeff2) <> 0 then aux f1 f2  ((degree2,(binary_of_int(sumBinary coeff coeff2)))::acc) 
      else aux f1 f2  acc
    | (_::_, _::_) -> failwith "Erreur somme_poly, valeur inconnue"
  in aux p q [];;


(*Test de la fonction de la some *)

let p = [(1, Zero);(1, One); (2, One); (3, Zero); (4, One)];;
let q = [(1, Zero);(1, One); (2, One); (3, One); (4, Zero)];;

print_endline "test somme polynomes" ;;
print_endline "polynomes" ;;
print_tuples p ;; 
print_endline "";;
print_tuples q ;; 
print_endline "";; 
print_endline "test somme" ;; 
print_tuples (somme_poly p q);;
print_endline "";;
print_endline "-------------------------------------------------------" ;;
(**-------------------------------------------------------------------Fin Somme---------------------------------------------- *)
(* -----------------------------------------fonction auxilières pour la methode karatsuba et division------------------------------- *)
(**multiplication par un coefficient de type binary *)
let multCoeff (p:poly) (nb : binary) : poly = 
  let rec aux p acc =
    match p with
    | [] -> List.rev acc
    | (degree, coeff) :: q -> aux q ((degree, binary_of_int (multBinary2 nb coeff)) :: acc)
  in aux p [];;

let p = [(0, One);(1, One); (2, One); (3, Zero); (4, One)];;

Printf.printf "fonction multCoeff";;
Printf.printf "\n";;
print_tuples (multCoeff p One);;
Printf.printf "\n";;

(**multiplication par un coefficient de type int*)

let multCoeffint (p:poly) (n : int) : poly = 
  let rec aux p acc =
    match p with
    | [] -> List.rev acc
    | (degree, coeff) :: q -> aux q ((degree, (binary_of_int (multi_in_z2 n (int_of_binary coeff)))) :: acc)
  in aux p [];;

let p = [(0, One);(1, One); (2, One); (3, Zero); (4, One)];;

print_endline "----test multicoefint--------";;
print_tuples (multCoeffint p 4) ;; 
print_endline "---------------------------" ;;

(**multiplication de monôme *)

let multXn (p:poly) (n:int) =
	let rec aux p acc = 
		match p with
			| [] -> List.rev acc
			| (d1, c1)::q ->  aux q ((d1 + n, c1)::acc)
	in aux p [];;

let monome0 = 0;;
let monome1 = 1;;
let monome2 = 2;;

let p = [(0, One);(1, One); (2, One); (3, Zero); (4, One)];;

Printf.printf "fonction multiXn";;
Printf.printf "\n";;
print_tuples (multXn p monome0);;
Printf.printf "\n";;
print_tuples (multXn p monome1);;
Printf.printf "\n";;
print_tuples (multXn p monome2);;
Printf.printf "\n";;

let rec multNaive (polyA:poly) (polyB:poly) =
	let rec aux polyA acc =
		match polyA with
			| []-> acc
			| (d, c) :: p ->  somme_poly (aux p (multCoeff (multXn polyB d) c)) acc
	in aux polyA [];;

  let p = [(0, One);(1, One); (2, One)];;
  let q = [(0, One);(1, One); (2, One)];;

print_endline "--------------------test multNaive----------------------------------" ;;
print_endline "polynomes" ;;
print_tuples p ;; 
print_endline "";;
print_tuples q ;; 
print_endline "";; 
print_endline "test";;
print_tuples (multNaive p q);;
print_endline "";;

let cut (p:poly) (k : int)  = 
  let rec aux p acc1 acc2 = 
    match p with
    | [] ->  List.rev acc1, List.rev acc2
    | (degree, w) :: q when degree < k -> aux q ((degree, w) :: acc1) acc2
    | (degree, w) :: q -> aux q acc1 ((degree - k, w) :: acc2)
  in aux p [] [];;

let p = [(0, One);(1, One); (2, One); (3, Zero); (4, One)];;

print_endline "-------------------------------------------fonction cut------------------------------------------";;
print_tuples p ;; 
print_endline "";;
print_poly (cut p 3);;
Printf.printf "\n";;

let even p q = 
	degre p < 2 && degre q < 2 
;;

let rec karatsuba (polyA:poly) (polyB:poly) =
	if even polyA polyB  then multNaive polyA polyB
	else
			let k = (1 + max (degre polyA) (degre polyB))/2 in
			(**PolyA = a0 + k/2a1 *) 
			let a0, a1 = cut polyA k in
			(**PolyB = b0 + k/2a0 *)
			let b0, b1 = cut polyB k in
			(*c0 = a0 * b0 *)
			let c0 = karatsuba a0 b0 in
			(*c2 = a1*b1 *)
			let c2 = karatsuba a1 b1 in
			(*u = (a0+a1)*(b0 + b1) *)
			let u = karatsuba (somme_poly a0 a1) (somme_poly b0 b1) in
			(*c1 = u - c0 - c2 *) 
			let c1 = somme_poly u  (somme_poly c0 c2) in 
			(*c = c0 + k/2c1 + k/2c2 *)
			somme_poly (somme_poly c0 (multXn c1 k)) (multXn c2 (k * 2));;

let p = [(0, One);(1, One); (2, One)];;
let q = [(0, Zero);(1, One); (2, One)];;

print_endline "-------------------------Test  Karatsuba--------------------------" ;;
print_endline "polynomes" ;;
print_tuples p ;; 
print_endline "";;
print_tuples q ;; 
print_endline "";;
print_endline "resultat:";;
print_tuples (karatsuba p q);;
print_endline "";;
print_endline "-------------------------------------------------------------------";;

(**---------------------------------------------------------Fin Multiplication------------------------------------------- *)

(**-------------------------------------------------------Methodes pour la division------------------------------------  *)
let inverse (p:poly):poly = 
		let rec aux p acc =
			match p with
			| [] -> acc
			| (d, c) :: q -> aux q ((-d, c) :: acc)
		in aux p [];;

let renverse k (p:poly) = 
	if k >= degre p then
		multXn (inverse p) k
	else failwith "Impossible de calculer le renverse";;

let moduloXn (p:poly) n = 
	if n > degre p then p
		else fst (cut p n);;

let inverse_mod_poly (polyA:poly) (polyB:poly) = 
	let bn = (degre polyB) in
	let p = multCoeffint (renverse (degre polyB) polyB)  bn in
	let rec aux (acc:poly) acc2 = 
		if acc2 > (degre polyA) - (degre polyB) + 1 then acc
		else 
			aux (moduloXn (somme_poly (multCoeffint acc 2) (karatsuba p (karatsuba acc acc))) acc2) (2 * acc2)
	in aux [(0, One)] 1;;

let quotient_poly (polyA:poly) (polyB:poly) = 
	let bn = degre polyB in
	let g = inverse_mod_poly polyA polyB in
    let m = degre polyA and n = degre polyB in
	renverse (m - n) (moduloXn (karatsuba (renverse m polyA) (multCoeffint g bn)) (m - n + 1));;

let p = [(0, One);(1, One); (2, One)];;
let q = [(0, Zero);(1, One); (2, One)];;

print_tuples (inverse_mod_poly p q);;








