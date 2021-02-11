%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche   
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************


%******************************************************************************

main :-
    initial_state(S0),
    heuristique2(S0,H0),
	% initialisations Pf, Pu et Q 
	empty(Pf),
	empty(Ps),
	empty(Q),
	insert([[H0,H0,0],S0],Pf,NewPf),
	insert([S0,[H0,H0,0],nil,nil],Ps,NewPs),
	aetoile(NewPf,NewPs,Q).
	% lancement de Aetoile.  




%*******************************************************************************


aetoile(Pf, Ps, _) :-
	empty(Pf),
	empty(Ps),
	write("Pas de solution: l'état final n'est pas atteignable!").

aetoile(Pf, [], Qs) :-
	final_state(Fin),
	suppress_min(Min,Pf,_),
	[_,U] is Min,
	Fin is U,
	write_state(U),
	write_state(Qs).

aetoile(Pf, Ps, Qs) :-
	suppress_min(Min,Pf,NewPf),
	[[F,H,G],U] is Min,	
	suppress_min([U,[F,H,G],_,_],Ps,NewPs),
	expand(U,List),
	loop_successors(List,NewPf, NewPs,Qs),
	insert(U,Qs,NewQs),
	aetoile(NewPf,NewPs,NewQs).
	


expand([U,[F,H,G],Min,_],List):-
		findall([Next,[F,H,G],Min,Act],rule(Act,1,U,Next),Y),
		expand_cost(Y,List).

expand_cost([[U,[_,_,G],Min,Act]|Rest],list1):-
	Gs is G+1,
	heuristique2(U,Hs),
	Fs is Gs+Hs,
	Etat = [U,[Fs,Hs,Gs],Min,Act],
	expand_cost(Rest,list2),
	list1 = [Etat|list2].

expand_cost([],_).

loop_successors([],_,_,_).

loop_successors([[U,[F,H,G],Min,Act]|Rest],Pf,Ps,Qs):-
	belongs(U,Qs)-> loop_successors(Rest,Pf,Ps,Qs);
	belongs([U,_,_,_],Ps)-> update([U,[F,H,G],Min,Act],Ps,Pf,New_Ps,New_Pf), loop_successors(Rest,New_Pf,New_Ps,Qs);
	insert([[F,H,G],U],Pf,New_Pf),insert([U,[F,H,G],Min,Act],Ps,New_Ps),loop_successors(Rest,New_Pf,New_Ps,Qs).


   
update([U,[F,H,G],Min,Act],Pu,Pf,Pu_2,Pf_2):-
	suppress([U,[F1,H1,G1],Pere,A],Pu,New_Pu),
	F1 > F -> insert([U,[F,H,G],Min,Act],New_Pu,Pu_2),suppress([U,[F1,H1,G1]],Pf,New_Pf),insert([[F,H,G],U],New_Pf,Pf_2);
	insert([U,[F1,H1,G1],Pere,A],New_Pu,Pu_2), Pf_2 is Pf.

