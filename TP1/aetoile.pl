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

main :-
    initial_state(S0),
    heuristique(S0,H0),
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
	writeln("Pas de solution: l'état final n'est pas atteignable!").

aetoile(Pf, Ps, Qs) :-
	final_state(Fin),
	suppress_min([[F,H,G],U],Pf,NewPf),
	(U = Fin ->
		suppress([Fin, [F2, H2, G2], Pere, Act], Ps, _New_Pu),
		insert([Fin, [F2, H2, G2], Pere, Act], Qs, NewQs),
		affiche_solution(NewQs, U) 
	;
		suppress([U,[F,H,G],Pere,Act],Ps,NewPs),
		expand(U,G,List),
		loop_successors(List,NewPf, NewPs,Qs,Pf_2, Ps_2),
		insert([U,[F,H,G],Pere,Act],Qs,NewQs),
		aetoile(Pf_2,Ps_2,NewQs)
	).
	


expand(U,G,Y):-
		findall([Next,[Fs,Hs,Gs],U,New_Act],expand_cost(U,G,Gs,Next,Hs,Fs,New_Act),Y).

expand_cost(U,G,Gs,Next,Hs,Fs,New_Act):-
	rule(New_Act,1,U,Next),
	Gs is G+1,
	heuristique(Next,Hs),
	Fs is Gs+Hs.


loop_successors([],Pf,Ps,_,Pf, Ps).

loop_successors([[U,[F,H,G],Min,Act]|Rest],Pf,Ps,Qs,New_pf2, New_ps2):-
	(belongs([U,_,_,_],Qs)->
		writeln("DEJA FAIT"),
		loop_successors(Rest,Pf,Ps,Qs,New_pf2, New_ps2)
	;
		(belongs([U,_,_,_],Ps)->
			update([U,[F,H,G],Min,Act],Ps,Pf,Ps_2,Pf_2), 
			loop_successors(Rest,Pf_2,Ps_2,Qs,New_pf2, New_ps2)
		;
			insert([U,[F,H,G],Min,Act],Ps,Ps_2),
			insert([ [F,H,G], U],Pf,Pf_2),
			loop_successors(Rest,Pf_2,Ps_2,Qs,New_pf2, New_ps2)
		)
	).
  
update([U,[F,H,G],Min,Act],Pu,Pf,Pu_2,Pf_2):-
	suppress([U,[F1,H1,G1],Pere,A],Pu,New_Pu),
	(F1 > F -> 
		insert([U,[F,H,G],Min,Act],New_Pu,Pu_2),
		suppress([U,[F1,H1,G1]],Pf,New_Pf),
		insert([[F,H,G],U],New_Pf,Pf_2)
	;
		insert([U,[F1,H1,G1],Pere,A],New_Pu,Pu_2),
		Pf_2 = Pf
	).



affiche_solution(Qs, U) :-
	belongs([U, _, nil, nil], Qs),
	write("\n========= Solution du taquin ========\n\n"),
	write_state(U).

affiche_solution(Qs, U) :-
	belongs([U, _, Pere, Act], Qs),
	Pere \= nil,
	affiche_solution(Qs, Pere),

	write("\n\t|\n\t"),
	write(Act),
	write("\n\t|\n"),
	write("\tv\n"),
	write_state(U).


%********************************** Test Unitaire ***********************************************%
test_expand:-
    initial_state(Ini),
    expand(Ini,0,List),
    write_state(List).

test_loop_successor:-
    initial_state(S0),

    empty(Pf),
    empty(Ps),
    empty(Qs),

    insert([[H0,H0,0],S0],Pf,NewPf),
    insert([S0,[H0,H0,0],nil,nil],Ps,NewPs),

    writeln("OLD PF"),
    put_flat(NewPf),
    nl,
    nl,
    suppressmin([[,_,G],U],NewPf,Pf_2),
    expand(U,G,List),
    loop_successors(List,Pf_2, NewPs,Qs,Pf_3, _Ps_2),
    writeln("NEW PF"),
    put_flat(Pf_3).
