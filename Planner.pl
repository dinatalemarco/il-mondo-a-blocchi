%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Present in SWI-Prolog, it allows to use functions like append, and member for lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definisco lo stato inizia 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

block(a).				% Definisco il blocco A
block(b).				% Definisco il blocco B
block(c).				% Definisco il blocco C
color(b, red).				% Il blocco B è di colore rosso
color(a, green).				% Il blocco A è verde
color(c, yellow).			% il blocco C è giallo 
clear(table).

:- assert(on(c,a)).			% C è sul blocco a
:- assert(on(a,table)).			% A è sul tavolo
:- assert(clear(c)).			% C non ha blocchi su di se
:- assert(on(b,table)).			% B è sul tavolo
:- assert(clear(b)).			% B non ha blocchi su di se
:- assert(lastgrasped(nothing)).

:- assert(holding(nothing)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% funzione utilità
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alldif([]).
alldif([E|Es]) 				:- maplist(dif(E), Es),alldif(Es).

apply([], State).
apply([H|Preconditions],State) 		:- member(H, State), apply(Preconditions,State).

find_missing([], To, Res).								% caso base
find_missing([H|From],To,Res) 		:- member(H,To),find_missing(From,To,Res). 	% salta elementi
find_missing([H|From], To, [H|Res])     :- find_missing(From, To, Res). 			% aggiungi l'ogetto H a Res

																				% naiverev() from "Learn Prolog Now!, http://www.learnprolognow.org/"
naiverev([],[]).
naiverev([H|T],R) :- naiverev(T,RevT),  append(RevT,[H],R). 

write_list([]).
write_list([H|T]) :- writeln(H),write_list(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% algoritmo per la risoluzione
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Azioni possibili per il problema STRIPS. 
action(	move(B,X,Y),					% Nome delle azioni
	[block(B),block(Y),on(B,X),clear(B),clear(Y)], 	% Precondizioni
	[on(B,Y),clear(X)],				% Effetti
	[on(B,X),clear(Y)],				% Effetti: cose non più valide in State
	[B,X,Y] ).					% Variabili usate. Ne ha bisogno alldif()

action( moveToTable(B,X),				% Nome delle azioni
	[on(B,X),clear(B),block(B),block(X)],		% Precondizioni
	[on(B,table),clear(X)],				% Effetti
	[on(B,X)],					% Effetti: cose non più valide in State
	[B,X] ).						% Variabili usate. Ne ha bisogno alldif()



% casi impossibili
solve([], Goal, Steps). % non ce ne è bisogno per arrivare all'obiettivo. Questo è impossibile per raggiungerlo
solve(Init, [], Steps). % non ce ne è bisogno per arrivare all'obiettivo. Questo è impossibile per raggiungerlo

solve(State, Goal, Steps) :- apply(State,Goal),		% caso base della ricorsione
			     naiverev(Steps,StepsReverse),
			     Steps=StepsReverse,
			     write_list(StepsReverse).
											% stato dato (il corrente), lo stato dell'obiettivo e la lista di passi che tu ti stai aspettando:
solve(State, Goal, [Name|Steps]) :- action(Name, Preconditions, Effects, Old, Vars),	% prendi un'azione possibile
				    alldif(Vars),					% ( se alcune variabili sono uguali in action => l'assignamento non corrente )
				    apply(Preconditions, State),				% se questa precondizione applicata allo stato corrente
				    subtract(State, Effects, NewState),			% applica i suoi effetti (per esempio, rimuovi gli effetti da state e aggiungi la nuova informazione)...
				    subtract(NewState, Old, NewState2),
				    find_missing(Effects, NewState2,Missing),
				    append(NewState2, Missing,NewState3),
				    subtract(NewState3, [clear(table)], NewState4),	% (altrimenti "table" potrebbe essere aggiunta all'azione 1, errata)
				    solve(NewState4,Goal,[Name|Steps]).			% ...e aggiungi il movimento alla soluzione, che è Steps


