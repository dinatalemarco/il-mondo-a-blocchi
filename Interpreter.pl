%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START PARSET %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
% Tokenizer
%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(porter_stem)).
tokenize(Sentence, Tokens) :- string_lower(Sentence,PrivSentence),tokenize_atom(PrivSentence,Tokens).



%%%%%%%%%%%%%%%%%%%
% Assertions
%%%%%%%%%%%%%%%%%%%
s(A)					--> assertion(A).
assertion(on(S,P)) 		 	--> verb(_),subj(S),place(P),questionmark(_).
subj(B) 				 	--> block(B).
article(article(the))			--> [the].
blockname(a)				--> [a].
blockname(b)				--> [b].
blockname(c)				--> [c].
preposition(prep(on)) 			--> [on].
questionmark(questionmark(q))		--> [?].
verb(be)		 			--> [is].
word(table)		  		--> [table].
word(block)		 		--> [block].
block(B)		 			--> article(_),word(_),blockname(B). 	% the block a
block(B)		 			--> word(_),blockname(B). 		% block a
block(B) 				--> blockname(B).			% a
place(B)					--> preposition(_),block(B).
place(B)					--> preposition(_),fixplace(B). 		% there is only one fixplace: the table, so discart assignment
fixplace(table)				--> article(_),[table].

%%%%%%%%%%%%%%%%%%%
% Queries
%%%%%%%%%%%%%%%%%%%
s(Q)					--> query(Q).
% 1. Which blocks are on the table?
query(blockson(W))			--> which(WHICH),verb(_),place(W),questionmark(_).
verb(are)				--> [are].
which(which(W))				--> [which],([block];[blocks]). 		% notice AND and OR to force a certain sequence of words

% 2. What color is b?
query(color(B))				--> what(_),subj(B),questionmark(_).
what(what(W))				--> [what],[color],[is].

%%%%%%%%%%%%%%%%%%%
% Commands
%%%%%%%%%%%%%%%%%%%
s(C)					--> command(C).
% 1. Grasp block b
command(grasp(O))			--> [grasp],obj(O).
obj(O)					--> block(O).

% 2. Put it onto the table
command(toplace(TP))			--> [put],[it],toplace(TP).
toplace(FP)				--> [onto],fixplace(FP).

% 3. Put it onto c
toplace(B)				--> [onto],block(B).

% 4. Print state
command(printstate())			--> [print],[state].

% 5. Put b onto c
command(putonto(B1,B2))			--> [put],block(B1),[onto],block(B2).
command(putonto(B1,B2))			--> [put],block(B1),[onto],fixplace(B2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END PARSET %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START INTERPRETER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%
% utility functions
%%%%%%%%%%%%%%%%%%%%

% estrai l'argomento da un predicato.
% per esempio, extract("on(b,a)","on(",Args). ---> Args=[b,,,a]. (",,," perche
% "," è considerato "anything else" (ossia "qualunque cosa") da tokenize_atom() (vedi il documento online per SWI Prolog), che si aspetta spazi )
extract(From,PredName,Args) :- 	substring(PredName,From),
                                string_length(PredName,PredLen),
                                sub_string(From, PredLen, _, 1, TempArgs), 
				tokenize_atom(TempArgs,Args).

% istanzia una lista di atomi per sommare variabili. Perchè questa funzione è fatta per lavorare con extract() (vedi sopra),
% controlla se il tocken è un ',' (vedi la documentazione tokenize_atom() per maggiori informazioni).
% per esempio, instanciate([b,,,c],[Obj,RelObj]). ---> Obj = b; RelObj = c.
instanciate([],L).
instanciate([H|ListToken], [J|ListVars]) :- H\==',',J=H,instanciate(ListToken,ListVars).
instanciate([H|ListToken], [J|ListVars]) :- H==',',instanciate(ListToken,[J|ListVars]).

% ritrae tutto il on(X,Object).
% per esempio, cancelOn([a,b,c],b) = retract(on(a,b)),retract(on(b,b)),retract(on(c,b))
cancelOn([],Object).
cancelOn([H|List],Object) :- retract(on(Object,H)),assert(clear(H)),cancelOn(List,Object).

% ottieni lo stato corretto del mondo dei blocchi
getstate(State)	:- findall(clear(X),clear(X),L1),
		   findall(block(X),block(X),L2),
		   append(L1,L2,ResL1),
		   findall(on(X,Y),on(X,Y),L3),
		   append(ResL1,L3,State).

% ottieni lo stato risultante da on(Object,RelativeObject).
getstate(Object, RelativeObject, Final)	:- getstate(State),				% prima, controlla che il movimento abbia senso(è ammissibile?)
					   member(clear(Object),State),
					   member(clear(RelativeObject),State),
					   findall(on(Object,X),on(Object,X),Delete),	% computa il nuovo stato
					   findall(clear(X),on(Object,X),NewClear),
					   append(Delete,[clear(RelativeObject)],NewDelete),
					   subtract(State,NewDelete,Res),
					   append(Res,NewClear,Res1),
					   append(Res1,[on(Object,RelativeObject),clear(Object),clear(table)],Final).

% vero se X è sottostringa di S.
substring(X,Y) :- forall(sub_atom(X,_,1,_,C), sub_atom(Y,_,1,_,C)).

% stampa il risultato di Planner.pl in Inglese
translateEnglish([]).
translateEnglish([H|Steps]) :- term_string(H,Str),
			       extract(Str,"move(",Args),
			       instanciate(Args,[Block,FromOn,ToOn]),
			       nl,
			       write('Move block '),
			       write(Block),
			       write(' from '),
			       write(FromOn),
			       write(' onto '),
			       write(ToOn),
			       write('.'),
			       translateEnglish(Steps).

translateEnglish([H|Steps]) :-	term_string(H,Str),
			        extract(Str,"moveToTable(",Args),
				instanciate(Args,[Block,FromOn]),
				nl,
				write('Move block '),
				write(Block),
				write(' from '),
				write(FromOn),
				write(' onto the table.'),
			        translateEnglish(Steps).

%%%%%%%%%%%%%%%%%%%%
% Interprete
%%%%%%%%%%%%%%%%%%%%

% Assersione: Is b on c?
interpret(Stringtree)   :- extract(Stringtree,"on(",Args),
			   instanciate(Args,[Object,RelativeObject]),
			   !, % the ! is needed if on(X,Y) is false, in order not to backtrack
			   ( on(Object,RelativeObject) -> writeln( 'Yes.') ; (writeln('No.'),fail) ).

% Query: Which blocks are on the table?
interpret(Stringtree)   :- extract(Stringtree,"blockson(",Args),
			   instanciate(Args,[Object]),
			   !,
			   write("Blocks on it are: "),
			   findall(X,on(X,Object),L),
			   write_list(L).

% Query: What color is b?
interpret(Stringtree)   :- extract(Stringtree,"color(",Args),
			   instanciate(Args,[Object]),
			   !,
			   write("The color is: "),
			   color(Object,Color),
			   write(Color).

% Command: Grasp b
interpret(Stringtree)   :- extract(Stringtree,"grasp(",Args),
			   instanciate(Args,[Object]),
			   !,
			   ( \+clear(Object) -> write('Cannot grasp it because the block is not on top'), fail ; write('') ),
			   ( \+holding(nothing) -> write('Cannot grasp it. Already grasping block '), holding(X), write(X), fail ; write('') ),
			   retract(holding(nothing)),
			   assert(holding(Object)),
			   assert(lastgrasped(Object)),
			   retract(clear(Object)),
			   findall(X,on(Object,X),L), 		% block Object is no more on any block 
			   cancelOn(L,Object),			% b is no more on any other block
			   write('0K, holding '), 
			   holding(X), 
			   write(X).

% Command: Put it onto c/onto the table. Questa è una combinazione con Graps
interpret(Stringtree)   :- extract(Stringtree,"toplace(",Args),
			   instanciate(Args,[RelativeObject]),
			   !,
			   ( \+clear(RelativeObject) -> write('Sorry, '),write(RelativeObject), write(' is not on top'),fail; write('')),
			   ( holding(nothing) -> write('Sorry, grasping nothing'), fail ; write('') ),
			   holding(Object),		        % il blocco correntemente afferrato
			   retract(holding(Object)),		% non afferrare nulla
			   assert(holding(nothing)),
			   retract(lastgrasped(_)),
			   assert(lastgrasped(nothing)),
			   retract(clear(RelativeObject)),	% l' Object blocco è in cima a c
			   assert(clear(table)),			% il tavolo è sempre pulito (necessario per metterlo sul tavolo)
		           assert(clear(Object)),
			   assert(on(Object,RelativeObject)),
			   write('0K, put '), 
			   write(Object), 
			   write(' on '), 
			   write(RelativeObject).

% Command: Print state
interpret(Stringtree)	:- extract(Stringtree,"printstate(",DiscartedArgs),	% controlla command
			   !,
			   findall(X,block(X),LBlocks),
			   write('blocks: '),write(LBlocks),
			   findall([B,C],color(B,C),L),
			   nl,
			   write('colors: '),write_list(L),
			   findall(X,clear(X),L1),
			   nl,
			   write('clear: '),
			   write_list(L1),
			   findall([X,Y],on(X,Y),L2),
			   nl,
			   write('on relations: '),
			   write_list(L2),
			   holding(X),
			   nl,
			   write('holding: '),
			   write(X).

% Command: Put a onto b 
% Nota che questo non modifica lo stato del mondo a blocchi, restituisce semplicemente i passaggi per raggiungere quello stato!
interpret(Stringtree)	:- extract(Stringtree,"putonto(",Args),
			   instanciate(Args,[Object,RelativeObject]),
			   !,
			   getstate(Init),
			   write('Current state: '),
			   write_list(Init),
			   getstate(Object,RelativeObject, Final),
			   nl,
			   write('Final state:'),
			   write_list(Final),
			   !,
			   solve(Init,Final,Steps),	% vedi Planner.pl
			   write('Steps: '),
			   write(Steps),
			   nl,
			   translateEnglish(Steps).

% Se nessuno dei precedenti tentativi di interpretazione fallisce, mostra un messaggio di errore
interpret(_) :- 	nl,writeln("Sorry, I do not understand what you say...").