%%%%%%%%%%%%%%%%%%%%%%%
% Importo i file di lavoro
%%%%%%%%%%%%%%%%%%%%%%%

:- consult('Planner.pl').
:- consult('Interpreter.pl').


%%%%%%%%%%%%%%%%%%%%%%%
% Utility functions
%%%%%%%%%%%%%%%%%%%%%%%
% writes a list 
write_list([]).
write_list([H|List]) :- write(H),write(", "),write_list(List).

% calls a predicate in Interpreter.pl that returns the parse tree of sentence (e.g., "toplace(put,b).")
parse(Sentence, Tree) :- s(Tree, Sentence, []).


%%%%%%%%%%%%%%%%%%%%%%%
% Main program
%%%%%%%%%%%%%%%%%%%%%%%

help :- writeln("Assertion:"),
	writeln(" 1) 'Is b on the table?'"),
	writeln(" 2) 'Is b on c?'\n"),
	writeln("Command:"),
	writeln(" 1) 'Grasp block b'"),
	writeln(" 2) 'Put it onto the table'"),
	writeln(" 3) 'Put it onto c'"),
	writeln(" 4) 'Print state'"),
	writeln(" 5) 'Put a onto b'\n"),
	writeln("Query:"),
	writeln(" 1) 'Which blocks are on the table?'"),
	writeln(" 2) 'What color is b?'").


start:- 	write("\nDammi un istruzione : "),
	read(Request),
	tokenize(Request, Tokens),
	parse(Tokens, Tree),
	term_string(Tree,String),
	interpret(String).
		
