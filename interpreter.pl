/***
A skeleton for Assignment 3 on PROP HT2019 at DSV/SU.
Peter Idestam-Almquist, 2019-12-09.
***/

/*** 
Load the tokenizer (tokenize/2) and the file_writer (write_to_file/3).
***/
:- [tokenizer].
:- [filewriter].


/***
The top level predicate run/2 of the solution.
To be called like this:
?- run('program1.txt','myparsetree1.txt').
***/
run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 
	write_to_file(OutputFile,ParseTree,VariablesOut).
	
/***
parse(-ParseTree)-->
	A grammar defining your programming language,
	and returning a parse tree.
***/

/* WRITE YOUR CODE FOR THE PARSER HERE */
	
	
parse(ParseTree) --> block(ParseTree).

block(block(L_curly, Stmts, R_curly)) -->  left_curly(L_curly), statement(Stmts), right_curly(R_curly).

statement(statement) --> [].
statement(statement(Assign, Stmts)) --> assign(Assign), statement(Stmts).

assign(assign(Id, Equal, Expr, Semicolon)) --> ident(Id), assign_op(Equal), expr(Expr), semicolon(Semicolon).

expr(expr(Term)) --> term(Term).
expr(expr(Term, Add_op, Expr) --> term(Term), add_op(Add_op), expr(Expr).
expr(expr(Term, Sub_op, Expr) --> term(Term), sub_op(Sub_op), expr(Expr).

term(term(Factor)) --> factor(Factor).
term(term(Factor, Mult_op, Term)) --> term(Factor), mult_op(Mult_op), term(Term).
term(term(Factor, Div_op, Term)) --> term(Factor), div_op(Div_op), term(Term).

factor(factor(Int) --> int(Int).
factor(factor(Ident) --> ident(Ident). 
factor(factor(Left_Paren, Expr, Right_Paren) --> left_paren(Left_Paren), expr(Expr), right_paren(Right_Paren).

right_curly(right_curly) --> ['}'].
left_curly(left_curly) --> ['{'].
ident(ident(Ident)) --> [Ident], {atom(Ident)}.
assign_op(assign_op) --> [=].
semicolon(semicolon) --> [;].
add_op(add_op) --> [+]. 
sub_op(sub_op) --> [-].
div_op(div_op) --> [/].
mult_op(mult_op) --> [*].
right_paren(right_paren) -->[)].
left_paren(left_paren) --> [(].
int(int(Int)) --> [Int], {integer(Int)}.
	
	
	
/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
***/

/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
