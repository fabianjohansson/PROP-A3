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
    %evaluate(ParseTree,[],VariablesOut),
    write_to_file(OutputFile,ParseTree,VariablesOut).
   
/***
parse(-ParseTree)-->
    A grammar defining your programming language,
    and returning a parse tree.
***/
 
parse(ParseTree) --> block(ParseTree).
 
block(block(Left_curly,Stmts,Right_curly)) -->
    left_curly(Left_curly), stmts(Stmts), right_curly(Right_curly).

stmts(stmts) --> [].	
stmts(stmts(Assign,Stmts)) -->
    assign(Assign), stmts(Stmts).

 
assign(assign(Id,Eq,Expression,Semi)) -->
    ident(Id), assign(Eq), expression(Expression), semicolon(Semi).
 
expression(expression(Term)) -->
    term(Term).
expression(expression(Term,(Add_op ; Sub_op),Expression)) -->
    term(Term), (add_op(Add_op); sub_op(Sub_op)), expression(Expression).
expression(expression(Term,Sub_op,Expression)) -->
    term(Term), sub_op(Sub_op), expression(Expression).
 
term(term(Factor)) -->
    factor(Factor).
term(term(Factor,Div_op,Term)) -->
    factor(Factor), div_op(Div_op), term(Term).
term(term(Factor,Mult_op,Term)) -->
    factor(Factor), mult_op(Mult_op), term(Term).
 
factor(factor(Int)) -->
    int(Int).
factor(factor(Ident)) -->
    ident(Ident).
factor(factor(Left_paren, Expression, Right_paren)) -->
    left_paren(Left_paren), expression(Expression), right_paren(Right_paren).
 
 
ident(ident(Ident)) --> [Ident],{atom(Ident)}.
assign(assign) --> [=].
add_op(add_op) --> [+].
sub_op(sub_op)--> [-].
mult_op(mult_op) --> [*].
div_op(div_op) -->[/].
int(int(Int)) --> [Int],{integer(Int)}.
left_paren(left_paren) --> ['('].
right_paren(right_paren) --> [')'].
left_curly(left_curly) --> ['{'].
right_curly(right_curly) --> ['}'].
semicolon(semicolon) --> [;].

/***
evaluate(+ParseTree,+VariablesIn,-VariablesOut):-
    Evaluates a parse-tree and returns the state of the program
    after evaluation as a list of variables and their values in
    the form [var = value, ...].
***/
 
/* WRITE YOUR CODE FOR THE EVALUATOR HERE */
 
