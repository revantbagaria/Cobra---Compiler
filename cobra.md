MATH 221 FALL 2016
------------------

the Cobra compiler
=====================

*A compiler for a Python-like language (Cobra) written in Standard ML*

**Due**: November 16th, 2016

This SML code implements a recursive descent parser
for programs written in a Python-like language called
Cobra. Like most Python programs, Cobra programs 
consist of a series of function definitions followed
by lines of code at the bottom (the Cobra "script")
that do the work of the program. The language has
loops, conditionals, assignment statements, `print`
and `input`, etc.

Here is the full grammar for the language:
	<prgm> ::= <defs> <stmt>
	
	<defs> ::= <defn>
	         | <defn> <defs>
	
	<defn> ::= def <name> ( <name>, ..., <name> ) : <stmt>
	
	<stmt> ::= <name> = <expn>
	         | <name> ( <expn>, ... , <expn> )
	         | print ( <expn> )
	         | while <cond>: <stmt>
	         | if <cond>: <stmt> else: <stmt>
	         | return <expn>
	         | pass
	         | begin <block> end
	
	<blck> ::= <stmt>
	         | <stmt> <blck>
	
	<cond> ::= <expn> < <expn>
	<cond> ::= <expn> == <expn>
	
	<expn> ::= <sum>
	
	<sum> ::= <product> <rest-of-sum>
	<rest-of-sum> ::= .
	                | + <product> <rest-of-sum>
	                | - <product> <rest-of-sum>
	
	<product> ::= <term> <product-terms>
	<rest-of-product> ::= .
	                    | * <term> <rest-of-product>
	                    | // <term> <rest-of-product>
	                    | % <term> <rest-of-product>

	<term> ::= <number>
	         | <name>
	         | <name> ( <expn>, ... , <expn> )
	         | input ( )
	         | ( <expn> )

Note that identation and spacing is not important
in Cobra the way it is in Python. In Python, you 
use tabbed indentation to indicate which blocks of 
code are associated with which constructs (`def`, 
`while`, `if`, etc.). Instead, a Cobra programmer
uses a `begin ... end` block in function definitions, 
`while` loops, and `if` statements when those constructs
need to run several lines of code. For example, like so

	  def factorial(n):
	  begin
	    i = 1
	    x = 1
	    while i <= n:
	    begin
	      x = xi
	      i = i + 1
	    end
	    return x
	  end

You'll find several Cobra example programs
(all ending with `.cob`) in this folder. Take
a look at them to get a better sense of 
Cobra syntax.
    
The SML code defines a function:

	parse_source: string -> prgm

which takes the name of a Cobra source file, 
opens it, converts it to a sequence of tokens, 
consumes that sequence, and then returns a 
syntax tree representing the program.  The 
tokens are printed as they are consumed to 
help you debug any syntax errors in your
Cobra programs.

You'll want to take a look at the `datatype`
declaration defining `prgm`, `stmt`, `expn`,
etc. This collection of datatypes define the
structure of syntax trees that we use to 
represent Cobra programs.

The `parse_source` function relies on several 
helper parsing functions:

	  parse_prgm : tokens -> prgm
	  parse_stmt : tokens -> stmt
	  parse_blck : tokens -> stmt list
	  parse_expn : tokens -> expn

These each consume a portion of the tokens at
that front of that sequence when those tokens 
conform to the associated grammar rule. They each 
raise a `SyntaxError` exception if the tokens don't
follow the expected syntax.

Please note: they are not all complete! I've left the 
job of parsing `while` and `if` statements to you. 
These are supposed to be parsed within `parse_stmt`.
(See the "ASSIGNMENT" section below, under Exercise 1).

The code also defines a compiler for Cobra programs.
This will inspect a source file, parse it, then convert
its syntax tree into a sequence of machine instructions.  
We do so by an intermediate step. There is a `compile`
function that takes an expression tree (a term of type
`prgm`) and converts it to a list of `ir` terms.
That is to say, `compile` has the type signature

	compile : prgm -> ir list

We define a `datatype ir` which defines a collection of
machine instructions as terms.  An example might be
something like

	(SUB ("x","y","z"))

which represents an instruction that takes the values
stored in registers named `y` and `z`, subtracts them, and
then puts the result into a register named `x`. Another
example is

	(GOTOIF ("x",LT0,"end_of_loop"))

which represents an instruction that checks the value stored 
in a register named `x`, sees if it is negative, and then
jumps to an instruction labeled as `end_of_loop`.

What this means is that our compiler, rather than directly 
spitting out the code of some machine language program, instead
uses a dtat structure of type `ir list` (a list of these 
`ir` terms) as an "intermediate representation" or "IR" for 
the compiled code. Once in this form (because we have been
careful in our definition of `ir`) that list can be easily
converted into machine instructions (for a "target" machine
like a Ha75 computer, or an Intel x86 processor,etc). 

Thus, what you'll see is the definition of a function

	compile_source: string -> ir list

which takes a file name of a Cobra script and gives back a 
list of these instruction terms. You can type, for example,

	compile_source "test2.cob";

after loading the file `cobra.sml` into SML. It will then
spit back a list of `ir` terms, with an interaction like

	- compile_source "test5.cob";
	val it =
	  [LABEL "main",SET ("tmp1",10),SET_ARG (1,"tmp1"),CALL "sqr",SET ("tmp2",10),
	   SET_ARG (1,"tmp2"),CALL "succ",SET ("main0",0),SET_RET "main0",RET,
	   LABEL "succ",GET_ARG (1,"x"),SET ("y",1),ADD ("tmp3","x","y"),
	   SET_RET "tmp3",RET,LABEL "sqr",GET_ARG (1,"x"),MUL ("tmp4","x","x"),
	   SET_RET "tmp4",RET] : ir list

If you look carefully at this generated list, it
corresponds to a "pseudo-assembly" of the Cobra 
program, including line labels, and individual 
machine instructions---all terms from the datatype `ir`.

To keep things simple, we don't worry about using 
real register names in the code that we generate.
Instead, we let our compiler make up any names 
that it likes, ones that are convenient for 
compilation. The compiler will makes up register names 
and label names on the fly. We imagine that the 
conversion of the `ir` terms into actual machine code 
will also involve an additional "register allocation"
stage, replacing the made-up names with the actual 
registers of the machine we are programming with 
Cobra code. We've skipped this stage and instead
keep the made-up register names like `x`, `tmp3`,
etc.

The compiler code, as is, relies on five functions:

	gen_prgm : prgm -> ir list
	gen_defn : defn -> ir list
	gen_blck : (stmt list) -> ir list
	gen_stmt : stmt -> ir list
	gen_expn : string -> expn -> ir list

which each generate a sequence of machine instruction
terms that correspond to the syntax tree they are given, 
either the whole program, a function definition, a block 
of statements (just a `stmt list`), a statement, or an 
expression. Note the different form of the signature
for the `gen_expn` function. It takes *two* arguments,
a register name (a `string`) along with an expression
tree (an `expn`). It gives back machine code that ultimately
places the result of its expression into the register named 
by the string of the first argument.

Two of the functions, namely `gen_stmt` and `gen_expn`, are 
incomplete.  Right now, `gen_stmt` fails to properly compile 
a WHILE statement. And `gen_expn` fails to completely compile
addition and subtraction trees (`PLUS` and `MINUS` terms of type
`expn`). You will fix these problems in Exercises 3 and 2 below.

Finally, a remark on how all this is put together. The code, as
provided, can compile lots of different Cobra programs---just
not ones with `while` and `if` statements, or compilcated 
additions and subtractions---into runnable Ha75 code(!). I've
written a modified Ha75 simulator as a Python program 
`pseudoHa75.py` that you can use to run your compiled `.cob`
programs. To do this, you need only run a working `cobra.sml`
in SML with an additional `.cob` filename in the argument to
`sml`, like so:

	sml cobra.sml test4.cob

This will run the Cobra compiler on the given Cobra program
and produce a file named `test4.pha`. This code will be made
up of Ha75 pseudoassembly (one that uses all sorts of names
for registers, like `x` and `tmp5`). If you then run the
command

	python3 pseudoHa75.py test4.pha

you will see that code run!

There are limits right now to what will compile. If a program
uses `*`, `//`, or `%` then the compiler will produce 
intermediate code that includes "library" Cobra functions
that compute products, quotients, and remainders with loops 
and `if` statements (see the file `lib.cobl`). Since the 
compiler isn't set up yet to parse `if` and `while` these 
kind of Cobra programs will fail to compile, raising an
`Unimplemented` exception.

---


**ASSIGNMENT**
==============


Exercise 1: parsing
-------------------

Complete `parse_stmt` so that it parses `if-else` statements
and `while` statements. You'll probably want to add a helper
function `parse_cond` that parses the condition that is used
by these two conditional statement types. You can test it
by running functions like `parse`, which parses a source code
string directly, and gives back a tree. Your tests can be
source code strings like

	"begin i=10 if i<11: print(1) else: print(0) end"

which corresponds to the more carefully formatted Cobra
program

	begin 
	  i=10 
	  if i<11: 
	    print(1) 
	  else: 
	    print(0) 
	end

The resulting parse should produce a syntax tree with no function 
definitions, just a list of statements terms form that script.

Once you feel like your additions are working, then test it
with made-up `.cob` programs. Come up with a variety of test 
programs, and make sure that `parse_source` generates the 
correct tree for each of them. 

Be thoughtful with your tests. You will be graded by the kind of
test programs you write.

Exercise 2: compiling expressions
---------------------------------

The `gen_expn` function takes two arguments---an expression term
along with the name of a "target" register. It should produce a
series of machine instructions (each an `ir` term) ending with
an instruction that assigns a computed result to the target
register.

For example, if the expression parsed is (3+4) - (5+6) it could
produce something like

	[SET("tmp100",3),
	 SET("tmp101",4),
	 ADD("tmp102","tmp100","tmp101"),
	 SET("tmp103",5),
	 SET("tmp104",6),
	 ADD("tmp105","tmp103","tmp103"),
	 SUB("target","tmp102","tmp105")]

when `gen_expn` is given `"target"` as the first argument and

	(MINUS(PLUS(NUM 3,NUM 4),PLUS(NUM 5,NUM 6)))

as the second argument.

Right now, the `gen_expn` function only works for
integer constants, lookups of variables' values, 
function calls, and products and divisions.
Note that multiplication and division gets
converted to shifts `MUL2` and `DIV2`
in the case where the second argument is a 
`NUM 2`. It gets converted into function
calls to "mul", "div", "mod" which are
defined in the library source code `lib.cobl`
also in this project folder.

The `gen_expn` code does not work fully with
sums and differences.  Your job is to complete
that code. Just to be suggestive, I've worked
out the simple cases where we need to generate
the `ir` term for a sum or a difference of 
two variable `LOOKUP` terms. Those are as simple
as issuing the correct `ADD` or `SUB` instruction
term from the `ir` datatype.

For more general sums and differences, where you 
have two expression subtrees `e1` and `e2`, you'll
need to complete the code so that it generates two
new register names (see below), recursively call
`gen_expn` twice to build the `ir` lists whose
code evaluates `e1` and `e2`, placing their results
into those two registers, and then does a final
`ADD` or `SUB` of those two registers' values into
the specified register `x`.

To do this work, your code will need to make up 
register names to use as temporaries that serve as 
places where intermediate calculations' results are 
placed.  To generate these names, you can use the 
expression:

	(gen_tmp ())

and you'll get back a new string like "tmp101".

Again, generate `.cob` programs that test your 
compiler. Make sure that the `ir` sequences that
`compile_source` produces are correct.


Exercise 3: compiling statements
--------------------------------

I've written the `gen_stmt` code so that it handles
all the statement types, including `if` statements,
but not including the work for `while` statements.
Your job is to complete `gen_stmt` so that it handles 
`WHILE` terms.

To do this work, your code will need use the `GOTO`
pseudo-assembly instructions defined as `ir` to have 
the code jump to, or conditionally jump to, another 
part of the code. There is a `LABEL` instruction whose 
string can be the target of these jump instructions. 
You'll need to generate novel label names as you 
generate this code.  If you use the expression

	(gen_lbl "skip")

you'll get back a novel string like "skip35", and
if you use

	(gen_lbl "loop")

you'll get back a novel string like "loop415".

Both `IF` and `WHILE` rely on the parsed `cond` expression.
That term is either a `LESS` term or an `EQUAL` term.
These can then be translated, when handling `IF` and
`WHILE`, into `GOTOIF` terms with one of the register-
checking `ir` conditions. If, for example, you want to
jump to the label "skip237" when a register value
"tmp678" is not equal to zero, you would use the
instruction 

	(GOTOIF ("skip237", NEQ0, "tmp678"))

Take a look at how I convert an `if` statment into
a series of `ir` lists. I generate two labels,
one to skip ahead to the `then` code (skipping the
`else` code) when the condition holds, and one to 
skip past the `then` code at the end of the `else`
code. Thus I generate an `ir` list that corresponds
to the pseudo-Ha code:

	  # evaluate the left side of the condition
	  ...
	  x1 = ...the code from gen_expn...
	  # evaluate the right side of the condition
	  ...
	  x2 = ...the code from gen_expn...
	  x = x1 - x2
	  if x < 0 go to lt
 	  ...the code for the else...
      go to la
    lt:
	  ...the code for the then...
	la:

You will need to do something similar to compile all
the subtree parts of a `WHILE` term, one for the
`LESS` condition, and one for the `EQUAL` condition.

---

Having done all the above, write a bunch of Cobra
programs that convince yourself, and me, that your
compiler works! Generate their `.pha` files, and 
run them with `pseudoHa75.py`.




