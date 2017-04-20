%{
#include <iostream>
#include <iomanip>
using namespace std;

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <assert.h>

#include "tinycomp.h"
#include "tinycomp.hpp"

/* Prototypes - for lex */
int yylex(void);
void yyerror(const char *s);

void printout();

/* Mapping of types to their names */
const char* typestrs[] = {
	"integer",
	"floating point"
};

int TempAddress::counter = 0;

/* Global variables */
Memory& mem = Memory::getInstance();
SimpleArraySymTbl *sym = new SimpleArraySymTbl();
TargetCode *code = new TargetCode();

%}

/* This is the union that defines the type for var yylval,
 * which corresponds to 'lexval' in our textboox parlance.
 */
%union{
	/* tokens for constants */
	int iValue;					/* integer value */
	float fValue;				/* float value */

	/* tokens for other lexemes (var id's and generic lexemes) */
	char idLexeme;				/* identifiers */
	typeName typeLexeme;		/* lexemes for type id's */

	/* types for other syntactical elements: typically, attributes of the symbols */
	Attribute* attrs;			/* attributes for nonterminals */
	int inhAttr;    			/* inherited attribute storing address */
}

%token <idLexeme>ID <iValue>INTEGER <fValue>FLOAT <typeLexeme>TYPE
%token STAT

%token TRUE FALSE

%token WHILE IF PRINT
%nonassoc ELSE

%left OR AND

%left GE LE EQ NE '>' '<'
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS ASSIGN

%type <attrs>expr
%type <attrs>stmt
%type <attrs>stmt_list
%type <attrs>cond

%%
prog:	decls stmt_list 		{
									// add the final 'halt' instruction
									TacInstr *i = code->gen(haltOpr, NULL, NULL);
									code->backpatch(((StmtAttr *)$2)->getNextlist(), i);

									// print out the output IR, as well as some other info
									// useful for debugging
									printout();
								}
	;

decls:	decls decl
	| decl
	;

decl: TYPE id_list ';'
	;

id_list:	id_list ',' ID 	{
														sym->put($3, $<typeLexeme>0);
													}
	   | 	ID 				{
	   								sym->put($1, $<typeLexeme>0);
	   							}
	;

stmt_list:
          stmt ';'          { $$ = $1; }
        | stmt_list
          {$<inhAttr>$ = code->getNextInstr();}
          stmt ';'       	{
				code->backpatch(((StmtAttr *)$1)->getNextlist(), code->getInstr($<inhAttr>2));

				$$ = $3;
			}
        ;

stmt:
	STAT 	{
				code->gen(fakeOpr, NULL, NULL);

				$$ = new StmtAttr();
			}
	| ID ASSIGN expr	{
				VarAddress* var = sym->get($1);
				code->gen(copyOpr, var, ((ExprAttr*)$3)->getAddr());

				$$ = new StmtAttr();
			}
	| WHILE '('
	  {$<inhAttr>$ = code->getNextInstr();}
	  cond ')'
	  {$<inhAttr>$ = code->getNextInstr();}
	  '{' stmt_list '}' {
			/* This is the "while" production: stmt -> WHILE cond '{' stmt_list '}'
			 * Since we're gonna need some backpatches, I'm using inherited attributes.
			 * You should remember them from theory; however their main use here is to
			 * keep track of "interesting addresses" (instructions that we'll use for backpatching).
			 * With embedded inherithed attributes, the production becomes:
			 * stmt -> WHILE <inh_attr1> cond '{' <inh_attr2> stmt_list '}'
			 * The type of an inherited attribute in bison needs to be defined in the
			 * %union structure (it's "inhAttr" in our case), and needs to be specified in the
			 * production (it's the weird "$<inhAttr>$" field, which is initialized to "nextinstr")
			*/

				/* this backpatches $4.truelist (i.e. cond.truelist) to $6.nextinstr
				 * $6 is the second inherited attribute, so its nextinstr is the beginning
				 * of stmt_list
				 */
				code->backpatch(((BoolAttr *)$4)->getTruelist(), code->getInstr($<inhAttr>6));

				/* this generates a goto $3.nexinstr (i.e. "goto cond") */
				TacInstr* i = code->gen(jmpOpr, NULL, NULL, code->getInstr($<inhAttr>3)->getValueNumber());

				/* this backpatches stmt_list.nextlist to the beginning of instruction i
				 * i.e. to the goto we've just generated.
				 */
				code->backpatch(((StmtAttr *)$8)->getNextlist(), i);

				/* Finally, we're creating the attributes for the while statement.
				 * As any other statement, it needs a nextlist; in this case,
				 * the nextlist is the falselist of the cond
				 */
				StmtAttr *attrs = new StmtAttr();
				attrs->addNext(((BoolAttr *)$4)->getFalselist());

				$$ = attrs;
			}
	;

expr:
	INTEGER {
				ConstAddress *ia = new ConstAddress($1);

				$$ = new ExprAttr(ia);
			}
	| FLOAT {
				ConstAddress *ia = new ConstAddress($1);

				$$ = new ExprAttr(ia);
			}
	| ID 	{
				VarAddress *ia = sym->get($1);

				$$ = new ExprAttr(ia);
			}
	| expr '+' expr {
				// Note: I'm not handling all cases of type checking here; needs to be completed

				if ( ((ExprAttr*)$1)->getType() == intType && ((ExprAttr*)$3)->getType() == intType ) {
					TempAddress* temp = mem.getNewTemp(sizeof(int));

					TacInstr* i = code->gen(addOpr, ((ExprAttr*)$1)->getAddr(), ((ExprAttr*)$3)->getAddr(), temp);

					$$ = new ExprAttr(i, intType);
				} else {
				// else ... (all other type combinations should be considered here)
				// ...
				}
			}
	;

cond:
	TRUE 	{
				BoolAttr* attrs = new BoolAttr();

				TacInstr* i = code->gen(jmpOpr, NULL, NULL);
				attrs->addTrue(i);

				$$ = attrs;
			}
	| FALSE {
				BoolAttr* attrs = new BoolAttr();

				TacInstr* i = code->gen(jmpOpr, NULL, NULL);
				attrs->addFalse(i);

				$$ = attrs;
			}
	| cond OR {$<inhAttr>$ = code->getNextInstr();} cond {
				code->backpatch(((BoolAttr *)$1)->getFalselist(), code->getInstr($<inhAttr>3));

				BoolAttr* attrs = new BoolAttr();
				attrs->addTrue(((BoolAttr *)$1)->getTruelist());
				attrs->addTrue(((BoolAttr *)$4)->getTruelist());

				attrs->addFalse(((BoolAttr *)$4)->getFalselist());

				$$ = attrs;
			}
			| expr EQ expr { /*the "if op1 == op2 goto instr" operator */
	 		 		BoolAttr* attrs = new BoolAttr();
	 				if ( ((ExprAttr*)$1)->getType() == intType && ((ExprAttr*)$3)->getType() == intType ) {
	 						TacInstr* i1 = code->gen(eqcondJmpOpr, ((ExprAttr*)$1)->getAddr(), ((ExprAttr*)$3)->getAddr(), NULL);
	 						TacInstr* i2 = code->gen(jmpOpr, NULL, NULL, NULL);
	 						attrs->addTrue(i1);
	 						attrs->addFalse(i2);
	 				} else {
	 					assert(false);
	 				}

	 				$$ = attrs;
	 	 }
	;

%%
void printout() {
	/* ====== */
	cout << "*********" << endl;
	cout << "Size of int: " << sizeof(int) << endl;
	cout << "Size of float: " << sizeof(float) << endl;
	cout << "*********" << endl;
	cout << endl;
	cout << "== Symbol Table ==" << endl;
	sym->printOut();
	cout << endl;
	cout << "== Memory Dump ==" << endl;
	// mem.hexdump();
	mem.printOut(sym);
	cout << endl;
	cout << endl;
	cout << "== Output (3-addr code) ==" << endl;
	code->printOut();
	/* ====== */
}


void yyerror(const char *s) {
    fprintf(stderr, "%s\n", s);
}

int main(void) {
    yyparse();
}
