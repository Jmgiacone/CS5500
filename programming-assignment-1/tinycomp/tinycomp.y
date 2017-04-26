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
	"floating point",
        "fraction"
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
  fraction fractValue; //Fraction Value

	/* tokens for other lexemes (var id's and generic lexemes) */
	char idLexeme;				/* identifiers */
	typeName typeLexeme;		/* lexemes for type id's */

	/* types for other syntactical elements: typically, attributes of the symbols */
	Attribute* attrs;			/* attributes for nonterminals */
	int inhAttr;    			/* inherited attribute storing address */
}

%token <idLexeme>ID <iValue>INTEGER <fValue>FLOAT <fractValue>FRACTION <typeLexeme>TYPE
%token STAT

%token TRUE FALSE

%token WHILE IF THEN PRINT
%nonassoc ELSE

%left OR AND

%left GE LE EQ EXACT_EQ NE '>' '<'
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
        cout << "ID ASSIGN expr" << endl;
				VarAddress* var = sym->get($1);

                                if(var == NULL)
                                {
                                  yyerror("Error! Undeclared variable!");
                                  exit(-1);
                                }

                                ExprAttr* expr = (ExprAttr*)$3;
                                
                                if(var->getType() == expr->getType())
                                {
                                  //They're the same type, assign as normal
                                  code->gen(copyOpr, var, expr->getAddr());
                                }
                                else
                                {
                                  //They're different types
                                  
                                  //Check the type of the LHS, that's the type we're aiming for
				  TempAddress* temp;
                                  switch(var->getType())
                                  {
                                    case intType:
                                      //None of the types can be promoted to ints
                                      cout << "Type " << expr->getType() << " cannot be converted to int!" << endl;
                                      exit(-1);
                                      break;
                                    case floatType: 
                                      switch(expr->getType())
                                      {
                                        case intType:
                                          cout << "Type promotion from int to float not yet implemented" << endl;
                                          exit(-1);
                                          break;
                                        case fractionType:
					  cout << "Type promotion from fraction to float not yet implemented" << endl;
                                          exit(-1);
                                          break;
                                        default:
                                          cout << "Type " << expr->getType() << " cannot be converted to float!" << endl;
                                          exit(-1); 
                                          break;
                                      }  
                                      break;
                                    case fractionType:
                                      switch(expr->getType())
                                      {
                                        case intType:
                                          cout << "Promoting int to fraction" << endl;
                                          //TODO: Promote the int to a fraction
                                          //t = new Temp()
                                          //t[0] = expr.addr
                                          //t[4] = 1
                                          temp = mem.getNewTemp(2 * sizeof(int));
			                  code->gen(indexCopyOpr, new ConstAddress(0), expr->getAddr(), temp);
                                          code->gen(indexCopyOpr, new ConstAddress((int)sizeof(int)), new ConstAddress(1), temp);

                                          code->gen(copyOpr, var, temp);
                                          break;
                                        default:
                                          cout << "Type " << expr->getType() << " cannot be converted to fraction!" << endl;
                                          exit(-1);
                                          break;
                                      }  
                                      break;
                                  }  
                                }
                                /*if(var->getType() == fractionType)
                                {
                                  if(expr->getType() == fractionType)
                                  {
                                    //Copy it straight
                                    code->gen(copyOpr, var, expr->getAddr());
                                  }
                                  else if(expr->getType() == intType)
                                  {
                                    cout << "Promoting int to fraction" << endl;
                                    //TODO: Promote the int to a fraction
                                    //t = new Temp()
                                    //t[0] = expr.addr
                                    //t[4] = 1
                                    TempAddress* temp = mem.getNewTemp(2 * sizeof(int));
			            code->gen(indexCopyOpr, new ConstAddress(0), expr->getAddr(), temp);
                                    code->gen(indexCopyOpr, new ConstAddress((int)sizeof(int)), new ConstAddress(1), temp);

                                    code->gen(copyOpr, var, temp);
                                  }
                                  else
                                  {
                                    //Can't promote anything else to a fraction
                                    cout << "Error! Can't promote " << expr->getType() << " to fraction" << endl;
                                    assert(false);
                                  }
                                }
                                else
                                {
                                  code->gen(copyOpr, var, expr->getAddr());
                                }*/
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
        | IF '(' cond ')' THEN {$<inhAttr>$ = code->getNextInstr();} '{' stmt_list '}'
          {
            cout << "if statement" << endl;
            //Create the attribute for this statement
            StmtAttr* attrs = new StmtAttr();

            //Patch cond.true to the beginning of stmt_list
            code->backpatch(((BoolAttr*)$3)->getTruelist(), code->getInstr($<inhAttr>6));

            //Patch stmt_list.next to stmt.next
            attrs->addNext(((StmtAttr*)$8)->getNextlist());

            //Patch cond.false to stmt.next
            attrs->addNext(((BoolAttr*)$3)->getFalselist());

            $$ = attrs;
          }
	;

expr:
INTEGER 
{
	cout << "int expression" << endl;
	ConstAddress *ia = new ConstAddress($1);

	$$ = new ExprAttr(ia);
}
| FLOAT 
  {
	ConstAddress *ia = new ConstAddress($1);

	$$ = new ExprAttr(ia);
			}
| FRACTION
  {
	cout << "Found a fraction" << endl;
        //t = new Temp()
        //t[0] = f.num
        //t[4] = f.denom
	fraction f = $1;

	//TODO: Use the 'gen' function here? Grab a temp address of 2*int and do it that way?
        TempAddress* t = mem.getNewTemp(2 * sizeof(int));
        code->gen(indexCopyOpr, new ConstAddress(0), new ConstAddress(f.numerator), t);
        code->gen(indexCopyOpr, new ConstAddress((int)sizeof(int)), new ConstAddress(f.denominator), t);
	
	$$ = new ExprAttr(t, fractionType);
  }
| ID 	
  {
	  VarAddress *ia = sym->get($1);

	  $$ = new ExprAttr(ia);
  }
| expr '+' expr 
  {
	// Note: I'm not handling all cases of type checking here; needs to be completed

	if ( ((ExprAttr*)$1)->getType() == intType && ((ExprAttr*)$3)->getType() == intType ) 
        {
		TempAddress* temp = mem.getNewTemp(sizeof(int));

		TacInstr* i = code->gen(addOpr, ((ExprAttr*)$1)->getAddr(), ((ExprAttr*)$3)->getAddr(), temp);

		$$ = new ExprAttr(i, intType);
	} 
        else 
        {
	  // else ... (all other type combinations should be considered here)
	  // ...
	}
  }
| expr '*' expr
  {
    cout << "expr * expr" << endl;
    //TODO: Promote ints to do the multiplication? Just multiply the numerator?
    if ( ((ExprAttr*)$1)->getType() == fractionType && ((ExprAttr*)$3)->getType() == fractionType)
    {
      TempAddress* temp = mem.getNewTemp(2 * sizeof(int));
      TempAddress* tempNum1 = mem.getNewTemp(sizeof(int));
      TempAddress* tempDenom1 = mem.getNewTemp(sizeof(int));
      TempAddress* tempNum2 = mem.getNewTemp(sizeof(int));
      TempAddress* tempDenom2 = mem.getNewTemp(sizeof(int));
      TempAddress* tempProduct1 = mem.getNewTemp(sizeof(int));
      TempAddress* tempProduct2 = mem.getNewTemp(sizeof(int));
	
      //t0 = e1[0]
      //t1 = e2[0]
      //t2 = e1[4]
      //t3 = e2[4]
      //t4 = t0 * t1
      //t5 = t2 * t3
      //t6[0] = t4
      //t6[4] = t5

      //TODO: Make sure this is good
      code->gen(offsetOpr, ((ExprAttr*)$1)->getAddr(), new ConstAddress(0), tempNum1);
      code->gen(offsetOpr, ((ExprAttr*)$3)->getAddr(), new ConstAddress(0), tempNum2);
      code->gen(offsetOpr, ((ExprAttr*)$1)->getAddr(), new ConstAddress((int)sizeof(int)), tempDenom1);
      code->gen(offsetOpr, ((ExprAttr*)$3)->getAddr(), new ConstAddress((int)sizeof(int)), tempDenom2);
      code->gen(mulOpr, tempNum1, tempNum2, tempProduct1);
      code->gen(mulOpr, tempDenom1, tempDenom2, tempProduct2);
      code->gen(indexCopyOpr, new ConstAddress(0), tempProduct1, temp);
      code->gen(indexCopyOpr, new ConstAddress((int)sizeof(int)), tempProduct2, temp);

      $$ = new ExprAttr(temp, fractionType);
    }  
  }
	;

cond:
  TRUE 	
  {
	BoolAttr* attrs = new BoolAttr();

	TacInstr* i = code->gen(jmpOpr, NULL, NULL);
	attrs->addTrue(i);

	$$ = attrs;
  }
| FALSE 
  {
	BoolAttr* attrs = new BoolAttr();

	TacInstr* i = code->gen(jmpOpr, NULL, NULL);
	attrs->addFalse(i);

	$$ = attrs;
  }
| cond OR {$<inhAttr>$ = code->getNextInstr();} cond 
  {
	code->backpatch(((BoolAttr *)$1)->getFalselist(), code->getInstr($<inhAttr>3));

	BoolAttr* attrs = new BoolAttr();
	attrs->addTrue(((BoolAttr *)$1)->getTruelist());
	attrs->addTrue(((BoolAttr *)$4)->getTruelist());

	attrs->addFalse(((BoolAttr *)$4)->getFalselist());

	$$ = attrs;
  }
| expr EQ expr 
  {
    /*the "if op1 == op2 goto instr" operator */
    BoolAttr* attrs = new BoolAttr();
    if ( ((ExprAttr*)$1)->getType() == intType && ((ExprAttr*)$3)->getType() == intType ) 
    {
	TacInstr* i1 = code->gen(eqcondJmpOpr, ((ExprAttr*)$1)->getAddr(), ((ExprAttr*)$3)->getAddr(), NULL);
	TacInstr* i2 = code->gen(jmpOpr, NULL, NULL);
	attrs->addTrue(i1);
	attrs->addFalse(i2);
    }
    else if (((ExprAttr*)$1)->getType() == fractionType && ((ExprAttr*)$3)->getType() == fractionType)
    {
      cout << "Comparing fractions for the same rational number" << endl;

      //t0 = e1[0]
      //t1 = e1[4]
      //t2 = e2[0]
      //t3 = e2[4]
      //t4 = t0/t1
      //t5 = t2/t3
      //if t4 == t5

      TempAddress* fract1Num = mem.getNewTemp(sizeof(int));
      TempAddress* fract1Denom = mem.getNewTemp(sizeof(int));
      TempAddress* fract2Num = mem.getNewTemp(sizeof(int));
      TempAddress* fract2Denom = mem.getNewTemp(sizeof(int));
      TempAddress* fract1RatNum = mem.getNewTemp(sizeof(float));
      TempAddress* fract2RatNum = mem.getNewTemp(sizeof(float));

      code->gen(offsetOpr, ((ExprAttr*)$1)->getAddr(), new ConstAddress(0), fract1Num);
      code->gen(offsetOpr, ((ExprAttr*)$1)->getAddr(), new ConstAddress((int)sizeof(int)), fract1Denom);
      code->gen(offsetOpr, ((ExprAttr*)$3)->getAddr(), new ConstAddress(0), fract2Num);
      code->gen(offsetOpr, ((ExprAttr*)$3)->getAddr(), new ConstAddress((int)sizeof(int)), fract2Denom);

      code->gen(divOpr, fract1Num, fract1Denom, fract1RatNum);
      code->gen(divOpr, fract2Num, fract2Denom, fract2RatNum);

      TacInstr* finalResultTrue = code->gen(eqcondJmpOpr, fract1RatNum, fract2RatNum, NULL);
      TacInstr* finalResultFalse = code->gen(jmpOpr, NULL, NULL);

      attrs->addTrue(finalResultTrue);
      attrs->addFalse(finalResultFalse);

    }
    else 
    {
      assert(false);
    }

    $$ = attrs;
  }
| expr EXACT_EQ expr
  {
	 BoolAttr* attrs = new BoolAttr();

	 if(((ExprAttr*)$1)->getType() == fractionType && ((ExprAttr*)$3)->getType() == fractionType)
	 {
	   cout << "Comparing fractions for the exact same fraction" << endl;

           //t0 = e1[0]
	   //t1 = e1[4]
           //t2 = e2[0]
           //t3 = e2[4]
           //if t0 == t2 goto
           //if t1 == t3 goto

           TempAddress* fract1Num = mem.getNewTemp(sizeof(int));
           TempAddress* fract1Denom = mem.getNewTemp(sizeof(int));
           TempAddress* fract2Num = mem.getNewTemp(sizeof(int));
           TempAddress* fract2Denom = mem.getNewTemp(sizeof(int));

	   code->gen(offsetOpr, ((ExprAttr*)$1)->getAddr(), new ConstAddress(0), fract1Num);
           code->gen(offsetOpr, ((ExprAttr*)$1)->getAddr(), new ConstAddress((int)sizeof(int)), fract1Denom);
           code->gen(offsetOpr, ((ExprAttr*)$3)->getAddr(), new ConstAddress(0), fract2Num);
           code->gen(offsetOpr, ((ExprAttr*)$3)->getAddr(), new ConstAddress((int)sizeof(int)), fract2Denom);

	   TacInstr* compareNumerators = code->gen(eqcondJmpOpr, fract1Num, fract2Num, NULL);
	   TacInstr* failedComparison1 = code->gen(jmpOpr, NULL, NULL, NULL);
	   TacInstr* compareDenominators = code->gen(eqcondJmpOpr, fract1Denom, fract2Denom, NULL);
	   TacInstr* failedComparison2 = code->gen(jmpOpr, NULL, NULL, NULL);

	   //TODO: Backpatch the numerator instruction to jump to the next one when true
	   list<TacInstr*> l;
	   l.push_front(compareNumerators);

	   code->backpatch(l, compareDenominators);

	   attrs->addTrue(compareDenominators);
	   attrs->addFalse(failedComparison1);
	   attrs->addFalse(failedComparison2);
	 }
	 else
	 {
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
