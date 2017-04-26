%{
#include <stdlib.h>
#include "tinycomp.h"
#include "tinycomp.tab.h"

void yyerror(const char *);
%}

%option noyywrap

/* regular definitions */
intconst        0|[1-9][0-9]*
floatconst      {intconst}\.[0-9]*
fractionconst   {intconst}\|{intconst}

%%

"int"       {
                yylval.typeLexeme = intType;
                return TYPE;

            }

"float"     {
                yylval.typeLexeme = floatType;
                return TYPE;

            }

"fraction"  {
                yylval.typeLexeme = fractionType;
                return TYPE;
            }

"stat"      {
                return STAT;
            }

">="            return GE;
"<="            return LE;
"=="            return EQ;
"="             return EXACT_EQ;
"!="            return NE;

":="            return ASSIGN;

"||"            return OR;
"&&"            return AND;


"while"         return WHILE;
"if"            return IF;
"then"          return THEN;
"else"          return ELSE;
"print"         return PRINT;

"true"          return TRUE;
"false"         return FALSE;

[a-z]       {
                yylval.idLexeme = yytext[0];
                return ID;
            }

{intconst}  {
                yylval.iValue = atoi(yytext);
                return INTEGER;
            }

{floatconst} {
                yylval.fValue = atof(yytext);
                return FLOAT;
            }
{fractionconst} {
                  //TODO: Fix this
                  //Declare num and denom
                  int counter = 0;
                  char numString[25];
                  char denomString[25];
                  bool pipeFound = false;

                  //Search for the pipe               
                  for(int i = 0; i < strlen(yytext); i++)
                  {
                    //printf("%c", yytext[i]);
                    if(yytext[i] == '|')
                    {
                      //printf("Found pipe!");
                      pipeFound = true;
                      numString[counter] = '\0';     
                      counter = 0;                 
                    }
                    else if(!pipeFound)
                    {
                      numString[counter] = yytext[i];
                      counter++;
                    }
                    else
                    {
                      denomString[counter] = yytext[i];
                      counter++;
                    }                    
                  }
                  denomString[counter] = '\0';
                  //printf("\n");
                  //printf("Num String: %s\n", numString);
                  //printf("Denom String: %s\n", denomString);

                  fraction f;
                  f.numerator = atoi(numString);
                  f.denominator = atoi(denomString);

                  //printf("Num: %d\n", f.numerator);
                  //printf("Denom: %d\n", f.denominator);

                  yylval.fractValue = f;
                  return FRACTION;
                }

[-()<>=+*/,;{}.] {
                return *yytext;
             }

"//".*          { /* Skip 1-line comments */ }

[ \t\n]+        ;       /* ignore whitespace */

.               {
                    printf("Unknown character %x\n", yytext);
                    const char* err = "Unknown character";
                    //yyerror(err);
                }


%%
