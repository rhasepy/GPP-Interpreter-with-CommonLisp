%{
	#include <stdio.h>
	#include <string.h>
	#include "tokens.h"

	int oc_cc_count = 0;
%}

%%
[aA][nN][dD]					return KW_AND;		
[oO][rR]						return KW_OR;			
[nN][oO][tT]					return KW_NOT;			
[eE][qQ][uU][aA][lL]			return KW_EQUAL;
[lL][eE][sS][sS]				return KW_LESS;
[nN][iI][lL]					return KW_NIL;
[lL][iI][sS][tT]				return KW_LIST;
[aA][pP][pP][eE][nN][dD]		return KW_APPEND;
[cC][oO][nN][cC][aA][tT]		return KW_CONCAT;
[sS][eE][tT]					return KW_SET;
[dD][eE][fF][fF][uU][nN]		return KW_DEFFUN;
[fF][oO][rR]					return KW_FOR;
[iI][fF]						return KW_IF;
[eE][xX][iI][tT]				return KW_EXIT;
[lL][oO][aA][dD]				return KW_LOAD;
[dD][iI][sS][pP]				return KW_DISP;
[tT][rR][uU][eE]				return KW_TRUE;
[fF][aA][lL][sS][eE]			return KW_FALSE;

[+]								return OP_PLUS;
[-]								return OP_MINUS;
[/]								return OP_DIV;
[*][*]							return OP_DBLMULT;
[*]								return OP_MULT;
[(]								return OP_OP;
[)]								return OP_CP;
["] 							{
									if(oc_cc_count % 2 == 0){

										++ oc_cc_count;
										return OP_OC;

									}	else{

										++oc_cc_count;
										return OP_CC;
									}
								}
[,]								return OP_COMMA;
[;][;]							return COMMENT;
[0][0-9]						return UNKNOWN;
[0-9]							return VALUE;
[a-zA-Z_][a-zA-Z0-9_]*			return IDENTIFIER;
[1-9][0-9]*						return VALUE;
[\n]							return NEWLINE;
[0-9a-zA-z_][a-zA-Z0-9_]*		return UNKNOWN;
[ \t]							return WHITESPACE;
.								return UNKNOWN;
%%

int yywrap(void) { 

	return 1;
}