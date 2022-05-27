/*
    Muharrem Ozan Yeşiller 171044033
*/

%{
	#include <stdlib.h>
	#include "utils.h"
	#include "y.tab.h"
%}

%%
;;(.*) {
    return COMMENT;
}

"+" {
    return OP_PLUS;
}

"-" {
    return OP_MINUS;
}

"/" {
    return OP_DIV;
}

"*" {
    return OP_MULT;
}

"**" {
    return OP_DBLMULT;
} 

"," {
    return OP_COMMA;
}

"(" {
    return OP_OP;
}

")" {
    return OP_CP;
}

"and" {
    return KW_AND;
}

"or" {
    return KW_OR;
}

"not" {
    return KW_NOT;
}

"equal" {
    return KW_EQUAL;
}

"less" {
    return KW_LESS;
}

"nil" {
    return KW_NIL;
}

"true" {
    return KW_TRUE;
}

"false" {
    return KW_FALSE;
}

"list" {
    return KW_LIST;
}

"append" {
    return KW_APPEND;
}

"concat" {
    return KW_CONCAT;
}

"set" {
    return KW_SET;
}

"defvar" {
    return KW_DEFVAR;
}

"for" {
    return KW_FOR;
}

"if" {
    return KW_IF;
}

"load" {
    return KW_LOAD;
}

"disp" {
    return KW_DISP;
}

"while" {
    return KW_WHILE;
}

"deffun" {
    return KW_DEFFUN;
}

"exit" {
    return KW_EXIT;
}

\"*\" {
    return STRING;
}

"'" {
    return OP_Q;
}

[0][0-9]+ {
    printf("%s Syntax Error!\n",yytext);
}

[0-9]+[a-zA-Z]+.* {
    printf("%s Syntax Error!\n",yytext);
}

[a-zA-Z][a-zA-Z0-9]* {
    yylval.expression = strdup(yytext);
	return IDENTIFIER;
}

[0]{1} {
    yylval.opValue = 0;
    return VALUE;
}

[1-9][0-9]* {
	yylval.opValue = atoi(yytext);
	return VALUE;	
}

[+][0-9A-zA-Z]* {
    printf("%s Syntax Error!\n", yytext);
}

[-][0-9A-zA-Z]*	{
    printf("%s Syntax Error!\n", yytext);
}

[/][0-9A-zA-Z]* {
    printf("%s Syntax Error!\n", yytext);
}

[*][0-9A-zA-Z]* {
    printf("%s Syntax Error!\n", yytext);
}

[,][0-9A-zA-Z]* {
    printf("%s Syntax Error!\n", yytext);
}

[\t\n\r ] {}

. {
    printf("%s Syntax Error!\n",yytext);
};
%% 	

int yywrap(void) {
    return 1;
} 