/*
		Muharrem Ozan Yesiller
			171044033
			Assignment 1
*/

#include <stdio.h>
#include <stdlib.h>
#include "tokens.h"

extern int yylex();
extern int yylineno;
extern char* yytext;
extern FILE *yyin;

const char *names[] = { "", "KW_AND", "KW_OR", "KW_NOT",
	"KW_EQUAL", "KW_LESS", "KW_NILL", "KW_LIST", "KW_APPEND",
	"KW_CONCAT", "KW_SET", "KW_DEFFUN", "KW_FOR",
	"KW_IF", "KW_EXIT", "KW_LOAD", "KW_DISP", "KW_TRUE",
	"KW_FALSE", "OP_PLUS", "OP_MINUS", "OP_DIV", "OP_DBLMULT",
	"OP_MULT", "OP_OP", "OP_CP", "OP_OC", "OP_CC", "OP_COMMA",
	"COMMENT", "VALUE", "IDENTIFIER", "SYNTAX ERROR! cannot be tokanized" , "", "" };

void printToken_onFile(const int token, FILE* outFile) {

	if(token >= ARRAY_SIZE) {

		printf("ERROR: Index out of bounds; index-> %d, arr_size-> %d\n",token, ARRAY_SIZE);
		exit(1);

	} else if(token < 0) {

		printf("ERROR: Index is negative...\n");
		exit(1);

	} else if(token <= UNKNOWN) { fprintf(outFile, "%s\n", names[token]); }
}

void runInterpreter() {		// REPL MODE

	int _token;
	int enterCount = 0;
	yyin = stdin;

	FILE* fp_out = fopen("parsed_cpp.txt", "w");

	printf("> ");

	do {

		if( (_token = yylex()) == NEWLINE) {

			printf("> ");

			if( (_token = yylex()) == NEWLINE) exit(1);
		}

		printToken_onFile(_token, fp_out);

	}while(_token);
}

void runWithFile(const char* fileName) {   // READ FILE MODE
	
	FILE* fp_in = fopen(fileName, "r");
	FILE* fp_out = fopen("parsed_cpp.txt", "w");

	int _token;

	if(fp_in == NULL) {

		printf("File can not found...\n");
		return;

	} else {

		yyin = fp_in;
		
		do {
			_token = yylex();
			printToken_onFile(_token, fp_out);

			if(_token == COMMENT)
				while(_token != NEWLINE) 
					_token = yylex();
		}while(_token != 0);
	}
}

int main(int argc, char const *argv[]) {

	if(argc == 1) runInterpreter();

	else if(argc == 2) runWithFile(argv[1]);
	
	else {
		printf("WRONG FORMAT ABOUT FILE...\n");
		exit(1);
	}	

	return 0;
}