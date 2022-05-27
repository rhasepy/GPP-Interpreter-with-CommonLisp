gpp_interpreter: gpp_lexer.c utils.c
		flex gpp_lexer.l
		yacc -d gpp_interpreter.y
		cc lex.yy.c y.tab.c utils.c -o gpp_interpreter
clean: 
	rm gpp_lexer.c y.tab.c y.tab.h lex.yy.c gpp_interpreter