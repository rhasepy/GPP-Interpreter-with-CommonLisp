all: gpp_lexer

gpp_lexer: 171044033_main.c gpp_lexer.c
		gcc 171044033_main.c gpp_lexer.c -o gpp_lexer.out

gpp_lexer.c: lex.yy.c
		mv lex.yy.c gpp_lexer.c

lex.yy.c: gpp_lexer.l
		flex gpp_lexer.l
