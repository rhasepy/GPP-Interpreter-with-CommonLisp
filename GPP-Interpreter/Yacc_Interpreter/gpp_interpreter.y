/*
    Muharrem Ozan Yeşiller 171044033
*/

%{
    #include <stdio.h> 
    #include <string.h>
    #include <stdlib.h>
    #include <stdarg.h>
    #include "utils.h"

    /*
        token ID is identifier list program
        token Value is value of identifiers list program

        !!!
        like hash map table: IDENTIFIER ---map---> VALUE
        !!!
    */
    LinkedList_IDENTIFIER* tokenID_LL = NULL;
    LinkedList_VALUE* tokenVALUE_LL = NULL;
%}

%union {
    LinkedList_VALUE* opValues;
    int opValue;
    char* expression;
};

/* operation tokens */
%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_DBLMULT
%token OP_OP
%token OP_CP
%token OP_COMMA
%token OP_Q         /* quaomark */

/* keyword tokens */
%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_LESS
%token KW_NIL
%token KW_LIST
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_DEFFUN
%token KW_FOR
%token KW_IF
%token KW_EXIT
%token KW_LOAD
%token KW_DISP
%token KW_TRUE
%token KW_FALSE
%token KW_DEFVAR
%token KW_WHILE

/* other tokens */
%token COMMENT
%token STRING
%token <opValue> VALUE
%token <expression> IDENTIFIER

/* Type rules */
%type <opValues> listval explisti values
%type <opValue> expi expb

/* for negative numbers */
%nonassoc UNMINUS

%%
start: start input | input;

input: expi | expb | COMMENT | explisti;

/* $$ -> return value */
values: 
        values VALUE {
            pushValue_LL(&($1), $2);
            $$ = $1;
        }

      | VALUE { 
            $$ = init_linkedlist_value($1);
        };

/* OP_Q for list expression example: '(1 2 3) */
listval: 
        OP_OP KW_LIST values OP_CP {$$ = $3;} | OP_Q OP_OP values OP_CP {$$ = $3;} | OP_Q OP_OP OP_CP {$$ = NULL;} | KW_NIL {$$ = NULL;} ;

expi:
        VALUE {
            $$ = $1;int isEmpty(LinkedList_VALUE* list);
        }
      | IDENTIFIER {
            $$ = getIdentifierVALUE(tokenID_LL, tokenVALUE_LL, $1);
        }

      | OP_MINUS expi %prec UNMINUS {
            $$ = -$2;
        }

        | OP_OP OP_PLUS expi expi OP_CP {
            $$ = $3 + $4;
        }

      | OP_OP OP_MINUS expi expi OP_CP {
            $$ = $3 - $4;
        }

      | OP_OP OP_DIV expi expi OP_CP {
            if($4 == 0) {
                printf("Infinity!\n"); 
            } else {
                $$ = $3/$4;
            }
        }

      | OP_OP OP_MULT expi expi OP_CP {
            $$ = $3 * $4;
        }

      | OP_OP OP_DBLMULT expi expi OP_CP {
            $$ = pow_fun($3,$4);
        }

      | OP_OP KW_SET IDENTIFIER expi OP_CP {
            $$ = setIdentifierVALUE(&tokenID_LL, &tokenVALUE_LL, $3, $4);
        }

      | OP_OP KW_IF expb explisti OP_CP {
            if ($3) $$ = $4 ? $4->tail->data : 0; 
            else $$ = 0;
        }

      | OP_OP KW_IF expb expi OP_CP {
            if ($3) { 
                $$ = $4;
            } else {
              $$ = 0;
            }
        }

      | OP_OP KW_IF expb explisti explisti OP_CP {
            if ($3) $$ = $4 ? $4->tail->data : 0; 
            else $$ = $5 ? $5->tail->data : 0;
        }

      | OP_OP KW_IF expb expi expi OP_CP {
            if ($3) {
                $$ = $4;
            } else {
                $$ = $5;
            } 
        }

      | OP_OP KW_WHILE OP_OP expb OP_CP explisti OP_CP { while($4) $$ = $6 ? $6->tail->data : 0; }

      | OP_OP KW_WHILE OP_OP expb OP_CP expi OP_CP {
            while($4) {
                $$ = $6;
            } 
        }

      | OP_OP  KW_FOR OP_OP IDENTIFIER expi expi OP_CP explisti OP_CP {
            while(getIdentifierVALUE(tokenID_LL, tokenVALUE_LL, $4) < $5) {
                setIdentifierVALUE(&tokenID_LL, &tokenVALUE_LL, $4, getIdentifierVALUE(tokenID_LL, tokenVALUE_LL, $4) + $6);

                if ($8) {
                    $$ = $8->tail->data;
                } else{
                    $$ = 0;
                }
            }
        }

      | OP_OP  KW_FOR OP_OP IDENTIFIER expi expi OP_CP expi OP_CP {
            while(getIdentifierVALUE(tokenID_LL, tokenVALUE_LL, $4)< $5) {
                setIdentifierVALUE(&tokenID_LL, &tokenVALUE_LL, $4,getIdentifierVALUE(tokenID_LL, tokenVALUE_LL, $4)+$6);$$ = $8;
            }
        }

      | OP_OP KW_DISP expi OP_CP {
            $$ = 1;
            printf("%d\n", $3);
        }

      | OP_OP KW_DISP explisti OP_CP {
            $$ = 1;
            if($3) {
                VALUE_NODE* traverser =  $3 -> head;
                while(NULL != traverser){
                    printf("%d ", traverser -> data);
                    traverser = traverser -> next;
                }
                printf("\n\n");
            } else {
                printf("NIL\n\n");
            }
        }

      | OP_OP KW_LOAD  STRING OP_CP {
            $$ = 1;
            printf("TRUE\n");
        }

      | OP_OP KW_EXIT  OP_CP {
            exit(0);
        };

expb: 
        OP_OP KW_AND expb expb OP_CP {$$ = $3 && $4;} | OP_OP KW_OR expb expb OP_CP {$$ = $3 || $4;} | OP_OP KW_NOT expb OP_CP {$$ = !$3;}
        | OP_OP KW_EQUAL expb expb OP_CP {$$ = $3 == $4;} | OP_OP KW_EQUAL expi expi OP_CP {$$ = $3 == $4;} | OP_OP KW_LESS expi expi OP_CP {$$ = $3 < $4;}
        | KW_TRUE {$$ = 1;} | KW_FALSE{$$ = 0;};

explisti:
        OP_OP KW_CONCAT explisti explisti OP_CP {
            VALUE_NODE* start = NULL;

            if ($3) {
                start = $3->head;
            } else if($4) {
                start = $4->head;
            }

            if(start) {
                $3->tail->next = $4->head;
                $$ = $3;
            } else {
                $$ = NULL;
            }  
        }

      | OP_OP KW_APPEND expi explisti OP_CP 
        {
            if ($4) {
                pushValue_LL(&($4), $3);
                $$ = $4;
            } else {
                $$ = init_linkedlist_value($3);
            }     
        }
        
      | listval {$$ = $1;};
%%

void init_mapTable_id_to_value() {

    // allocate dynamic identifier table
    tokenID_LL = malloc(sizeof(LinkedList_IDENTIFIER));

    // allocate dynamic value table
    tokenVALUE_LL = malloc(sizeof(LinkedList_VALUE));

    // initiliaze this dynamic table with NULL
    tokenID_LL -> head = NULL;
    tokenVALUE_LL -> head = NULL;
    tokenID_LL -> tail = NULL;
    tokenVALUE_LL -> tail = NULL;
}

int main(int argc, char **argv) {

    // initilaize my map table (id -> value)
    init_mapTable_id_to_value();
    
    printf("Gpp Interpreter Started. Type (exit) for exit\n\n");
    
    // run REPL parsing
    yyparse();

    // exit parser
    return 0;
}
