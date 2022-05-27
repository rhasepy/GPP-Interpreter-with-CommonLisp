/*
    Muharrem Ozan Yeşiller 171044033
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Boolean macros
#define TRUE 1
#define FALSE 0

// Data Structures for Hash Table approach for keep identifier and value

// Node structs for list
typedef struct IDENTIFIER_NODE {
    char* data;
    struct IDENTIFIER_NODE* next;
}IDENTIFIER_NODE;

typedef struct VALUE_NODE {
    int data;
    struct VALUE_NODE* next;
}VALUE_NODE;

// List structs
typedef struct LinkedList_IDENTIFIER {
    struct IDENTIFIER_NODE* head;
    struct IDENTIFIER_NODE* tail;
}LinkedList_IDENTIFIER;

typedef struct LinkedList_VALUE {
    struct VALUE_NODE* head;
    struct VALUE_NODE* tail;
}LinkedList_VALUE;

// init linked list method for operands
LinkedList_VALUE* init_linkedlist_value(int data);

// init linked list method for identifiers
LinkedList_IDENTIFIER* init_linkedlist_keys(char* string);

// pass next node
IDENTIFIER_NODE* nextNode_KEYS(IDENTIFIER_NODE* node);

// pass next node
VALUE_NODE* nextNode_VALUES(VALUE_NODE* node);

// push operand value into operand list
void pushValue_LL(LinkedList_VALUE** list, int item);

// yacc/lex method override
void yyerror(char* string);

// is empty method for operand list
int isEmpty(LinkedList_VALUE* list);

// is empty method for identifier list
int isEmpty_(LinkedList_IDENTIFIER* list);

// power function for DBMULT operation
int pow_fun(int value, unsigned int raise_val);

// set and map identifier with value into linked lists
int setIdentifierVALUE(LinkedList_IDENTIFIER** tokenID_LL, LinkedList_VALUE** tokenVALUE_LL, char* str, int val);

// search identifier and return value
int getIdentifierVALUE(LinkedList_IDENTIFIER* tokenID_LL, LinkedList_VALUE* tokenVALUE_LL, char * str);

// yac/lex method override
int yylex();