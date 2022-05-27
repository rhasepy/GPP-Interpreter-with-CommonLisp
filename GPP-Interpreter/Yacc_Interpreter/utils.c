/*
    Muharrem Ozan Yeşiller 171044033
*/

#include "utils.h"

int pow_fun(int value, unsigned int raise_val) {

    if (raise_val == 0) {
        return 1;
    }

    int returnVal = pow_fun(value, raise_val / 2);
    if (raise_val % 2 == 0) {
        return returnVal * returnVal;
    } else {
        return value* returnVal * returnVal;
    }
}

void yyerror(char* msg) {
    printf("%s\n", msg);
}

LinkedList_VALUE* init_linkedlist_value(int data){

    LinkedList_VALUE* list = malloc(sizeof(LinkedList_VALUE));
    list -> head = malloc(sizeof(VALUE_NODE));

    list -> tail = list -> head;
    list -> head -> data = data;
    list -> head -> next = NULL;

    return list;
}

LinkedList_IDENTIFIER* init_linkedlist_keys(char* string)
{
    LinkedList_IDENTIFIER* keyList = malloc(sizeof(LinkedList_IDENTIFIER));
    keyList->head = malloc(sizeof(IDENTIFIER_NODE));

    keyList->tail = keyList->head;
    keyList->head->data = strdup(string);
    keyList->head->next = NULL;

    return keyList;
}

int isEmpty(LinkedList_VALUE* list) {

    if (NULL != list) {
        return (list -> head == NULL);
    }
    return TRUE;
}

int isEmpty_(LinkedList_IDENTIFIER* list) {

    if (NULL != list) {
        return (list -> head == NULL);
    }
    return TRUE;
}

void pushValue_LL(LinkedList_VALUE** list, int item) {

    VALUE_NODE* newNode = malloc(sizeof(VALUE_NODE));

    newNode -> data = item;
    newNode -> next = NULL;

    LinkedList_VALUE* temp = *list;

    if (TRUE == isEmpty(temp)) {
        temp -> tail = newNode;
    } else {
        temp -> tail -> next = newNode;
    }
    temp -> tail = newNode;
}

void pushIdentifier_LL(LinkedList_IDENTIFIER** list, char* string) {

    IDENTIFIER_NODE* newNode = malloc(sizeof(IDENTIFIER_NODE));

    newNode -> data = strdup(string);
    newNode -> next = NULL;

    LinkedList_IDENTIFIER* temp = *list;

    if (TRUE == isEmpty_(temp)) {
        temp -> tail = newNode;
    } else {
        temp -> tail -> next = newNode;
    }
    temp -> tail = newNode;
}

IDENTIFIER_NODE* nextNode_KEYS(IDENTIFIER_NODE* node) {
    return node -> next;
}

VALUE_NODE* nextNode_VALUES(VALUE_NODE* node) {
    return node -> next;
}

int setIdentifierVALUE(LinkedList_IDENTIFIER** tokenID_LL, LinkedList_VALUE** tokenVALUE_LL, char* string, int item) {

    LinkedList_IDENTIFIER* tempID_LL = *tokenID_LL;
    LinkedList_VALUE* tempVALUE_LL = *tokenVALUE_LL;

    // First set identifier 
    if(NULL == tempID_LL->head) {
        tempID_LL = init_linkedlist_keys(string);
        tempVALUE_LL = init_linkedlist_value(item);
    
    // Non first set identifier
    } else {
        IDENTIFIER_NODE* traverser_ids = tempID_LL -> head;
        VALUE_NODE* traverser_operands = tempVALUE_LL -> head;
        int defined_identifier_flag = FALSE;

        while(NULL != traverser_ids) {
            if(FALSE == strcmp(traverser_ids -> data, string)) {
                defined_identifier_flag = TRUE;
            }
            traverser_ids = nextNode_KEYS(traverser_ids);
            traverser_operands = nextNode_VALUES(traverser_operands);
        }

        // if identifier defined set value
        if(TRUE == defined_identifier_flag) {
            traverser_operands -> data = item;

        // if identifier undefines append id->value to hashmap table
        } else {
            pushIdentifier_LL(&tempID_LL,  string);
            pushValue_LL(&tempVALUE_LL, item);
        }
    }

    *tokenID_LL = tempID_LL;
    *tokenVALUE_LL = tempVALUE_LL;
    return item;
}

int getIdentifierVALUE(LinkedList_IDENTIFIER* tokenID_LL, LinkedList_VALUE* tokenVALUE_LL, char* string){

    IDENTIFIER_NODE* key_traverser = tokenID_LL->head;
    VALUE_NODE* value_traverser = tokenVALUE_LL->head;

    while(NULL != key_traverser) {
        
        if(FALSE == strcmp(key_traverser -> data, string)) {
            return value_traverser -> data;
        }
        key_traverser = nextNode_KEYS(key_traverser);
        value_traverser = nextNode_VALUES(value_traverser);
    }

    printf("ERROR: UNDEFINED IDENTIFIER\n");
    exit(-1);
    return -1;     
}