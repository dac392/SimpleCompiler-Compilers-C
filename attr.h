/**********************************************
        CS415  Project 2
        Spring  2022
        Student Version
**********************************************/

#ifndef ATTR_H
#define ATTR_H

typedef union {int num; char *str;} tokentype;

typedef enum type_expression {TYPE_INT=0, TYPE_BOOL, TYPE_ERROR} Type_Expression;

typedef struct {
        Type_Expression type;
        int targetRegister;
} regInfo;

/* -----declarations_t----- */
typedef struct
{
	char**variable_buffer;
	int iterator;

}declarations_t;
void init_declarations(declarations_t*obj);
void reset_declarations(declarations_t*obj);
void addVariable(declarations_t*obj, char*a);
void destroy(declarations_t*obj);


#endif


  
