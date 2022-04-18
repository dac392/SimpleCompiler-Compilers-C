/**********************************************
        CS415  Project 2
        Spring  2022
        Student Version
**********************************************/

#ifndef ATTR_H
#define ATTR_H

typedef union {int num; char *str;} tokentype;

typedef enum type_expression {TYPE_INT=0, TYPE_BOOL, TYPE_ERROR} Type_Expression;
typedef enum expression_type {TYPE_ID = 0, TYPE_CONST=1}Expression_Type;
typedef enum state_type {TYPE_FOR=0, TYPE_IF=1} State_Type;

typedef struct {
        Type_Expression type;
        //Expression_Type var_type;
        int targetRegister;
        //int constant;
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

/* -----state_t-----*/
typedef struct
{
	State_Type type;
	int iterator;
	int target;
	int load_register;
	int expression_register;
	int label_one;
	int label_two;
	int label_three;
	/*symtab info*/
	char* name;
	int offset;
}state_t;

typedef struct{
	Type_Expression type;
	int is_array;
	int array_size;
}Type_t;

typedef struct{
	int label_1;
}label_t;

#endif


  
