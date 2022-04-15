/**********************************************
        CS415  Project 2
        Spring  2022
        Student Version
**********************************************/

#ifndef ATTR_H
#define ATTR_H
#define BASE 0
typedef union {int num; char *str;} tokentype;
typedef enum boolean {False = 0, True = 1} Boolean;
typedef enum type_expression {TYPE_INT=0, TYPE_BOOL, TYPE_ERROR} Type_Expression;

typedef struct {
        Type_Expression type;
        int targetRegister;
} regInfo;

typedef struct{
	Type_Expression dataType;	//TYPE_INT | TYPE_BOOL
	int iterator;
	char **vars;
	char *hold1;				//used for holding variable name
	char *hold2;				//used for holding variable name
	Boolean holding;

}Variables_t;

/* Function definitions */
void stringCopy(char*src, char*dest);
#endif


  
