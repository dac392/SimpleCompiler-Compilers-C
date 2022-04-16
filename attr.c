/**********************************************
        CS415  Project 2
        Spring  2022
        Student Version
**********************************************/

#include "attr.h" 
#include <string.h>
#include <stdlib.h>
#define BUFF_SIZE 200
#define VAR_SIZE 20


void init_declarations(declarations_t*obj){
	obj->variable_buffer = malloc(sizeof(char*)*BUFF_SIZE);
	obj->iterator = 0;
}
void reset_declarations(declarations_t*obj){
	//memset(obj->variable_buffer, 0, BUFF_SIZE);
	for(int i = 0; i < obj->iterator; i++){
		free(obj->variable_buffer[i]);
	}
	obj->iterator = 0;
}
void addVariable(declarations_t*obj, char*var_name){
	// I'm assuming that variables are one character long
	char* var = malloc(sizeof(char)*VAR_SIZE);
	strcpy(var, var_name);
	obj->variable_buffer[obj->iterator++] = var;
	
}

void destroy(declarations_t*obj){
	free(obj->variable_buffer);
}