/**********************************************
        CS415  Project 2
        Spring  2022
        Student Version
**********************************************/

#include "attr.h"
#include <stdio.h>
#include <string.h>
void stringCopy(char*src, char*dest){
	int srcLength = strlen(src);
	memset(dest,0,(sizeof(char)*59));
	memmove(dest, src, srcLength);
}

