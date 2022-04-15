/**********************************************
        CS415  Project 2
        Spring  2022
        Student Version
**********************************************/


#ifndef SYMTAB_H
#define SYMTAB_H

#include <string.h>
#include "attr.h"

/* The symbol table implementation uses a single hash     */
/* function. Starting from the hashed position, entries   */
/* are searched in increasing index order until a         */
/* matching entry is found, or an empty entry is reached. */

#define HASH_TABLE_SIZE 721

typedef struct { /* need to augment this */
  char *name;
  int offset;
  int reg1;
  int val_int;
  Boolean val_bool;
  Type_Expression type; 
} SymTabEntry;

extern
void InitSymbolTable();

extern
SymTabEntry * lookup(char *name);

extern
void insert(char *name, Type_Expression type, int offset);

extern
void PrintSymbolTable();

/* Helper functions */
Boolean isInt(SymTabEntry* entry);
int getInt(SymTabEntry* entry);
Boolean getBool(SymTabEntry* entry);
Type_Expression getType(SymTabEntry* entry);
int getRegister1(SymTabEntry* entry);
int getOffset(SymTabEntry* entry);
void setInt(SymTabEntry* entry, int val_int);
void setBool(SymTabEntry* entry, Boolean val_bool);


#endif
