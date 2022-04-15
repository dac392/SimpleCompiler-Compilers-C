%{
#include <stdio.h>
#include <string.h>
#include "attr.h"
#include "instrutil.h"
int yylex();
void yyerror(char * s);
#include "symtab.h"

FILE *outfile;
char *CommentBuffer;
Variables_t storage_db;
SymTabEntry *synch;
SymTabEntry *synch_id;
 
%}

%union {tokentype token;
        regInfo targetReg;
        Type_Expression dataType;
       }

%token PROG PERIOD VAR 
%token <dataType> INT BOOL PRT THEN IF DO FI ENDWHILE ENDFOR
%token ARRAY OF 
%token BEG END ASG  
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token WHILE FOR ELSE 
%token <token> ID ICONST 

%type <targetReg> exp 
%type <targetReg> lhs 

%start program

%nonassoc EQ NEQ LT LEQ GT GEQ 
%left '+' '-' AND
%left '*' OR

%nonassoc THEN
%nonassoc ELSE

%%
program : {emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
           emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY);} 
           PROG ID ';' block PERIOD {  }
	;

block	: variables cmpdstmt {  }
	;

variables: /* empty */
	| VAR vardcls { }
	;

vardcls	: vardcls vardcl ';' { }
	| vardcl ';' { }
	| error ';' { yyerror("***Error: illegal variable declaration\n");}  
	;

vardcl	: idlist ':' type { 

      for(int i = 0; i < storage_db.iterator; i++){
        /*I'm not totally sure about the offset being zero, but we'll see
         * insert( name, Type_Expression, offset)*/
        insert(storage_db.vars[i], storage_db.dataType, -1);
      } 
      storage_db.holding = False;
    }
	;

idlist	: idlist ',' ID { storage_db.vars[storage_db.iterator++]=$3.str; }
        | ID		{ storage_db.vars[storage_db.iterator++]=$1.str; } 
	;


type	: ARRAY '[' ICONST ']' OF stype {  }

        | stype {  }
	;

stype	: INT { storage_db.dataType = TYPE_INT; }
        | BOOL { storage_db.dataType = TYPE_BOOL }
	;

stmtlist : stmtlist ';' stmt { }
	| stmt { }
        | error { yyerror("***Error: ';' expected or illegal statement \n");}
	;

stmt    : ifstmt { }
	| fstmt { }
	| wstmt { }
	| astmt { }
	| writestmt { }
	| cmpdstmt { }
	;

cmpdstmt: BEG stmtlist END { }
	;

ifstmt :  ifhead 
          THEN
          stmt 
  	  ELSE 
          stmt 
          FI
	;

ifhead : IF condexp {  }
        ;

writestmt: PRT '(' exp ')' { 

			int printOffset = -4; /* default location for printing */
			//synch = lookup($3.name)
			//sprintf(CommentBuffer, "what is this: %s, %d", getRegister1(synch), getOffset(synch));
         	//emitComment(CommentBuffer);

            sprintf(CommentBuffer, "Code for \"PRINT\" from offset %d", printOffset);
         	emitComment(CommentBuffer);
            emit(NOLABEL, STOREAI, $3.targetRegister, 0, printOffset);
            emit(NOLABEL, 
                  OUTPUTAI, 
                  0,
                  printOffset, 
                  EMPTY);
           }
	;

fstmt	: FOR ctrlexp DO stmt { }
          ENDFOR
	;

wstmt	: WHILE condexp DO stmt { }
          ENDWHILE
        ;
  

astmt : lhs ASG exp  /*figure this out*/          { 
 				  if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) || 
				         (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
				    printf("*** ERROR ***: Assignment types do not match.\n");
				  }
          			emit(NOLABEL, STORE, $3.targetRegister, $1.targetRegister, EMPTY);
                                }
	;

lhs	: ID{ /* DONE? */
                synch_id = lookup($1.str);
                sprintf(CommentBuffer, "Trying to lhs: %s", synch_id->name);
                emitComment(CommentBuffer);
                if(synch_id->offset == -1){
                  int offsetRegister = NextRegister();
                  synch_id->reg1 = NextRegister();
                  synch_id->offset = NextOffset(1);
                  emit(NOLABEL, LOADI, getOffset(synch_id), offsetRegister, EMPTY);
                  emit(NOLABEL, ADD, BASE, offsetRegister, getRegister1(synch_id));
                  $$.targetRegister = getRegister1(synch_id);
                }else{
                	int prev = NextRegister();
                	emit(NOLABEL, LOADI, getOffset(synch_id), prev, EMPTY);
                	int offsetRegister = NextRegister();
                	emit(NOLABEL, ADD, 0, prev, offsetRegister);
                	//synch_id->reg1 = offsetRegister;
                	$$.targetRegister = offsetRegister;
                }
                
                $$.type = synch_id->type;
       	      }
                |  ID '[' exp ']' {   }
                ;


exp	: exp '+' exp		{ 
                      if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
    				            printf("*** ERROR ***: Operator types must be integer.\n");
                      }

                        synch = lookup(storage_db.hold1);
                        sprintf(CommentBuffer, "what is this %s , %d", synch->name, getRegister1(synch));
                		emitComment(CommentBuffer);

                		int target = NextRegister();
                        $$.type = getType(synch_id);
                        $$.targetRegister = target;
                        
                        emit(NOLABEL, ADD, $1.targetRegister, $3.targetRegister, $$.targetRegister);
                        storage_db.holding = False;
                        
                    }

        | exp '-' exp		{ 
                      /* WARNING: this was just copy pased from add. Don't trust without testing */
                      if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
                        printf("*** ERROR ***: Operator types must be integer.\n");
                      }
                        synch = lookup(storage_db.hold1);
                        int val1 = getInt(synch);
                        synch = lookup(storage_db.hold2);
                        int val2 = getInt(synch);
                        setInt(synch_id, val1-val2);
                        $$.type = getType(synch_id);
                        $$.targetRegister = getRegister1(synch_id);
                        
                        emit(NOLABEL, SUB, $1.targetRegister, $3.targetRegister, $$.targetRegister);
                        storage_db.holding = False;
                        
         }

        | exp '*' exp		{ 
                      /* WARNING: this was just copy pased from add. Don't trust without testing */
                      if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
                        printf("*** ERROR ***: Operator types must be integer.\n");
                      }
                        synch = lookup(storage_db.hold1);
                        int val1 = getInt(synch);
                        synch = lookup(storage_db.hold2);
                        int val2 = getInt(synch);
                        setInt(synch_id, val1*val2);
                        $$.type = getType(synch_id);
                        $$.targetRegister = getRegister1(synch_id);
                        
                        emit(NOLABEL, MULT, $1.targetRegister, $3.targetRegister, $$.targetRegister);
                        storage_db.holding = False;
                        
         }

        | exp AND exp		{  } 


        | exp OR exp       	{  }


        | ID { /* BOGUS  - needs to be fixed */

                    (storage_db.holding)? stringCopy($1.str, storage_db.hold2) : stringCopy($1.str, storage_db.hold1);
                    
                    synch = lookup(storage_db.hold1);//$1.str
                	int first_expression = NextRegister();
                	$1.num = first_expression;
                	emit(NOLABEL, LOADAI, 0, getOffset(synch), first_expression);
                	//synch->reg1 = first_expression;
                    $$.targetRegister = first_expression;
                    $$.type = getType(synch);
                    storage_db.holding = True;
                  }

        | ID '[' exp ']'	{   }
 


	| ICONST { 

              if(isInt(synch_id)){
                $$.targetRegister = NextRegister();
                $$.type = TYPE_INT;
                setInt(synch_id, $1.num);
                emit(NOLABEL, LOADI, getInt(synch_id), $$.targetRegister, EMPTY);
              }

           }

        | TRUE                   { 
          int newReg = NextRegister(); /* TRUE is encoded as value '1' */
          $$.targetRegister = newReg;
				  $$.type = TYPE_BOOL;
				  emit(NOLABEL, LOADI, 1, newReg, EMPTY); }

        | FALSE                   { 
          int newReg = NextRegister(); /* TRUE is encoded as value '0' */
          $$.targetRegister = newReg;
          $$.type = TYPE_BOOL;
          emit(NOLABEL, LOADI, 0, newReg, EMPTY); }

	| error { yyerror("***Error: illegal expression\n");}  
	;


ctrlexp	: ID ASG ICONST ',' ICONST { }
        ;

condexp	: exp NEQ exp		{  } 

        | exp EQ exp		{  } 

        | exp LT exp		{  }

        | exp LEQ exp		{  }

	| exp GT exp		{  }

	| exp GEQ exp		{  }

	| error { yyerror("***Error: illegal conditional expression\n");}  
        ;

%%

void yyerror(char* s) {
        fprintf(stderr,"%s\n",s);
        }


int
main(int argc, char* argv[]) {

  printf("\n     CS415 Spring 2022 Compiler\n\n");

  outfile = fopen("iloc.out", "w");
  if (outfile == NULL) { 
    printf("ERROR: Cannot open output file \"iloc.out\".\n");
    return -1;
  }

  CommentBuffer = (char *) malloc(1961);  
  InitSymbolTable();

  /* bro you hve to fix this at some point*/
  storage_db.vars = malloc(sizeof(char*)*200);
  storage_db.iterator = 0;
  storage_db.hold1 = (char *) malloc(60);
  storage_db.hold2 = (char *) malloc(60);
  

  printf("1\t");
  yyparse();
  printf("\n");

  PrintSymbolTable();
  
  fclose(outfile);
  
  return 1;
}




