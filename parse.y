%{
#include <stdio.h>
#include "attr.h"
#include "instrutil.h"
int yylex();
void yyerror(char * s);
#include "symtab.h"

FILE *outfile;
char *CommentBuffer;
declarations_t declaration_db;
SymTabEntry * assignment_entry;
 
%}

%union {tokentype token;
        regInfo targetReg;
        state_t state_machine;
       }

%token PROG PERIOD VAR 
%token INT BOOL PRT THEN IF DO FI ENDWHILE ENDFOR
%token ARRAY OF 
%token BEG END ASG  
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token WHILE FOR ELSE 
%token <token> ID ICONST 

%type <targetReg> exp 
%type <targetReg> lhs 
%type <state_machine> ctrlexp condexp ifhead

%start program

%nonassoc EQ NEQ LT LEQ GT GEQ 
%left '+' '-' AND
%left '*' OR

%nonassoc THEN
%nonassoc ELSE

%%
program : {
            emitComment("Assign STATIC_AREA_ADDRESS to register \"r0\"");
            emit(NOLABEL, LOADI, STATIC_AREA_ADDRESS, 0, EMPTY);
          } 
          PROG ID ';' block PERIOD { }
	;

block	: variables cmpdstmt { }
	;

variables: /* empty */
	| VAR vardcls {}
	;

vardcls	: vardcls vardcl ';' { }
	| vardcl ';' { }
	| error ';' { yyerror("***Error: illegal variable declaration\n"); }  
	;

vardcl	: idlist ':' type {  }
	;

idlist	: idlist ',' ID { addVariable(&declaration_db, $3.str); }
        | ID		{ addVariable(&declaration_db, $1.str); } 
	;


type	: ARRAY '[' ICONST ']' OF stype { 
        
}

        | stype { reset_declarations(&declaration_db); }
	;

stype	: INT {
          for(int i = 0; i < declaration_db.iterator; i++){
            insert(declaration_db.variable_buffer[i], TYPE_INT, -1);
          }
        }
        | BOOL { 
          for(int i = 0; i < declaration_db.iterator; i++){
            insert(declaration_db.variable_buffer[i], TYPE_BOOL, -1);
          }
        }
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
  	  ELSE {
        sprintf(CommentBuffer, "Branch to statement following the \"else\" statement list");
        emitComment(CommentBuffer);
        emit(NOLABEL, BR, $1.label_two, EMPTY, EMPTY);
        emit($1.label_two, NOP, EMPTY, EMPTY, EMPTY);
      }
          stmt 
          FI { emit($1.label_three, NOP, EMPTY, EMPTY, EMPTY); }
	;

ifhead : IF condexp {  
          $$.label_one    = $2.label_one; /*I don't think its needed*/
          $$.label_two    = $2.label_two; /*Takes you to else label*/
          $$.label_three  = $2.label_three;/*Break from if else label*/
}
        ;

writestmt: PRT '(' exp ')' { 
       int printOffset = -4; /* default location for printing */
       sprintf(CommentBuffer, "Code for \"PRINT\" from offset %d", printOffset);
       emitComment(CommentBuffer);
       emit(NOLABEL, STOREAI, $3.targetRegister, 0, printOffset);
       emit(NOLABEL, OUTPUTAI, 0,printOffset, EMPTY);
     }
	;

fstmt	: FOR ctrlexp DO stmt { 
        int newReg1 = NextRegister();
        int newReg2 = NextRegister();
        emit(NOLABEL, LOADAI, 0, $2.offset, newReg1);
        emit(NOLABEL, ADDI, newReg1, 1, newReg2);
        emit(NOLABEL, STOREAI, newReg2, 0, $2.offset);
        emit(NOLABEL, BR, $2.label_one, EMPTY, EMPTY);
        emit($2.label_three, NOP, EMPTY,EMPTY,EMPTY);
}
          ENDFOR
	;

wstmt	: WHILE condexp DO stmt { }
          ENDWHILE
        ;
  

astmt : lhs ASG exp             { 
 				  if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) || 
				         (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
				    printf("*** ERROR ***: Assignment types do not match.\n");
				  }

				  emit(NOLABEL,STORE, $3.targetRegister,$1.targetRegister,EMPTY);
                                }
	;

lhs	: ID	{

          assignment_entry = lookup($1.str);
          int newReg1 = NextRegister();
          int newReg2 = NextRegister();

          if(assignment_entry->offset == -1){
            assignment_entry->offset = NextOffset(1);
          }

          $$.targetRegister = newReg2;
          $$.type = assignment_entry->type;

          sprintf(CommentBuffer, "Compute address of variable \"%s\" at offset %d in register r%d", $1.str, assignment_entry->offset, newReg2);
          emitComment(CommentBuffer);

				  emit(NOLABEL, LOADI, assignment_entry->offset, newReg1, EMPTY);
				  emit(NOLABEL, ADD, 0, newReg1, newReg2);
				  
        }


                                |  ID '[' exp ']' {   }
                                ;


exp	: exp '+' exp		{ 
          if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) {
            printf("*** ERROR ***: Operator types must be integer.\n");
          }
          int newReg = NextRegister();
          $$.type = $1.type;
          $$.targetRegister = newReg;
          emit(NOLABEL, ADD, $1.targetRegister, $3.targetRegister, newReg);
          

          
        }

        | exp '-' exp		{  
          int newReg = NextRegister();
          $$.type = $1.type;
          $$.targetRegister = newReg;
          emit(NOLABEL, SUB, $1.targetRegister, $3.targetRegister, newReg);
        }

        | exp '*' exp		{  
          int newReg = NextRegister();
          $$.type = $1.type;
          $$.targetRegister = newReg;
          emit(NOLABEL, MULT, $1.targetRegister, $3.targetRegister, newReg);
        }

        | exp AND exp		{ 
          int newReg = NextRegister();
          emit(NOLABEL, AND_INSTR, $1.targetRegister, $3.targetRegister, newReg);
          $$.targetRegister = newReg;
          $$.type = TYPE_BOOL;
        } 


        | exp OR exp { 
          int newReg = NextRegister();
          emit(NOLABEL, OR_INSTR, $1.targetRegister, $3.targetRegister, newReg);
          $$.targetRegister = newReg;
          $$.type = TYPE_BOOL;

        }


        | ID{
          SymTabEntry* table_entry  = lookup($1.str);
          int newReg                = NextRegister();
          //$1.num                    = newReg;
          $$.targetRegister         = newReg;
          $$.type                   = table_entry->type;
          //$$.var_type               = TYPE_ID;
            sprintf(CommentBuffer, "Load RHS value of variable \"%s\" at offset %d", table_entry->name, table_entry->offset);
            emitComment(CommentBuffer);
          emit(NOLABEL, LOADAI, 0, table_entry->offset, newReg);
                                  
        }

        | ID '[' exp ']'	{   }
 


	| ICONST { 

            int newReg        = NextRegister();
            $$.targetRegister = newReg;
            $$.type           = TYPE_INT;
            //$$.var_type       = TYPE_CONST;
            //$$.constant       = $1.num;
            emit(NOLABEL, LOADI, $1.num, newReg, EMPTY);

        }
        | TRUE                   { int newReg = NextRegister(); /* TRUE is encoded as value '1' */
	                           $$.targetRegister = newReg;
				   $$.type = TYPE_BOOL;
				   emit(NOLABEL, LOADI, 1, newReg, EMPTY); }

        | FALSE                   { int newReg = NextRegister(); /* TRUE is encoded as value '0' */
	                           $$.targetRegister = newReg;
				   $$.type = TYPE_BOOL;
				   emit(NOLABEL, LOADI, 0, newReg, EMPTY); }

	| error { yyerror("***Error: illegal expression\n");}  
	;


ctrlexp	: ID ASG ICONST ',' ICONST { 
      assignment_entry = lookup($1.str);
      if(assignment_entry->offset == -1){
        assignment_entry->offset = NextOffset(1);
      }
      int newReg1 = NextRegister();
      int newReg2 = NextRegister();
      int newReg3 = NextRegister();
      int newReg4 = NextRegister();
      int newReg5 = NextRegister(); //r11
      int newReg6 = NextRegister(); //r12

      $$.name     = assignment_entry->name;
      $$.offset   = assignment_entry->offset;
      $$.type     = TYPE_FOR;
      $$.iterator = $3.num;
      $$.target   = $5.num;

      $$.load_register        = newReg3;
      $$.expression_register  = newReg4;
      $$.label_one            = NextLabel();  /*takes you to control label*/
      $$.label_two            = NextLabel();  /*takes you to loop label*/
      $$.label_three          = NextLabel();  /*takes you to break label*/

      sprintf(CommentBuffer, "Initialize ind. variable \"%s\" at offset %d with lower bound value %d", $$.name, $$.offset, $$.iterator);
      emitComment(CommentBuffer);
      emit(NOLABEL, LOADI, assignment_entry->offset, newReg1, EMPTY);
      emit(NOLABEL, ADD, 0, newReg1, newReg2);
      emit(NOLABEL, LOADI, $$.iterator, newReg5, EMPTY);
      emit(NOLABEL, LOADI, $$.target, newReg6, EMPTY);
      emit(NOLABEL, STORE, newReg5, newReg2, EMPTY);

      sprintf(CommentBuffer, "Generate control code for \"FOR\"");
      emitComment(CommentBuffer);
      emit($$.label_one, LOADAI, 0, $$.offset, $$.load_register);
      emit(NOLABEL, CMPLE, $$.load_register, $$.target, $$.expression_register);
      emit(NOLABEL, CBR, $$.expression_register, $$.label_two, $$.label_three);
      emit($$.label_two, NOP, EMPTY, EMPTY, EMPTY);  
      
}
        ;

condexp	: exp NEQ exp		{  } 

        | exp EQ exp		{  } 

        | exp LT exp		{
          int comparison = NextRegister();
          int true_label = NextLabel();
          int else_label = NextLabel();
          int break_label= NextLabel();
          emit(NOLABEL, CMPLE, $1.targetRegister, $3.targetRegister, comparison);
          emit(NOLABEL, CBR, comparison, true_label, else_label);
          emit(true_label, NOP, EMPTY, EMPTY, EMPTY);
          sprintf(CommentBuffer, "This branch is the \"true\" branch (FIX)");
          emitComment(CommentBuffer);
          $$.label_one    = true_label;
          $$.label_two    = else_label;
          $$.label_three  = break_label;

        }

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
  init_declarations(&declaration_db);
  printf("1\t");
  yyparse();
  printf("\n");

  PrintSymbolTable();
  
  fclose(outfile);
  
  return 1;
}




