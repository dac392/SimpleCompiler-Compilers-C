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
        Type_t type_cast;
        label_t label;
       }

%token PROG PERIOD VAR 
%token INT BOOL PRT THEN IF DO FI ENDWHILE ENDFOR
%token ARRAY OF 
%token BEG END ASG  
%token EQ NEQ LT LEQ GT GEQ AND OR TRUE FALSE
%token <token>WHILE FOR ELSE 
%token <token> ID ICONST


%type <targetReg> exp 
%type <targetReg> lhs
%type <type_cast> stype type
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
	| error ';' { /*yyerror("***Error***: illegal variable declaration\n");*/ }  
	;

vardcl	: idlist ':' type { 

          for(int i = 0; i < declaration_db.iterator; i++){
            int offset = NextOffset($3.array_size);
            if(lookup(declaration_db.variable_buffer[i])!=NULL){
              printf("\n*** ERROR ***: duplicate declaration of %s.\n",declaration_db.variable_buffer[i]);
            }else{
              insert(declaration_db.variable_buffer[i], $3.type, offset, $3.array_size);
            }
          }
          reset_declarations(&declaration_db); 
}
	;

idlist	: idlist ',' ID { addVariable(&declaration_db, $3.str); }
        | ID		{ addVariable(&declaration_db, $1.str); } 
	;


type	: ARRAY '[' ICONST ']' OF stype
        {
          $$.type = $6.type;
          $$.is_array = 1;
          $$.array_size = $3.num;
          $$.is_array = 1;
        }

        | stype {
          $$.type = $1.type;
          $$.is_array = 0;
          $$.array_size = 1;
          $$.is_array = 0;
        }
	;

stype	: INT {
          $$.type = TYPE_INT;
        }
        | BOOL { 
          $$.type = TYPE_BOOL;
        }
	;

stmtlist : stmtlist ';' stmt { }
	| stmt { }
  | error { yyerror("\n***Error: ';' expected or illegal statement \n");}
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
        emit(NOLABEL, BR, $1.label_three, EMPTY, EMPTY);
        emit($1.label_two, NOP, EMPTY, EMPTY, EMPTY);
      }
          stmt 
          FI { emit($1.label_three, NOP, EMPTY, EMPTY, EMPTY); }
	;

ifhead : IF condexp {
          if($$.type != TYPE_BOOL){ printf("\n***Error: exp in if stmt must be boolean\n"); }
          else{
            $$.label_one    = $2.label_one; /*I don't think its needed*/
            $$.label_two    = $2.label_two; /*Takes you to else label*/
            $$.label_three  = NextLabel(); /*Break from if else label*/
            sprintf(CommentBuffer, "This branch is the \"true\" branch"); 
            emitComment(CommentBuffer);
          } 
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

wstmt	:  WHILE
  {
    $1.num = NextLabel();
    emit($1.num, NOP, EMPTY, EMPTY, EMPTY);
    sprintf(CommentBuffer, "Control code for \"WHILE DO\"");
    emitComment(CommentBuffer);
  }
   condexp DO stmt 
          ENDWHILE {
            if($3.type != TYPE_BOOL){ printf("\n***Error: exp in while stmt must be boolean\n"); }
            emit(NOLABEL, BR, $1.num, EMPTY, EMPTY);
            emit($3.label_two, NOP, EMPTY, EMPTY, EMPTY);}
        ;
  

astmt : lhs ASG exp             { 
 				  if (! ((($1.type == TYPE_INT) && ($3.type == TYPE_INT)) || 
				         (($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)))) {
				    printf("\n*** ERROR ***: Assignment types do not match.\n");
				  }

				    emit(NOLABEL,STORE, $3.targetRegister,$1.targetRegister,EMPTY);
        }
	;

lhs	: ID	{

          assignment_entry = lookup($1.str);
          if(assignment_entry == NULL){ printf("\n***Error: undeclared identifier %s\n", $1.str); }
          else{
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
        }


        |  ID '[' exp ']' {
          if($3.type != TYPE_INT){ printf("\n***Error: subscript exp not type integer\n"); }
          assignment_entry = lookup($1.str);
          if(assignment_entry == NULL){ printf("\n***Error: undeclared identifier %s\n", $1.str); }
          if(assignment_entry != NULL && !assignment_entry->is_array){ printf("\n***Error: id %s is not an array\n",assignment_entry->name); }
          else{
            sprintf(CommentBuffer, "Compute address of array variable \"%s\" with base address %d", $1.str, assignment_entry->offset);
            emitComment(CommentBuffer);
            int newReg1 = NextRegister(); /*answer*/
            int newReg2 = NextRegister(); /*type size, always 4*/
            int newReg3 = NextRegister(); 
            int newReg4 = NextRegister(); /*base address*/
            int newReg5 = NextRegister(); /*storage*/
            emit(NOLABEL, LOADI, 4, newReg2, EMPTY);
            emit(NOLABEL, MULT, newReg1-1, newReg2, newReg3);
            emit(NOLABEL, LOADI, assignment_entry->offset, newReg4, EMPTY);
            emit(NOLABEL, ADD, newReg4, newReg3, newReg5);
            emit(NOLABEL, ADD, 0, newReg5, newReg1);
            $$.targetRegister = newReg1;
            $$.type = assignment_entry->type;
          }
        }
        ;


exp	: exp '+' exp		{ 
          if (! (($1.type == TYPE_INT) && ($3.type == TYPE_INT))) { 
            printf("\n*** ERROR ***: Operator types must be integer.\n"); 
          }
          int newReg = NextRegister();
          $$.type = $1.type;
          $$.targetRegister = newReg;
          emit(NOLABEL, ADD, $1.targetRegister, $3.targetRegister, newReg);
          
        }

        | exp '-' exp		{
          if( !(($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ){ printf("\n***Error: types of operands for operation %s do not match\n","-"); }
          int newReg = NextRegister();
          $$.type = $1.type;
          $$.targetRegister = newReg;
          emit(NOLABEL, SUB, $1.targetRegister, $3.targetRegister, newReg);
        }

        | exp '*' exp		{  
          if( !(($1.type == TYPE_INT) && ($3.type == TYPE_INT)) ){ printf("\n***Error: types of operands for operation %s do not match\n","*"); }
          int newReg = NextRegister();
          $$.type = $1.type;
          $$.targetRegister = newReg;
          emit(NOLABEL, MULT, $1.targetRegister, $3.targetRegister, newReg);
        }

        | exp AND exp		{ 
          if( !(($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)) ){ 
            printf("\n***Error: types of operands for operation %s do not match\n","AND"); 
          }
          int newReg = NextRegister();
          emit(NOLABEL, AND_INSTR, $1.targetRegister, $3.targetRegister, newReg);
          $$.targetRegister = newReg;
          $$.type = TYPE_BOOL;
        } 


        | exp OR exp { 
          if( !(($1.type == TYPE_BOOL) && ($3.type == TYPE_BOOL)) ){ printf("\n***Error: types of operands for operation %s do not match\n","OR"); }
          int newReg = NextRegister();
          emit(NOLABEL, OR_INSTR, $1.targetRegister, $3.targetRegister, newReg);
          $$.targetRegister = newReg;
          $$.type = TYPE_BOOL;

        }


        | ID{
          SymTabEntry* table_entry  = lookup($1.str);
          if(table_entry == NULL){ 
            printf("\n***Error: undeclared identifier %s\n", $1.str);
          }else{
            int newReg                = NextRegister();
            $$.targetRegister         = newReg;
            $$.type                   = table_entry->type;
            sprintf(CommentBuffer, "Load RHS value of variable \"%s\" at offset %d", table_entry->name, table_entry->offset);
            emitComment(CommentBuffer);
            emit(NOLABEL, LOADAI, 0, table_entry->offset, newReg); 
          }                        
        }

        | ID '[' exp ']'	{
          assignment_entry = lookup($1.str);  /*if something is breaking, concider changin this to not update the global reference*/
          if(assignment_entry == NULL){ printf("\n***Error: undeclared identifier %s\n", $1.str); }
          else{
            sprintf(CommentBuffer, "Load RHS value of array variable \"%s\" with base address %d", $1.str, assignment_entry->offset);
            emitComment(CommentBuffer);
            int newReg1 = NextRegister(); /*answer*/
            int newReg2 = NextRegister(); /*type size, always 4*/
            int newReg3 = NextRegister(); 
            int newReg4 = NextRegister(); /*base address*/
            int newReg5 = NextRegister(); /*storage*/
            emit(NOLABEL, LOADI, 4, newReg2, EMPTY);
            emit(NOLABEL, MULT, newReg1-1, newReg2, newReg3);
            emit(NOLABEL, LOADI, assignment_entry->offset, newReg4, EMPTY);
            emit(NOLABEL, ADD, newReg4, newReg3, newReg5);
            emit(NOLABEL, LOADAO, 0, newReg5, newReg1);
            $$.targetRegister = newReg1;
            $$.type = assignment_entry->type;
          }
        }
 


	| ICONST { 

            int newReg        = NextRegister();
            $$.targetRegister = newReg;
            $$.type           = TYPE_INT;
            emit(NOLABEL, LOADI, $1.num, newReg, EMPTY);

        }
        | TRUE  { int newReg = NextRegister(); /* TRUE is encoded as value '1' */
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
      if($3.num > $5.num){ printf("\n***Error: lower bound exceeds upper bound\n"); }
      assignment_entry = lookup($1.str);
      if(assignment_entry == NULL){ printf("\n***Error: undeclared identifier %s\n", $1.str); }
      else{
        if(assignment_entry->type != TYPE_INT){ printf("\n***Error: induction variable not scalar integer variable\n"); }
        else{
          if(assignment_entry->offset == -1){
            assignment_entry->offset = NextOffset(1);
          }
          int newReg1 = NextRegister();
          int newReg2 = NextRegister();
          int newReg3 = NextRegister();
          int newReg4 = NextRegister();
          int newReg5 = NextRegister(); //r11
          int newReg6 = NextRegister(); //r12 target

          $$.name     = assignment_entry->name;
          $$.offset   = assignment_entry->offset;
          $$.type     = TYPE_BOOL;
          $$.iterator = $3.num;
          $$.target   = $5.num;
          // maybe we should add the target register?
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
          emit(NOLABEL, CMPLE, $$.load_register, newReg6, $$.expression_register);
          emit(NOLABEL, CBR, $$.expression_register, $$.label_two, $$.label_three);
          emit($$.label_two, NOP, EMPTY, EMPTY, EMPTY);  
        }
      } 
}
        ;

condexp	: exp NEQ exp		{
          int comparison = NextRegister();
          int true_label = NextLabel();
          int else_label = NextLabel();
          emit(NOLABEL, CMPNE, $1.targetRegister, $3.targetRegister, comparison);
          emit(NOLABEL, CBR, comparison, true_label, else_label);
          emit(true_label, NOP, EMPTY, EMPTY, EMPTY);
          $$.label_one    = true_label;
          $$.label_two    = else_label;
          $$.type         = $1.type;
        } 

        | exp EQ exp		{
          int comparison = NextRegister();
          int true_label = NextLabel();
          int else_label = NextLabel();
          emit(NOLABEL, CMPEQ, $1.targetRegister, $3.targetRegister, comparison);
          emit(NOLABEL, CBR, comparison, true_label, else_label);
          emit(true_label, NOP, EMPTY, EMPTY, EMPTY);
          $$.label_one    = true_label;
          $$.label_two    = else_label;
          $$.type         = $1.type;
        } 

        | exp LT exp		{
          int comparison = NextRegister();
          int true_label = NextLabel();
          int else_label = NextLabel();
          emit(NOLABEL, CMPLT, $1.targetRegister, $3.targetRegister, comparison);
          emit(NOLABEL, CBR, comparison, true_label, else_label);
          emit(true_label, NOP, EMPTY, EMPTY, EMPTY);
          $$.label_one    = true_label;
          $$.label_two    = else_label;
          $$.type         = $1.type;
        }

        | exp LEQ exp		{
          int comparison = NextRegister();
          int true_label = NextLabel();
          int else_label = NextLabel();
          emit(NOLABEL, CMPLE, $1.targetRegister, $3.targetRegister, comparison);
          emit(NOLABEL, CBR, comparison, true_label, else_label);
          emit(true_label, NOP, EMPTY, EMPTY, EMPTY);
          $$.label_one    = true_label;
          $$.label_two    = else_label;
          $$.type         = $1.type;
        }

	| exp GT exp		{  
          int comparison = NextRegister();
          int true_label = NextLabel();
          int else_label = NextLabel();
          emit(NOLABEL, CMPGT, $1.targetRegister, $3.targetRegister, comparison);
          emit(NOLABEL, CBR, comparison, true_label, else_label);
          emit(true_label, NOP, EMPTY, EMPTY, EMPTY);
          $$.label_one    = true_label;
          $$.label_two    = else_label;
          $$.type         = $1.type;
  }

	| exp GEQ exp		{
          int comparison = NextRegister();
          int true_label = NextLabel();
          int else_label = NextLabel();
          emit(NOLABEL, CMPGE, $1.targetRegister, $3.targetRegister, comparison);
          emit(NOLABEL, CBR, comparison, true_label, else_label);
          emit(true_label, NOP, EMPTY, EMPTY, EMPTY);
          $$.label_one    = true_label;
          $$.label_two    = else_label;
          $$.type         = $1.type;
  }

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




