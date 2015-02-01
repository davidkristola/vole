%token Key_Import Id_Token Eos_Token Block_Begin Block_End
%token Key_Attribute Key_Predicate Key_Message Key_Returns Key_Actor Key_Constructor Key_Extends

%token Key_Assert
%token Key_Method Key_Emit
%token Key_If
%token Key_Then Key_Self Key_Super Key_Send
%token Key_Else
%token Key_Elseif
%token Key_Endif
%token Key_When
%token Key_While Key_Do Key_Loop
%token Key_For
%token Key_Case
%token Key_Local
%token Key_New
%token Op_And Op_Not
%token Op_Or Op_Xor
%token Op_Mod
%token Op_Eq
%token Op_Not_Eq
%token Op_Gt
%token Op_Gt_Eq
%token Op_Lt_Eq
%token Op_Lt
%token Key_Return Key_Tuple
%token Float_Literal Integer_Literal String_Literal True_Literal False_Literal Actor_Type Boolean_Type Tuple_Type Unsigned_Type
%token Integer_Type Float_Type String_Type
%token Colon_Token Dot_Token Paren_Begin Paren_End Comma_Token
%token Tuple_Begin Tuple_End
%token Op_Shift_Left Op_Shift_Right

%token      Op_Add Op_Sub
%token      Op_Mul Op_Div
%token      HIGH_RIGHT_PRECEDENCE
%token      Op_Exp Op_Assign

%nonassoc  Op_Assign
%nonassoc  Op_And Op_Or Op_Xor
%nonassoc  Op_Eq Op_Not_Eq Op_Gt Op_Gt_Eq Op_Lt_Eq Op_Lt
%left      Op_Shift_Left Op_Shift_Right
%left      Op_Add Op_Sub
%left      Op_Mul Op_Div Op_Mod
%right     Op_Not HIGH_RIGHT_PRECEDENCE
%nonassoc  Op_Exp Paren_Begin

%with kv.avm.vole_tree
{
   type YYSType is
      record
         Node : kv.avm.vole_tree.Node_Pointer;
      end record;
}

%start program

%%

-- Tokens are in mixed case, non-terminals are in lower case (ayacc is case insensitive)
-- "_x" means "empty or x"

program
   : _imports actors
      {
         Build_Program($$.Node, Line_Number, $1.Node, $2.Node);
         Save_Program($$.Node);
      }
   ;

empty
   :
   ;

_imports
   : empty
   | imports {Put_Line("unimp 01");}
   ;

imports
   : _imports import {Put_Line("unimp 02");}
   ;

import
   : Key_Import identifier Eos_Token {Put_Line("unimp 03");}
   ;

actors
   : actor Eos_Token _actors
      {
         $$.Node := $1.Node; -- Copy up
         $1.Node := null; -- Only keep one reference to this node.
         Add_Next($$.Node, $3.Node);
      }
   ;

_actors
   : empty  { $$.Node := null;}
   | actors { $$.Node := $1.Node;}
   ;

actor
   : Key_Actor identifier Block_Begin _attributes methods Block_End
      { Build_Actor_Node($$.Node, Line_Number, $2.Node, $4.Node, $5.Node); }
   | Key_Actor identifier Key_Extends identifier Block_Begin _attributes methods Block_End
      { Build_Actor_Node($$.Node, Line_Number, $2.Node, $6.Node, $7.Node, $4.Node); }
   ;

_attributes
   : empty      { $$.Node := null;}
   | attributes { $$.Node := $1.Node;}
   ;

attributes
   : attribute _attributes
      {
         $$.Node := $1.Node; -- Copy up
         $1.Node := null; -- Only keep one reference to this node.
         Add_Next($$.Node, $2.Node);
      }
   ;

attribute
   : Key_Attribute identifier Colon_Token type_opt_init Eos_Token {Build_Attribute($$.Node, Line_Number, $2.Node, $4.Node);}
   | Key_Predicate identifier end_of_predicate                    {Build_Attribute($$.Node, Line_Number, $2.Node, $3.Node);}
   ;

end_of_predicate
   :                           Eos_Token {Build_Kind($$.Node, Line_Number, Bit_Or_Boolean);}
   | Op_Assign boolean_literal Eos_Token {Build_Kind($$.Node, Line_Number, Bit_Or_Boolean, $2.Node);}
   ;

type_opt_init
   : integer_opt_init  { $$.Node := $1.Node;}
   | unsigned_opt_init { $$.Node := $1.Node;}
   | float_opt_init    { $$.Node := $1.Node;}
   | string_opt_init   { $$.Node := $1.Node;}
   | actor_opt_init    { $$.Node := $1.Node;}
   | tuple_opt_init    { $$.Node := $1.Node;}
   | boolean_opt_init  { $$.Node := $1.Node;}
   ;

integer_opt_init
   : Integer_Type
      {Build_Kind($$.Node, Line_Number, Signed_Integer);}
   | Integer_Type Op_Assign expression
      {Build_Kind($$.Node, Line_Number, Signed_Integer, $3.Node);}
   ;

unsigned_opt_init
   : Unsigned_Type
      {Build_Kind($$.Node, Line_Number, Unsigned_Integer);}
   | Unsigned_Type Op_Assign expression
      {Build_Kind($$.Node, Line_Number, Unsigned_Integer, $3.Node);}
   ;

float_opt_init
   : Float_Type {Put_Line("unimp 06");}
   | Float_Type Op_Assign Float_Literal {Put_Line("unimp 07");}
   ;

string_opt_init
   : String_Type {Put_Line("unimp 08");}
   | String_Type Op_Assign String_Literal {Build_Kind($$.Node, Line_Number, Immutable_String, $3.Node);}
   ;

actor_opt_init
   : Actor_Type
      {Build_Kind($$.Node, Line_Number, Actor_Definition);}
   | Actor_Type Op_Assign identifier
      {Build_Kind($$.Node, Line_Number, Actor_Definition, $3.Node);}
   | Actor_Type Op_Assign Key_New identifier arg_values
      {Build_Kind($$.Node, Line_Number, Actor_Definition, New_Constructor_Send($4.Node, $5.Node)); }
   ;

tuple_opt_init
   : Tuple_Type {Put_Line("unimp 10");}
   | Tuple_Type Op_Assign tuple {Put_Line("unimp 11");}
   ;

boolean_opt_init
   : Boolean_Type
      {Build_Kind($$.Node, Line_Number, Bit_Or_Boolean);}
   | Boolean_Type Op_Assign expression
      {Build_Kind($$.Node, Line_Number, Bit_Or_Boolean, $3.Node);}
   ;


type_mark
   : Integer_Type
      {Build_Kind($$.Node, Line_Number, Signed_Integer);}
   | Unsigned_Type
      {Build_Kind($$.Node, Line_Number, Unsigned_Integer);}
   | Float_Type
      {Build_Kind($$.Node, Line_Number, Floatingpoint);}
   | Actor_Type
      {Build_Kind($$.Node, Line_Number, Actor_Definition);}
   | Tuple_Type
      {Build_Kind($$.Node, Line_Number, Tuple);}
   | String_Type
      {Build_Kind($$.Node, Line_Number, Immutable_String);}
   ;

methods
   : method Eos_Token _methods
      {
         $$.Node := $1.Node; -- Copy up
         $1.Node := null; -- Only keep one reference to this node.
         Add_Next($$.Node, $3.Node);
      }
   ;

_methods
   : empty   { $$.Node := null;}
   | methods { $$.Node := $1.Node;}
   ;

method
   : Key_Message identifier arg_list Key_Returns arg_list code_block
      {Build_Message($$.Node, Line_Number, True, $2.Node, $3.Node, $5.Node, $6.Node, null);}
   | Key_When identifier Key_Message identifier arg_list Key_Returns arg_list code_block
      {Build_Message($$.Node, Line_Number, True, $4.Node, $5.Node, $7.Node, $8.Node, $2.Node);}
   | Key_Method identifier arg_list Key_Returns arg_list code_block
      {Build_Message($$.Node, Line_Number, False, $2.Node, $3.Node, $5.Node, $6.Node, null);}
   | Key_Constructor arg_list code_block
      {Build_Constructor($$.Node, Line_Number, $2.Node, $3.Node);}
   ;

arg_list
   : Paren_Begin _arg_defs Paren_End
      {$$.Node := $2.Node;}
   ;

_arg_defs
   : empty    { $$.Node := null;}
   | arg_defs { $$.Node := $1.Node;}
   ;

arg_defs
   : arg_def
      {$$.Node := $1.Node;}
   | arg_def Comma_Token arg_defs
      {
         $$.Node := $1.Node; -- Copy up
         $1.Node := null; -- Only keep one reference to this node.
         Add_Next($$.Node, $3.Node);
      }
   ;

arg_def
   : identifier Colon_Token type_mark
      {Build_Arg($$.Node, Line_Number, $1.Node, $3.Node);}
   ;

identifier
   : Id_Token
      {Build_Id_Node($$.Node, Line_Number, yytext);}
   ;

code_block
   : Block_Begin _statements Block_End { $$.Node := $2.Node;}
   ;

_statements
   : empty      { $$.Node := null;}
   | statements { $$.Node := $1.Node;}
   ;

statements
   : statement Eos_Token _statements
      {
         $$.Node := $1.Node; -- Copy up
         $1.Node := null; -- Only keep one reference to this node.
         Add_Next($$.Node, $3.Node);
      }
   ;

statement
   : statement_if      { $$.Node := $1.Node;}
   | statement_return  { $$.Node := $1.Node;}
   | statement_assign  { $$.Node := $1.Node;}
   | statement_var_def { $$.Node := $1.Node;}
   | statement_emit    { $$.Node := $1.Node;}
   | statement_send    { $$.Node := $1.Node;}
   | statement_assert  { $$.Node := $1.Node;}
   | statement_loop    { $$.Node := $1.Node;}
   ;

statement_loop
   : Key_While expression Key_Do   code_block {Build_While($$.Node, Line_Number, $2.Node, $4.Node);} -- depricated
   | Key_While expression Key_Loop code_block {Build_While($$.Node, Line_Number, $2.Node, $4.Node);}
   |                      Key_Loop code_block {Build_While($$.Node, Line_Number, null,    $2.Node);}
   ;

statement_assert
   : Key_Assert expression {Build_Assert($$.Node, Line_Number, $2.Node);}
   ;

statement_send
   : Key_Send send_option { $$.Node := $2.Node;}
   ;

send_option
   : send_self  { $$.Node := $1.Node;}
   | send_actor { $$.Node := $1.Node;}
   ;

statement_if
   : Key_If expression Key_Then Block_Begin _statements rest_of_if {Build_If($$.Node, Line_Number, $2.Node, $5.Node, $6.Node);}
   ;

rest_of_if
   : Block_End { $$.Node := null;} -- No else part so return null.
   | Block_End Key_Else code_block { $$.Node := $3.Node;}
   | Block_End Key_Elseif expression Key_Then Block_Begin _statements rest_of_if {Build_If($$.Node, Line_Number, $2.Node, $5.Node, $6.Node);}
   ;

statement_return
   : Key_Return return_option {Build_Return($$.Node, Line_Number, $2.Node);}
   ;

return_option
   : empty      { $$.Node := null;}
   | expression { $$.Node := $1.Node;}
   | tuple      { $$.Node := $1.Node;}
   | call_self  { $$.Node := $1.Node;}
   | call_super { $$.Node := $1.Node;}
   | call_actor { $$.Node := $1.Node;}
   ;

arg_values
   : Paren_Begin _expression_list Paren_End
      { $$.Node := $2.Node; }
   ;

tuple
   : Tuple_Begin _expression_list Tuple_End {Put_Line("unimp 30");}
   ;

-- Tuple associations look like tuples, but the elements must be variables (and can be empty)
associations
   : Tuple_Begin variable_list Tuple_End {Put_Line("unimp 29");}
   ;

send_self
   : Key_Self Dot_Token identifier arg_values {Build_Send_Statement($$.Node, Line_Number, Self, null, $3.Node, $4.Node);}
   ;

send_actor
   : variable Dot_Token identifier arg_values {Build_Send_Statement($$.Node, Line_Number, Actor, $1.Node, $3.Node, $4.Node);}
   ;

call_self
   : Key_Self Dot_Token identifier arg_values {Build_Call_Statement($$.Node, Line_Number, Self, null, $3.Node, $4.Node);}
   ;

call_super
   : Key_Super Dot_Token identifier arg_values {Build_Call_Statement($$.Node, Line_Number, Super, null, $3.Node, $4.Node);}
   ;

call_actor
   : variable Dot_Token identifier arg_values {Build_Call_Statement($$.Node, Line_Number, Actor, $1.Node, $3.Node, $4.Node);}
   ;

statement_assign
   : variable     Op_Assign assignment_rhs    {Build_Assignment($$.Node, Line_Number, $1.Node, $3.Node);}
   | associations Op_Assign assignment_rhs    {Raise_Exception(Unimplemented_Error'IDENTITY, "Rule statement_assign (tuple)");}
   ;

assignment_rhs
   : expression    { $$.Node := $1.Node;}
   | call_self    { $$.Node := $1.Node;}
   | call_actor    { $$.Node := $1.Node;}
   | new_actor     { $$.Node := $1.Node;}
   ;

new_actor
   : Key_New new_actor_constructor { Build_Op_Expression($$.Node, Line_Number, Op_Pos, null, $2.Node); }
   ;

new_actor_constructor
   : identifier arg_values { Build_Kind($$.Node, Line_Number, Actor_Definition, New_Constructor_Send($1.Node, $2.Node)); }
   ;

variable
   : Key_Self Dot_Token identifier
      {Build_Var_Expression($$.Node, Line_Number, True, $3.Node);}
   | identifier
      {Build_Var_Expression($$.Node, Line_Number, False, $1.Node);}
   ;

variable_list
   : empty                               {Put_Line("unimp 35");}
   | variable                            {Put_Line("unimp 36");}
   | empty    Comma_Token variable_list  {Put_Line("unimp 37");}
   | variable Comma_Token variable_list  {Put_Line("unimp 38");}
   ;

_expression_list
   : empty    { $$.Node := null;}
   | expression_list
      { $$.Node := $1.Node; }
   ;

expression_list
   : expression
      { $$.Node := $1.Node; }
   | expression Comma_Token expression_list
      {
         $$.Node := $1.Node; -- Copy up
         $1.Node := null; -- Only keep one reference to this node.
         Add_Next($$.Node, $3.Node);
      }
   ;

expression
   : expression Op_Add expression                  {Build_Op_Expression($$.Node, Line_Number, Add, $1.Node, $3.Node);}
   | expression Op_Sub expression                  {Build_Op_Expression($$.Node, Line_Number, Sub, $1.Node, $3.Node);}
   | expression Op_Mul expression                  {Build_Op_Expression($$.Node, Line_Number, Mul, $1.Node, $3.Node);}
   | expression Op_Div expression                  {Build_Op_Expression($$.Node, Line_Number, Div, $1.Node, $3.Node);}
   | expression Op_Mod expression                  {Build_Op_Expression($$.Node, Line_Number, Modulo, $1.Node, $3.Node);}
   | expression Op_Exp expression                  {Build_Op_Expression($$.Node, Line_Number, Exp, $1.Node, $3.Node);}
   | expression Op_Eq expression                   {Build_Op_Expression($$.Node, Line_Number, Eq, $1.Node, $3.Node);}
   | expression Op_Not_Eq expression               {Build_Op_Expression($$.Node, Line_Number, Neq, $1.Node, $3.Node);}
   | expression Op_Lt expression                   {Build_Op_Expression($$.Node, Line_Number, L_t, $1.Node, $3.Node);}
   | expression Op_Lt_Eq expression                {Build_Op_Expression($$.Node, Line_Number, Lte, $1.Node, $3.Node);}
   | expression Op_Gt expression                   {Build_Op_Expression($$.Node, Line_Number, G_t, $1.Node, $3.Node);}
   | expression Op_Gt_Eq expression                {Build_Op_Expression($$.Node, Line_Number, Gte, $1.Node, $3.Node);}
   | expression Op_Shift_Left expression           {Build_Op_Expression($$.Node, Line_Number, Shift_Left, $1.Node, $3.Node);}
   | expression Op_Shift_Right expression          {Build_Op_Expression($$.Node, Line_Number, Shift_Right, $1.Node, $3.Node);}
   | expression Op_And expression                  {Build_Op_Expression($$.Node, Line_Number, B_And, $1.Node, $3.Node);}
   | expression Op_Or  expression                  {Build_Op_Expression($$.Node, Line_Number, B_Or , $1.Node, $3.Node);}
   | expression Op_Xor expression                  {Build_Op_Expression($$.Node, Line_Number, B_Xor, $1.Node, $3.Node);}
   | Paren_Begin expression Paren_End              { $$.Node := $2.Node;}
   | Op_Not expression %prec HIGH_RIGHT_PRECEDENCE {Build_Op_Expression($$.Node, Line_Number, Negate, $2.Node);}
   | Op_Sub expression %prec HIGH_RIGHT_PRECEDENCE {Build_Op_Expression($$.Node, Line_Number, Op_Neg, $2.Node);}
   | Op_Add expression %prec HIGH_RIGHT_PRECEDENCE {Build_Op_Expression($$.Node, Line_Number, Op_Pos, $2.Node);}
   | variable                                      { $$.Node := $1.Node;}
   | literal                                       { $$.Node := $1.Node;}
   ;

literal
   : Integer_Literal     {Build_Literal_Expression($$.Node, Line_Number, Signed_Integer, yytext);}
   | Float_Literal       {Build_Literal_Expression($$.Node, Line_Number, Floatingpoint, yytext);}
   | String_Literal      {Build_Literal_Expression($$.Node, Line_Number, Immutable_String, yytext);}
   | boolean_literal     { $$.Node := $1.Node;}
   ; --!@#$ unsigned

boolean_literal
   : True_Literal        {Build_Literal_Expression($$.Node, Line_Number, Bit_Or_Boolean, yytext);}
   | False_Literal       {Build_Literal_Expression($$.Node, Line_Number, Bit_Or_Boolean, yytext);}
   ;

statement_var_def
   : Key_Local identifier Colon_Token type_opt_init
      {Build_Var_Def($$.Node, Line_Number, $2.Node, $4.Node);}
   ;

statement_emit
   : Key_Emit expression   {Build_Emit($$.Node, Line_Number, $2.Node);}
   ;

%%

package kv.avm.vole_parser is
   Unimplemented_Error : exception;
   procedure YYParse;
   Verbose : Boolean := False;
end kv.avm.vole_parser;

with Ada.Text_IO;
with Ada.Exceptions;
with Text_IO; -- Old style
with Vole_Tokens;
with Vole_Goto;
with Vole_Shift_Reduce;
with vole_lex_dfa;
with kv.avm.Vole_Lex;
with kv.avm.Vole_Tree;
with kv.avm.Instructions;
with kv.avm.Registers;

use Ada.Text_IO;
use Ada.Exceptions;
use Text_IO; -- Old style
use Vole_Tokens;
use Vole_Goto;
use Vole_Shift_Reduce;
use vole_lex_dfa;
use kv.avm.Vole_Lex;
use kv.avm.Vole_Tree;
use kv.avm.Instructions;
use kv.avm.Registers;

package body kv.avm.vole_parser is

   procedure YYError(Text : in String) is
   begin
      New_Line;
      Put_Line("PARSE ERROR on line"&Positive'IMAGE(kv.avm.Vole_Lex.Line_Number)&" parsing '"&vole_lex_dfa.yytext&"'");
   end YYError;

##

end kv.avm.vole_parser;
