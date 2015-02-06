
DIGIT_SEQUENCE    [0-9]([_]?[0-9])*

HEX_SEQUENCE      [0-9a-fA-F]([_]?[0-9a-fA-F])*

EXPONENT          [eE][-+]?{DIGIT_SEQUENCE}

--FLOAT_SEQUENCE    {DIGIT_SEQUENCE}[.]{DIGIT_SEQUENCE}{EXPONENT}?
FLOAT_SEQUENCE    {DIGIT_SEQUENCE}[.]{DIGIT_SEQUENCE}


%%

"actor"     { return Key_Actor; }
"assert"    { return Key_Assert; }
"attribute" { return Key_Attribute; }
"case"      { return Key_Case; }
"constructor" { return Key_Constructor; }
"else"      { return Key_Else; }
"elseif"    { return Key_Elseif; }
"emit"      { return Key_Emit; }
"endif"     { return Key_Endif; }
"extends"   { return Key_Extends; }
"for"       { return Key_For; }
"if"        { return Key_If; }
"import"    { return Key_Import; }
"return"    { return Key_Return; }
"returns"   { return Key_Returns; }
"local"     { return Key_Local; }
"message"   { return Key_Message; }
"method"    { return Key_Method; }
"self"      { return Key_Self; }
"send"      { return Key_Send; }
"super"     { return Key_Super; }
"then"      { return Key_Then; }
"tuple"     { return Key_Tuple; }
"when"      { return Key_When; }
"while"     { return Key_While; }
"loop"      { return Key_Loop; }
"new"       { return Key_New; }
"predicate" { return Key_Predicate; }

"<<"      { return Op_Shift_Left; }
">>"      { return Op_Shift_Right; }
"**"      { return Op_Exp; }
"+"       { return Op_Add; }
"-"       { return Op_Sub; }
"*"       { return Op_Mul; }
"/"       { return Op_Div; }
"%"|"mod" { return Op_Mod; }
"="|"=="  { return Op_Eq; }
"!="|"/=" { return Op_Not_Eq; }
">"       { return Op_Gt; }
">="      { return Op_Gt_Eq; }
"<="      { return Op_Lt_Eq; }
"<"       { return Op_Lt; }
":="      { return Op_Assign; }
"and"|"&" { return Op_And; }
"or"|"|"  { return Op_Or; }
"xor"     { return Op_Xor; }
"!"|"not" { return Op_Not; }

"Actor"    { return Actor_Type; }
"Boolean"  { return Boolean_Type; }
"Integer"  { return Integer_Type; }
"Unsigned" { return Unsigned_Type; }
"Float"    { return Float_Type; }
"String"   { return String_Type; }
"Tuple"    { return Tuple_Type; }

"True"  { return True_Literal; }
"False" { return False_Literal; }

[a-zA-Z]([_]?[a-zA-Z0-9])*     { return Id_Token; }

([0-9]([_]?[0-9])*)[\.]([0-9]([_]?[0-9])*)([eE][-+]?([0-9]([_]?[0-9])*))?  { return Float_Literal; }

[0-9]([_]?[0-9])*              { return Integer_Literal; }

\"(\\.|[^\\"])*\"              { return String_Literal; }

":"         { return Colon_Token; }
"."         { return Dot_Token; }
","         { return Comma_Token; }
";"         { return Eos_Token; }
"{"         { return Block_Begin; }
"}"         { return Block_End; }
"("         { return Paren_Begin; }
")"         { return Paren_End; }
"["         { return Tuple_Begin; }
"]"         { return Tuple_End; }

"#".*       { null; }
[\n]        { Inc_Line; }

[\t ]+      { null; }


%%

with Vole_Tokens;
use Vole_Tokens;

package kv.avm.Vole_Lex is

   Line_Number : Positive := 1;

   procedure Report;

   procedure Inc_Line;

   function YYLex return Token;

end kv.avm.Vole_Lex;

with Ada.Text_IO;
use Ada.Text_IO;

package body kv.avm.Vole_Lex is

   procedure Report is
   begin
      Put_Line(yytext);
   end Report;

   procedure Inc_Line is
   begin
      Line_Number := Line_Number + 1;
   end Inc_Line;

##

end kv.avm.Vole_Lex;
