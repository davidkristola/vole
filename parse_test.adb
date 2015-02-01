-- This file is freely given to the Public Domain.
--
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

with Vole_Tokens;
with kv.avm.Vole_Lex;
with Vole_Lex_IO;
with kv.avm.vole_parser;
with kv.avm.vole_tree;
with kv.avm.Tree_Dots;
with kv.avm.Tree_Rewrite;
with kv.avm.Code_Generator;

procedure parse_test is

   subtype String_Type is Ada.Strings.Unbounded.Unbounded_String;

   -- Conversion operators to go back and forth between strings and unbounded strings.
   --
   function "+"(S : String) return String_Type renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+"(U : String_Type) return String renames Ada.Strings.Unbounded.To_String;

   File_Name : String_Type := +"test_1.vole";
   Grapher : aliased kv.avm.Tree_Dots.Grapher_Class;
   Rewriter : aliased kv.avm.Tree_Rewrite.Rewriter_Class;
   Code_Gen : aliased kv.avm.Code_Generator.Code_Generator_Class;

begin
   for I in 1 .. Argument_Count loop
      if Argument(I) = "-v" then
         kv.avm.vole_parser.Verbose := True;
      else
         File_Name := +Argument(I);
      end if;
   end loop;
   Put_Line("//parse_test -- the vole parser test "&(+File_Name));
   Vole_Lex_IO.open_input(+File_Name);
   Vole_Lex_IO.create_output;
   kv.avm.vole_parser.yyparse;
   Vole_Lex_IO.close_input;
   Vole_Lex_IO.close_output;

   Rewriter.Init;
   kv.avm.vole_tree.Get_Program.Visit(Rewriter'ACCESS, 0);
   Rewriter.Finalize;

   Grapher.Init("program.dot");
   kv.avm.vole_tree.Get_Program.Visit(Grapher'ACCESS, 0);
   Grapher.Finalize;

   Code_Gen.Init;
   kv.avm.vole_tree.Get_Program.Visit(Code_Gen'ACCESS, 0);
   Code_Gen.Print;
   Code_Gen.Finalize;
end parse_test;
