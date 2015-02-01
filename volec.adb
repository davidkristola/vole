-- This file is freely given to the Public Domain.
--
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams.Stream_IO;

with Vole_Tokens;
with kv.avm.Vole_Lex;
with Vole_Lex_IO;
with kv.avm.vole_parser;
with kv.avm.vole_tree;
with kv.avm.Tree_Dots;
with kv.avm.Tree_Rewrite;
with kv.avm.Code_Generator;
with kv.avm.Assemblers;
with kv.avm.Log;

procedure volec is

   package SIO renames Ada.Streams.Stream_IO;

   function Last_Five(File_Name : String) return String is
      Answer : String := "     ";
      Index : Positive := File_Name'LAST;
   begin
      for I in reverse Answer'RANGE loop
         Answer(I) := File_Name(Index);
         if Index > File_Name'FIRST then
            Index := Index - 1;
         end if;
      end loop;
      return Answer;
   end Last_Five;

   function Sans_Extension(File_Name : String) return String is
      B : constant Positive := File_Name'FIRST;
      E : constant Positive := File_Name'LAST - 4; -- Assume valid ".vole" extension.
   begin
      return File_Name(B..E);
   end Sans_Extension;

   procedure Compile(File_Name : String) is

      Grapher  : aliased kv.avm.Tree_Dots.Grapher_Class;
      Rewriter  : aliased kv.avm.Tree_Rewrite.Rewriter_Class;
      Code_Gen  : aliased kv.avm.Code_Generator.Code_Generator_Class;
      Assembler : aliased kv.avm.Assemblers.Assembler_Type;

      F : SIO.File_Type;
      S : SIO.Stream_Access;

      procedure Assemble_Line(Line : String) is
      begin
         Assembler.Parse_Line(Line);
      end Assemble_Line;

   begin
      if Last_Five(File_Name) /= ".vole" then
         Put_Line("ERROR: File name '" & File_Name & "' does not end with '.vole'");
         return;
      end if;
      Put_Line("volec "& File_Name);
      Vole_Lex_IO.open_input(File_Name);
      Vole_Lex_IO.create_output;
      kv.avm.vole_parser.yyparse;
      Vole_Lex_IO.close_input;
      Vole_Lex_IO.close_output;
   
      Rewriter.Init;
      kv.avm.vole_tree.Get_Program.Visit(Rewriter'ACCESS, 0);
      Rewriter.Finalize;
   
      Grapher.Init(Sans_Extension(File_Name) & "dot");
      kv.avm.vole_tree.Get_Program.Visit(Grapher'ACCESS, 0);
      Grapher.Finalize;

      Code_Gen.Init;
      kv.avm.vole_tree.Get_Program.Visit(Code_Gen'ACCESS, 0);
      Assembler.Initialize;
      Code_Gen.Process_Lines(Assemble_Line'ACCESS);
      Code_Gen.Finalize;

      SIO.Create(F, SIO.Out_File, kv.avm.Assemblers.Make_Word_Code_Name(File_Name & 'a'));
      S := SIO.Stream(F);
      Assembler.Write_Word_Code(S);
      SIO.Close(F);
   end Compile;

begin
   for I in 1 .. Argument_Count loop
      if Argument(I) = "-v" then
         kv.avm.Log.Verbose := True;
      elsif Argument(I) = "-vv" then
         kv.avm.vole_parser.Verbose := True;
      else
         Compile(Argument(I));
      end if;
   end loop;
exception
   when kv.avm.vole_tree.Terminate_Parsing =>
      Set_Exit_Status(Failure);
   when Error: others =>
      Put_Line("EXCEPTION: "&Exception_Information(Error));
      Set_Exit_Status(Failure);
end volec;
