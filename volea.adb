with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO;

with kv.avm.Assemblers;
with kv.avm.Log;

procedure volea is
   Builder : aliased kv.avm.Assemblers.Assembler_Type;
   package SIO renames Ada.Streams.Stream_IO;
   F : SIO.File_Type;
   S : SIO.Stream_Access;
begin
   for I in 1 .. Argument_Count loop
      if Argument(I) = "-v" then
         kv.avm.Log.Verbose := True;
      else
         Put_Line("volea -- the vole assembler processing "&Argument(I));
         Builder.Initialize;
         Builder.Parse_Input_File(Argument(I));
      
         SIO.Create(F, SIO.Out_File, kv.avm.Assemblers.Make_Word_Code_Name(Argument(I)));
         S := SIO.Stream(F);
         Builder.Write_Word_Code(S);
         SIO.Close(F);
      end if;
   end loop;
end volea;
