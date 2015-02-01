with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces;

with kv.avm.Memories;
with kv.avm.Assemblers;
with kv.avm.Instances;
with kv.avm.Machines;
with kv.avm.Processors;
with kv.avm.Registers;
with kv.avm.Log;

procedure volem is
   Builder : aliased kv.avm.Assemblers.Assembler_Type;
   package SIO renames Ada.Streams.Stream_IO;
   F : SIO.File_Type;
   S : SIO.Stream_Access;
   VM  : aliased kv.avm.Machines.Machine_Type;
   CPU : aliased kv.avm.Processors.Processor_Type;
   Instance_Factory : aliased kv.avm.Instances.Instance_Factory;
   Data : kv.avm.Memories.Register_Set_Type(0..8);
   Registers : kv.avm.Memories.Register_Array_Type;
   Count : Interfaces.Unsigned_32 := 0;
   Value : Interfaces.Integer_64;
   Reporting_Increment : Natural := 100000;
   Goal : Natural := Reporting_Increment;

   function Img(Value : Natural) return String is
      Image : constant String := Natural'IMAGE(Value);
   begin
      return Image(2 .. Image'LAST);
   end Img;

   procedure Report is
   begin
      Put_Line("[steps=" & Img(VM.Get_Steps) &
               " cycles=" & Img(VM.Get_Cycles) &
               " t=" & Img(VM.Get_Total) &
               " a=" & Img(VM.Get_Active) &
               " i=" & Img(VM.Get_Idle) &
               " b=" & Img(VM.Get_Blocked) &
               " d=" & Img(VM.Get_Deferred) &
               " r=" & Img(VM.Get_Reaped) &
               " q=" & Img(VM.Get_Queue_Size) &
               "]");
   end Report;

   use Interfaces;

begin
   if Argument_Count < 3 then
      Put_Line("USAGE ERROR");
      Put_Line("usage: volem <word-code-file> <start-actor-name> <start-message-name> [arguments...]");
   end if;
   Put_Line("volem -- the vole virtual machine processing "&Argument(1));
   for I in 4 .. Argument_Count loop
      if Argument(I) = "-v" then
         kv.avm.Log.Verbose := True;
      --elsif Argument(I) = "-ss" then
      else
         Value := Interfaces.Integer_64'VALUE(Argument(I));
         Data(Count) := kv.avm.Registers.Make_S(Value);
         Count := Count + 1;
      end if;
   end loop;
   Builder.Initialize;
   SIO.Open(F, SIO.In_File, Argument(1));
   S := SIO.Stream(F);
   Builder.Read_Word_Code(S);
   SIO.Close(F);
   VM.Initialize(CPU'UNCHECKED_ACCESS, Instance_Factory'UNCHECKED_ACCESS);
   CPU.Initialize(VM'UNCHECKED_ACCESS);

   Builder.Transfer_Actors_To_Store;

   Registers.Initialize(Data(0..Count));
   VM.Start_With(Argument(2), Argument(3), Registers);

   Put_Line("Running...");

   while not VM.Done loop
      begin
         VM.Step;
         if VM.Done then
            VM.Deliver_Messages;
         end if;
         if VM.Get_Steps >= Goal then
            Report;
            Goal := Goal + Reporting_Increment;
         end if;
      exception
         when Error: others =>
            Put_Line("VM terminating due to exception: "&Exception_Information(Error));
            Put_Line("VM terminating after "&Natural'IMAGE(VM.Get_Steps)&" steps.");
            Put_Line("VM instance information: "&VM.Current_Instance.Debug_Info);
            raise;
      end;
   end loop;
   Put_Line("Completed after "&Natural'IMAGE(VM.Get_Steps)&" steps.");
   if CPU.Get_Failed_Assertion_Count > 0 then
      Put_Line("Execution failure, "&Natural'IMAGE(CPU.Get_Failed_Assertion_Count)&" failed assertion(s).");
      Set_Exit_Status(Failure);
   end if;
end volem;
