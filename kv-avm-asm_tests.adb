with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

with AUnit.Assertions; use AUnit.Assertions;

with kv.avm.references; use kv.avm.references;
with kv.avm.Memories;
with kv.avm.Assemblers;
with kv.avm.Instructions;
with kv.avm.Registers;
with kv.avm.Instances;
with kv.avm.Machines;
with kv.avm.Processors;
with kv.avm.Tuples;
with kv.avm.Actor_References;
with kv.avm.actor_pool;
with kv.avm.Actors;
with kv.avm.log;
with kv.avm.Methods;
with kv.avm.Control;
with kv.avm.Messages;
with kv.avm.Capabilities;

package body kv.avm.Asm_Tests is

   -----------------------------------------------------------------------------
   function U(Value : Interfaces.Unsigned_64) return kv.avm.Registers.Register_Type is
   begin
      return (format => kv.avm.Registers.Unsigned_Integer, unsigned_value => Value);
   end U;

   -----------------------------------------------------------------------------
   function Name (T : Test_A1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A1: basic parsing.");
   end Name;

   procedure Run_Test (T : in out Test_A1) is
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      Ref : Reference_Type;
      use kv.avm.Instructions;
   begin
      Put_Line("test A1");
      Ref := Make_Reference("C0");
      Assert(Ref.Memory = Fixed, "Improperly built reference");
      Assert(Ref.Index = 0, "Improperly built reference");

      Ref := Make_Reference("I25");
      Assert(Ref.Memory = Input, "Improperly built reference");
      Assert(Ref.Index = 25, "Improperly built reference");

      Ref := Make_Reference("I0");
      Assert(Ref.Memory = Input, "Improperly built reference");
      Assert(Ref.Index = 0, "Improperly built reference");

      Ref := Make_Reference("a3");
      Assert(Ref.Memory = Attribute, "Improperly built reference");
      Assert(Ref.Index = 3, "Improperly built reference");

      Ref := Make_Reference("L63");
      Assert(Ref.Memory = Local, "Improperly built reference");
      Assert(Ref.Index = 63, "Improperly built reference");

      Builder.Initialize;
      Assert(Builder.Lines_Parsed = 0, "Lines_Parsed");
      Builder.Parse_Line("# comment");
      Assert(Builder.Lines_Parsed = 1, "Lines_Parsed");
      Builder.Parse_Line(""); -- blank
      Assert(Builder.Lines_Parsed = 2, "Lines_Parsed");
      Builder.Parse_Line("# comment");
      Builder.Parse_Line(".actor Test_A1");
      Assert(Builder.Actor.Name.all = "Test_A1", "Actor error");
      Builder.Parse_Line(" C0 uint 0");
      Builder.Parse_Line("..message CONSTRUCTOR");
      Builder.Parse_Line("  REPLY C0"); -- Format 1
      Assert(Builder.Actor.Message.Name.all = "CONSTRUCTOR", "Message Error");
      Assert(Builder.Actor.Message.Count = 1, "Message Error");
      Assert(Builder.Actor.Message.Code(0) = (op_code => REPLY, value => (memory => fixed, index => 0)), "Instruction Parsing Error");
      Builder.Parse_Line("  terminate_program"); -- Format 0
      Assert(Builder.Actor.Message.Code(1) = (op_code => TERMINATE_PROGRAM), "Instruction Parsing Error");
      Builder.Parse_Line("  SET L3 C7"); -- Format 2
      Assert(Builder.Actor.Message.Code(2) = (op_code => SET,
                                              lhs => (memory => local, index => 3),
                                              rhs => (memory => fixed, index => 7)), "Instruction Parsing Error");
      Builder.Parse_Line("  trap L4 L3 C7"); -- Format 3
      Assert(Builder.Actor.Message.Code(3) = (op_code => trap,
                                              a => (memory => local, index => 4),
                                              x => (memory => local, index => 3),
                                              y => (memory => fixed, index => 7)), "Instruction Parsing Error");
      Builder.Parse_Line("  EMIT I0"); -- Format 1
      Assert(Builder.Actor.Message.Count = 5, "Instruction Count Error");
      -- Format 3b
      Builder.Parse_Line("  COMPUTE A1 := L1 * L2"); -- Format 3b
      Assert(Builder.Actor.Message.Count = 6, "Instruction Count Error");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_A2) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A2: a few more parsing tricks.");
   end Name;

   procedure Run_Test (T : in out Test_A2) is
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      use kv.avm.Registers;
   begin
      Put_Line("test A2");
      Builder.Initialize;
      Builder.Parse_Line(".actor Fred");
      Builder.Parse_Line("..message Message_1");
      Builder.Parse_Line("..message Message_2");
      Builder.Parse_Line(".actor Wilma");
      Builder.Parse_Line(" ");
      Builder.Parse_Line(" C12 : uint := 347");
      Builder.Parse_Line(" C13 adef Test_A1_Calculator");
      Builder.Parse_Line(" C14 mdef Compute");
      Assert(Builder.Actor.F_Max = 14, "Wrong fixed max");
      Assert(Builder.Actor.Fixed(12).Format = Unsigned_Integer, "Expected Unsigned_Integer, found "&
         Data_Kind'IMAGE(Builder.Actor.Fixed(12).Format));
      Assert(Builder.Actor.Fixed(12).unsigned_value = 347, "Expected 347, found "&
         Interfaces.Unsigned_64'IMAGE(Builder.Actor.Fixed(12).unsigned_value));

      Assert(Builder.Actor.Fixed(13).Format = Actor_Definition, "Expected Actor_Definition, found "&
         Data_Kind'IMAGE(Builder.Actor.Fixed(13).Format));
      Assert(Builder.Actor.Fixed(14).Format = Message_Definition, "Expected Message_Definition, found "&
         Data_Kind'IMAGE(Builder.Actor.Fixed(14).Format));

      Assert((+Builder.Actor.Fixed(13).Actor_Kind) = "Test_A1_Calculator", "Expected Test_A1_Calculator, found "&
         (+Builder.Actor.Fixed(13).Actor_Kind));
      Assert((+Builder.Actor.Fixed(14).Message_Name) = "Compute", "Expected Compute, found "&
         (+Builder.Actor.Fixed(14).Message_Name));

      Assert(kv.avm.Assemblers.Make_Word_Code_Name("test_a1.volea") = "test_a1.volec", "Make_Word_Code_Name failed.");

      Builder.Parse_Line("..message Message_3 # just another messageg");

      -- Now append more constants onto the actor and make sure the last one is still 14.
      Builder.Parse_Line("..constants # append some more constants");
      Builder.Parse_Line(" C7 int64_t -13");
      Assert(Builder.Actor.F_Max = 14, "Last constant is not longer 14");
      Assert(Builder.Actor.Fixed(7).Format = Signed_Integer, "Expected Signed_Integer, found "&
         Data_Kind'IMAGE(Builder.Actor.Fixed(7).Format));
      Assert(Builder.Actor.Fixed(7).Signed_value = -13, "Expected -13, found "&
         Interfaces.Integer_64'IMAGE(Builder.Actor.Fixed(7).Signed_value));

      Builder.Parse_Line(" C8 : str := ""fr# ed""");
      Assert(Builder.Actor.Fixed(8).Format = Immutable_String, "Expected Immutable_String, found "&
         Data_Kind'IMAGE(Builder.Actor.Fixed(8).Format));
      Assert(Builder.Actor.Fixed(8).The_String = +"fr# ed", "The string was wrong.");

      Builder.Parse_Line(" C10 : TUPLE_MAP := [ L3, L6, L7] # ignore this comment");
      Assert(Builder.Actor.Fixed(10).Format = Tuple_Map, "Expected Tuple_Map, found "&
         Data_Kind'IMAGE(Builder.Actor.Fixed(10).Format));
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_A3) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A3: Parse a file.");
   end Name;

   procedure Run_Test (T : in out Test_A3) is
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
   begin
      Put_Line("test A3");
      Builder.Initialize;
      Builder.Parse_Input_File("test_a1.volea");
      Assert(True, "All is good.");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_A4) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A4: Load and execute asm.");
   end Name;

   procedure Run_Test (T : in out Test_A4) is
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Instance_Factory : aliased kv.avm.Instances.Instance_Factory;
      Data : aliased kv.avm.Tuples.Tuple_Type;
      Instance_1 : kv.avm.Actor_References.Actor_Reference_Type;
      Registers : kv.avm.Memories.Register_Array_Type;
      use kv.avm.Registers;
   begin
      Put_Line("test A4");
      Builder.Initialize;
      Builder.Parse_Input_File("test_a1.volea");
      Builder.Transfer_Actors_To_Store;

      VM.Initialize(CPU'UNCHECKED_ACCESS, Instance_Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);

      Registers.Initialize((Make_U(0), Make_U(1)));
      VM.Start_With("Test_A1", "Go", Registers);

      while not VM.Done loop
         VM.Step;
         Assert(VM.Get_Steps < 200, "Too many steps have been taken.");
      end loop;
      Assert(VM.Get_Steps = 60, "Wrong step count."&Natural'IMAGE(VM.Get_Steps));
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_A5) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A5: Streams.");
   end Name;

   procedure Run_Test (T : in out Test_A5) is
      pragma Unreferenced (T);
      package SIO renames Ada.Streams.Stream_IO;
      F : SIO.File_Type;
      S : SIO.Stream_Access;
      I : Integer;
   begin
      Put_Line("test A5");
      SIO.Create(F, SIO.Out_File, "test_a5.volec");
      S := SIO.Stream(F);
      Integer'WRITE(S, 1127);
      Integer'WRITE(S, 1349);
      SIO.Close(F);
      SIO.Open(F, SIO.In_File, "test_a5.volec");
      S := SIO.Stream(F);
      Integer'READ(S, I);
      Assert(I = 1127, "1127");
      Integer'READ(S, I);
      Assert(I = 1349, "1349");
      SIO.Close(F);
      Assert(True, "All is good.");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_A6) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A6: data stream.");
   end Name;

   procedure Run_Test (T : in out Test_A6) is
      pragma Unreferenced (T);
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      Builder2 : aliased kv.avm.Assemblers.Assembler_Type;
      package SIO renames Ada.Streams.Stream_IO;
      F : SIO.File_Type;
      S : SIO.Stream_Access;
      use kv.avm.Assemblers;
      use kv.avm.Registers;
      use kv.avm.Instructions;
      A1 : Actor_Pointer;
      A2 : Actor_Pointer;
      M1 : Message_Access;
      M2 : Message_Access;
      use kv.avm.Registers;
   begin
      Put_Line("test A6");
      --kv.avm.log.verbose := True;
      Builder.Initialize;
      Builder.Parse_Input_File("test_a1.volea");

      SIO.Create(F, SIO.Out_File, "test_a6.volec");
      S := SIO.Stream(F);
      Builder.Write_Word_Code(S);
      SIO.Close(F);

      Put_Line("test A6: reading back in the assembled word code.");
      SIO.Open(F, SIO.In_File, "test_a6.volec");
      S := SIO.Stream(F);
      Builder2.Initialize;
      Builder2.Read_Word_Code(S);
      SIO.Close(F);

      Put_Line("test A6: 2");
      Assert(Builder.Actor_Count = Builder2.Actor_Count, "Actor count.");
      Assert(Builder2.Actor /= null, "Actor list is null.");
      A1 := Builder.Actor;
      A2 := Builder2.Actor;
      Assert(A1.Name.all = A2.Name.all, "Actor name.");
      Assert(A1.F_Max = A2.F_Max, "Actor F_Max.");
      for I in A1.Fixed'FIRST .. Interfaces.Unsigned_32(A1.F_Max) loop
         Assert(A1.Fixed(I) = A2.Fixed(I), "Actor Fixed(I).");
      end loop;
      Assert(A1.M_Count = A2.M_Count, "Actor M_Count.");
      M1 := A1.Message;
      M2 := A2.Message;
      Assert(M1.Name.all = M2.Name.all, "Message name.");
      Assert(M1.Count = M2.Count, "Message (code) Count.");
      for I in M1.Code'FIRST .. Interfaces.Unsigned_32(M1.Count-1) loop
         Assert(M1.Code(I) = M2.Code(I), "Message code (I).");
      end loop;

      M1 := M1.Next;
      M2 := M2.Next;
      Assert(M1.Name.all = M2.Name.all, "Message name.");
      Assert(M1.Count = M2.Count, "Message (code) Count.");
      for I in M1.Code'FIRST .. Interfaces.Unsigned_32(M1.Count) loop
         Assert(M1.Code(I) = M2.Code(I), "Message code (I).");
      end loop;

      A1 := A1.Next;
      A2 := A2.Next;
      Assert(A1.Name.all = A2.Name.all, "Actor name.");
      Assert(A1.F_Max = A2.F_Max, "Actor F_Max.");
      for I in A1.Fixed'FIRST .. Interfaces.Unsigned_32(A1.F_Max) loop
         if A1.Fixed(I).Format = Unsigned_Integer then
            Assert(A1.Fixed(I) = A2.Fixed(I), "Actor Fixed(I)."&Interfaces.Unsigned_32'IMAGE(I));
         elsif A1.Fixed(I).Format = Actor_Definition then
            Assert(+A1.Fixed(I).Actor_Kind = +A2.Fixed(I).Actor_Kind, "Actor Fixed(I) actor def."&Interfaces.Unsigned_32'IMAGE(I));
         elsif A1.Fixed(I).Format = Message_Definition then
            Assert(+A1.Fixed(I).Message_Name = +A2.Fixed(I).Message_Name, "Actor Fixed(I) msg def."&Interfaces.Unsigned_32'IMAGE(I));
         end if;
      end loop;
      Assert(A1.M_Count = A2.M_Count, "Actor M_Count.");
      M1 := A1.Message;
      M2 := A2.Message;
      Assert(M1.Name.all = M2.Name.all, "Message name.");
      Assert(M1.Count = M2.Count, "Message (code) Count.");
      for I in M1.Code'FIRST .. Interfaces.Unsigned_32(M1.Count-1) loop
         Assert(M1.Code(I) = M2.Code(I), "Message code (I).");
      end loop;

      M1 := M1.Next;
      M2 := M2.Next;
      Assert(M1.Name.all = M2.Name.all, "Message name.");
      Assert(M1.Count = M2.Count, "Message (code) Count.");
      for I in M1.Code'FIRST .. Interfaces.Unsigned_32(M1.Count) loop
         Assert(M1.Code(I) = M2.Code(I), "Message code (I).");
      end loop;
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_A7) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A7: load and run a binary.");
   end Name;

   procedure Run_Test (T : in out Test_A7) is
      pragma Unreferenced (T);
      Builder2 : aliased kv.avm.Assemblers.Assembler_Type;
      package SIO renames Ada.Streams.Stream_IO;
      F : SIO.File_Type;
      S : SIO.Stream_Access;
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Instance_Factory : aliased kv.avm.Instances.Instance_Factory;
      Data : aliased kv.avm.Tuples.Tuple_Type;
      Instance_1 : kv.avm.Actor_References.Actor_Reference_Type;
      Registers : kv.avm.Memories.Register_Array_Type;
      Status : kv.avm.Control.Status_Type;
      Message : kv.avm.Messages.Message_Type;
      use kv.avm.Assemblers;
   begin
      Put_Line("test A7");
      --kv.avm.log.verbose := True;
      kv.avm.actor_pool.Empty_Actor_Pool;
      kv.avm.Actors.Empty_Actor_Map;
      SIO.Open(F, SIO.In_File, "test_a6.volec");
      S := SIO.Stream(F);
      Builder2.Initialize;
      Builder2.Read_Word_Code(S);
      SIO.Close(F);

      Builder2.Transfer_Actors_To_Store;

      VM.Initialize(CPU'UNCHECKED_ACCESS, Instance_Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);

      VM.New_Actor("Test_A1", Instance_1);

      Registers.Initialize((U(0), U(1)));
      Data.Initialize;
      Data.Fold(Registers);

      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Instance_1,
          Message_Name => "CONSTRUCTOR",
          Data         => Data,
          Future       => kv.avm.Control.NO_FUTURE);
      VM.Post_Message
         (Message => Message,
          Status  => Status);
      Message.Finalize;

      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Instance_1,
          Message_Name => "Go",
          Data         => Data,
          Future       => kv.avm.Control.NO_FUTURE);
      VM.Post_Message
         (Message => Message,
          Status  => Status);

      while not VM.Done loop
         VM.Step;
         Assert(VM.Get_Steps < 200, "Too many steps have been taken.");
      end loop;
      Assert(VM.Get_Steps = 60, "Wrong step count."&Natural'IMAGE(VM.Get_Steps));
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_A8) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A8: Fibonacci debug.");
   end Name;

   procedure Run_Test (T : in out Test_A8) is
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Instance_Factory : aliased kv.avm.Instances.Instance_Factory;
      Registers : kv.avm.Memories.Register_Array_Type;
      use kv.avm.Registers;
   begin
      Put_Line("test A8");
      Builder.Initialize;
      Builder.Parse_Line(".actor FibonacciHelper");
      Builder.Parse_Line(" C0 uint 0");
      Builder.Parse_Line(" F1 uint 1");
      Builder.Parse_Line(" F2 uint 2");
      Builder.Parse_Line(" F3 uint 3");
      Builder.Parse_Line(" F4 uint 4");
      Builder.Parse_Line(" F5 mdef compute");
      Builder.Parse_Line(" F6 : Tuple_Map := [L0, L1, L2]");
      Builder.Parse_Line("..message CONSTRUCTOR # I0 = depth");
      Builder.Parse_Line("  SET A0 := I0 # A0 = Self.Depth");
      Builder.Parse_Line("  REPLY C0");
      Builder.Parse_Line("  STOP_FRAME");
      Builder.Parse_Line("..message compute # I0 = level, I1 = fibk, I2 = fibk1");
      Builder.Parse_Line("   COMPUTE L9 := A0 != I0");
      Builder.Parse_Line("   BRANCH_REL L9 2 # if Self.Depth != level then skip next two instructions");
      Builder.Parse_Line("   SET L0 I2");
      Builder.Parse_Line("   REPLY L0");
      Builder.Parse_Line("   COMPUTE L0 := I0 + F1 # L0 := level + 1");
      Builder.Parse_Line("   COMPUTE L1 := I1 + I2 # L1 := fibk + fibk1");
      Builder.Parse_Line("   SET L2 I1 # L2 := fibk");
      Builder.Parse_Line("   FOLD L4 := F6 # L4 := fold(L0, L1, L2)");
      Builder.Parse_Line("   SELF_TAIL_CALL L4 => F5");
      Builder.Parse_Line("   STOP_FRAME");
      Builder.Parse_Line(".actor Fibonacci");
      Builder.Parse_Line(" F0 uint 0");
      Builder.Parse_Line(" F1 uint 1");
      Builder.Parse_Line(" F2 uint 2");
      Builder.Parse_Line(" F3 uint 3");
      Builder.Parse_Line(" F4 uint 4");
      Builder.Parse_Line(" F5 mdef compute");
      Builder.Parse_Line(" F6 adef FibonacciHelper");
      Builder.Parse_Line(" F7 : Tuple_Map := [L1, L2, L3]");
      Builder.Parse_Line("..message CONSTRUCTOR");
      Builder.Parse_Line("  REPLY F0");
      Builder.Parse_Line("  STOP_FRAME");
      Builder.Parse_Line("..message Compute");
      Builder.Parse_Line("  EMIT S0");
      Builder.Parse_Line("  NEW_ACTOR L0 S0 F6");
      Builder.Parse_Line("  SET L1 F1");
      Builder.Parse_Line("  SET L2 F1");
      Builder.Parse_Line("  SET L3 F0");
      Builder.Parse_Line("  FOLD L4 := F7");
      Builder.Parse_Line("  ACTOR_CALL L5 := L4 => L0 . F5");
      Builder.Parse_Line("  PEEK L6 L5 F0");
      Builder.Parse_Line("  TRAP L7 F1 L6");
      Builder.Parse_Line("  REPLY L6");
      Builder.Parse_Line("  STOP_FRAME");
      Builder.Transfer_Actors_To_Store;
      VM.Initialize(CPU'UNCHECKED_ACCESS, Instance_Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);

      Registers.Initialize((0 => U(6)));
      VM.Start_With("Fibonacci", "Compute", Registers);

      while not VM.Done loop
         VM.Step;
         Assert(VM.Get_Steps < 200, "Too many steps have been taken.");
      end loop;
      Assert(VM.Get_Steps = 56, "Wrong step count."&Natural'IMAGE(VM.Get_Steps));
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_A9) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A9: Test predicate parsing.");
   end Name;

   procedure Run_Test (T : in out Test_A9) is
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      Built_Actor : kv.avm.Actors.Actor_Access;
      Test_Method : kv.avm.Methods.Method_Access;
      Offset : constant kv.avm.References.Offset_Type := 13;
      use kv.avm.Registers;
      use kv.avm.Actors;
      use kv.avm.Methods;
   begin
      Put_Line("test A9");
      Builder.Initialize;
      Builder.Parse_Line(".actor PredicateTest");
      Builder.Parse_Line("..message CONSTRUCTOR");
      Builder.Parse_Line("  STOP_FRAME");
      Builder.Parse_Line("..message Message_1");
      Builder.Parse_Line("...predicate 13");
      Builder.Parse_Line("  STOP_FRAME");
      Builder.Transfer_Actors_To_Store;
      Built_Actor := kv.avm.Actors.Get_Actor_By_Name("PredicateTest");
      Assert(Built_Actor /= null, "Failed to build PredicateTest actor");
      Test_Method := Built_Actor.Get_Method("Message_1");
      Assert(Test_Method /= null, "Failed to build Message_1");
      Assert(Test_Method.Has_Predicate, "Message_1 does not have a predicate");
      Assert(Test_Method.Get_Predicate = Offset, "Predicate has incorrect offset");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A10) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A10: Test more streaming.");
   end Name;

   procedure Run_Test (T : in out Test_A10) is
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      Builder2 : aliased kv.avm.Assemblers.Assembler_Type;
      package SIO renames Ada.Streams.Stream_IO;
      F : SIO.File_Type;
      S : SIO.Stream_Access;
      Built_Actor : kv.avm.Actors.Actor_Access;
      Test_Method : kv.avm.Methods.Method_Access;
      Offset : constant kv.avm.References.Offset_Type := 13;
      use kv.avm.Registers;
      use kv.avm.Actors;
      use kv.avm.Methods;
   begin
      kv.avm.log.verbose := False;
      Put_Line("test A10");
      Builder.Initialize;
      Builder.Parse_Line(".actor PredicateTest");
      Builder.Parse_Line(" C0 : bit_or_boolean := False");
      Builder.Parse_Line("..message CONSTRUCTOR");
      Builder.Parse_Line("  SET A0 := C0");
      Builder.Parse_Line("  STOP_FRAME");
      Builder.Parse_Line("..message Message_1");
      Builder.Parse_Line("...predicate 13");
      Builder.Parse_Line("  STOP_FRAME");

      SIO.Create(F, SIO.Out_File, "test_a10.volec");
      S := SIO.Stream(F);
      Builder.Write_Word_Code(S);
      SIO.Close(F);

      kv.avm.Actors.Empty_Actor_Map;

      Put_Line("test A10: reading back in the assembled word code.");
      SIO.Open(F, SIO.In_File, "test_a10.volec");
      S := SIO.Stream(F);
      Builder2.Initialize;
      Builder2.Read_Word_Code(S);
      SIO.Close(F);

      Builder2.Transfer_Actors_To_Store;
      Built_Actor := kv.avm.Actors.Get_Actor_By_Name("PredicateTest");
      Assert(Built_Actor /= null, "Failed to build PredicateTest actor");
      Test_Method := Built_Actor.Get_Method("Message_1");
      Assert(Test_Method /= null, "Failed to build Message_1");
      Assert(Test_Method.Has_Predicate, "Message_1 does not have a predicate");
      Assert(Test_Method.Get_Predicate = Offset, "Predicate has incorrect offset");
   end Run_Test;



   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   package Test_Cap is
      type Recorder_Cap_Type is new kv.avm.Capabilities.Capability_Interface with
         record
            Super_Constructor_Called : Boolean := False;
            Super_Message_2_Called : Boolean := False;
            Super_Run_It_Called : Boolean := False;
            SubClass_Run_It_Called : Boolean := False;
         end record;
      overriding procedure Execute
         (Self    : in out Recorder_Cap_Type;
          Machine : in out kv.avm.Control.Control_Interface'CLASS;
          Input   : in     kv.avm.Registers.Register_Type;
          Output  :    out kv.avm.Registers.Register_Type;
          Status  :    out kv.avm.Control.Status_Type);
      not overriding
      procedure Initialize(Self : in out Recorder_Cap_Type);
      Recorder : aliased Recorder_Cap_Type;
      Pool : kv.avm.Capabilities.Capabilities_Type;

      procedure Prep_Once;
   end Test_Cap;

   package body Test_Cap is
      --------------------------------------------------------------------------
      procedure Execute
         (Self    : in out Recorder_Cap_Type;
          Machine : in out kv.avm.Control.Control_Interface'CLASS;
          Input   : in     kv.avm.Registers.Register_Type;
          Output  :    out kv.avm.Registers.Register_Type;
          Status  :    out kv.avm.Control.Status_Type) is
      begin
         if Input.Unsigned_Value = 1 then
            Self.Super_Constructor_Called := True;
         elsif Input.Unsigned_Value = 2 then
            Self.Super_Message_2_Called := True;
         elsif Input.Unsigned_Value = 3 then
            Self.Super_Run_It_Called := True;
         elsif Input.Unsigned_Value = 4 then
            Self.SubClass_Run_It_Called := True;
         end if;
         Output := kv.avm.Registers.Make_U(1397);
         Status := kv.avm.Control.Active;
      end Execute;

      --------------------------------------------------------------------------
      procedure Initialize(Self : in out Recorder_Cap_Type) is
      begin
         Self.Super_Constructor_Called := False;
         Self.Super_Message_2_Called := False;
         Self.Super_Run_It_Called := False;
         Self.SubClass_Run_It_Called := False;
      end Initialize;

      --------------------------------------------------------------------------
      Prep_Me : boolean := True;
      procedure Prep_Once is
      begin
         if Prep_Me then
            Test_Cap.Pool.Add("Recorder", Test_Cap.Recorder'UNCHECKED_ACCESS);
            Prep_Me := False;
         end if;
      end Prep_Once;
  end Test_Cap;


   -----------------------------------------------------------------------------
   procedure Set_Up (T : in out Superclass_Test_Case) is
   begin
      kv.avm.Log.Verbose := False;
      T.Builder.Initialize;
      Test_Cap.Recorder.Initialize;
   end Set_Up;

   -----------------------------------------------------------------------------
   procedure Tear_Down (T : in out Superclass_Test_Case) is
   begin
      kv.avm.Actors.Empty_Actor_Map;
   end Tear_Down;

   -----------------------------------------------------------------------------
   procedure Load (T : in out Superclass_Test_Case; Volea : in String) is
   begin
      T.Builder.Parse_Input_File(Volea);
      T.Builder.Transfer_Actors_To_Store;
   end Load;

   -----------------------------------------------------------------------------
   procedure Prep_VM
      (T             : in out Superclass_Test_Case;
       VM            : access kv.avm.Machines.Machine_Type;
       Start_Actor   : in     String;
       Start_Message : in     String) is
      use kv.avm.Registers;
   begin
      -- VM is passed in because when it is part of T something goes wrong and the
      -- following line raises a CONSTRAINT_ERROR because the router isn't intitialized.
      VM.Initialize(T.CPU'UNCHECKED_ACCESS, T.Instance_Factory'UNCHECKED_ACCESS);
      T.CPU.Initialize(VM.all'UNCHECKED_ACCESS);

      T.Registers.Initialize((Make_U(0), Make_U(1)));
      VM.Start_With(Start_Actor, Start_Message, T.Registers);
   end Prep_VM;

   -----------------------------------------------------------------------------
   procedure Run_VM (T : in out Superclass_Test_Case; VM : access kv.avm.Machines.Machine_Type; Steps : in Positive) is
   begin
      while not VM.Done loop
         VM.Step;
         Assert(VM.Get_Steps < Steps, "Too many steps have been taken.");
      end loop;
   end Run_VM;

   -----------------------------------------------------------------------------
   function Name (T : Test_A11) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A11: ..subclassof directive.");
   end Name;

   procedure Run_Test (T : in out Test_A11) is
      Subclass : kv.avm.Actors.Actor_Access;
      Superclass : kv.avm.Actors.Actor_Access;
      use type kv.avm.Actors.Actor_Access;
   begin
      kv.avm.log.verbose := False;
      Put_Line("test A11");
      T.Load("test_a11.volea");
      Subclass := kv.avm.Actors.Get_Actor_By_Name("SubClass");
      Assert(Subclass /= null, "SubClass not loaded.");
      Assert(Subclass.Get_Name = "SubClass", "SubClass not returned by Get_Actor_By_Name(SubClass), <" & Subclass.Get_Name & "> returned instead.");
      Superclass := Subclass.Get_Parent;
      Assert(Superclass /= null, "SubClass returned null for its parent.");
      Assert(Superclass.Get_Name = "SuperClass", "SuperClass parent name unexpedcted, got <" & Superclass.Get_Name & ">.");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A12) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A12: dispatching to super class.");
   end Name;

   procedure Run_Test (T : in out Test_A12) is
      Local_VM  : aliased kv.avm.Machines.Machine_Type;
   begin
      kv.avm.log.verbose := False;
      Put_Line("test A12");
      T.Load("test_a11.volea");
      Test_Cap.Prep_Once;
      T.Prep_VM(Local_VM'ACCESS, "Test_A11", "Go");
      Local_VM.Set_Capabilities(Test_Cap.Pool);
      T.Run_VM(Local_VM'ACCESS, 200);

      Assert(Test_Cap.Recorder.Super_Constructor_Called, "Super CONSTRUCTOR not called");
      Assert(Test_Cap.Recorder.Super_Message_2_Called, "Super Message_2 not called");
      Assert(Local_VM.Get_Steps = 21, "Wrong step count."&Natural'IMAGE(Local_VM.Get_Steps));
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A13) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A13: dispatching to subclass.");
   end Name;

   procedure Run_Test (T : in out Test_A13) is
      Local_VM  : aliased kv.avm.Machines.Machine_Type;
   begin
      kv.avm.log.verbose := False;
      Put_Line("test A13");
      T.Load("test_a13.volea");
      Test_Cap.Prep_Once;
      T.Prep_VM(Local_VM'ACCESS, "Test_A13", "Go");
      Local_VM.Set_Capabilities(Test_Cap.Pool);
      T.Run_VM(Local_VM'ACCESS, 200);

      Assert(Test_Cap.Recorder.Super_Constructor_Called, "Super CONSTRUCTOR not called");
      Assert(Test_Cap.Recorder.Super_Message_2_Called, "Super Message_2 not called");
      Assert(Test_Cap.Recorder.SubClass_Run_It_Called, "SubClass Run_It message not called");
      Assert(not Test_Cap.Recorder.Super_Run_It_Called, "Super Run_It message WAS called");
      Assert(Local_VM.Get_Steps = 21, "Wrong step count."&Natural'IMAGE(Local_VM.Get_Steps));
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A14) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A14: Tuple Definition.");
   end Name;

   procedure Run_Test (T : in out Test_A14) is
      Registers : kv.avm.Memories.Register_Array_Type;
      Registers2 : kv.avm.Memories.Register_Array_Type;
      Registers3 : kv.avm.Memories.Register_Array_Type;
      Tuple : kv.avm.Tuples.Tuple_Type;
      Tuple2 : kv.avm.Tuples.Tuple_Type;
      Tuple3 : kv.avm.Tuples.Tuple_Type;
      Def : kv.avm.Tuples.Definition_Type;
      Def2 : kv.avm.Tuples.Definition_Type;
      Def3 : kv.avm.Tuples.Definition_Type;
      Def4 : kv.avm.Tuples.Definition_Type;
      use kv.avm.Registers;
      use kv.avm.Tuples;
   begin
      Registers.Initialize((Make_U(0), Make_S(1), Make_String("Hello")));
      Tuple.Fold(Registers);
      Def.Make(Tuple);
      Assert(Def.Length = 3, "Tuple definition length wasn't 3, it was " & Natural'IMAGE(Def.Length) & ".");
      Assert(Def.To_String = "UIS", "Tuple definition string wasn't 'UIS', it was '" & Def.To_String & "'.");
      Def2 := Def;
      Assert(Def2.Length = 3, "Tuple definition 2 length wasn't 3, it was " & Natural'IMAGE(Def2.Length) & ".");
      Assert(Def2.To_String = "UIS", "Tuple definition 2 string wasn't 'UIS', it was '" & Def2.To_String & "'.");
      Assert(Def = Def2, "Def /= Def2");

      Registers2.Initialize((Make_U(8), Make_S(-33), Make_String("World")));
      Tuple2.Fold(Registers2);
      Def3.Make(Tuple2);
      Assert(Def3.Length = 3, "Tuple definition 3 length wasn't 3, it was " & Natural'IMAGE(Def3.Length) & ".");
      Assert(Def3.To_String = "UIS", "Tuple definition 3 string wasn't 'UIS', it was '" & Def3.To_String & "'.");
      Assert(Def = Def3, "Def /= Def3");

      Registers3.Initialize((Make_S(-8), Make_String("Yo"), Make_Tuple(Tuple), Make_U(100)));
      Tuple3.Fold(Registers3);
      Def4.Make(Tuple3);
      Assert(Def4.Length = 4, "Tuple definition 4 length wasn't 4, it was " & Natural'IMAGE(Def4.Length) & ".");
      Assert(Def4.To_String = "ISTU", "Tuple definition 4 string wasn't 'ISTU', it was '" & Def4.To_String & "'.");
      Assert(Def /= Def4, "Def = Def4");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A15) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A15: Empty Tuple Definition.");
   end Name;

   procedure Run_Test (T : in out Test_A15) is
      Tuple1 : kv.avm.Tuples.Tuple_Type;
      Tuple2 : kv.avm.Tuples.Tuple_Type;
      Def1 : kv.avm.Tuples.Definition_Type;
      Def2 : kv.avm.Tuples.Definition_Type;
      use kv.avm.Registers;
      use kv.avm.Tuples;
   begin
      Tuple1.Fold_Empty;
      Def1.Make(Tuple1);
      Tuple2.Fold_Empty;
      Def2.Make(Tuple2);

      Assert(Def1 = Def2, "Def1 /= Def2");

      Assert(Def1.Length = 0, "Tuple definition 1 length wasn't 0, it was " & Natural'IMAGE(Def1.Length) & ".");
      Assert(Def1.To_String = "", "Tuple definition 1 string wasn't '', it was '" & Def1.To_String & "'.");

      Assert(Def2.Length = 0, "Tuple definition 2 length wasn't 0, it was " & Natural'IMAGE(Def2.Length) & ".");
      Assert(Def2.To_String = "", "Tuple definition 2 string wasn't '', it was '" & Def2.To_String & "'.");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A16) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A16: Data Kind Signatures.");
   end Name;

   procedure Run_Test (T : in out Test_A16) is
      All_Kinds : constant String := "UITSEFfBMmRAd";
      use kv.avm.Registers;
   begin
      for Kind in Data_Kind loop
         Assert(Kind = Format(Signature(Kind)), "");
      end loop;
      Assert(All_Kinds = Signature_To_String(String_To_Signature(All_Kinds)), "");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A17) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A17: Saved and re-loaded, dispatching to superclass.");
   end Name;

   procedure Run_Test (T : in out Test_A17) is
      Local_VM  : aliased kv.avm.Machines.Machine_Type;
      Builder2 : aliased kv.avm.Assemblers.Assembler_Type;
      package SIO renames Ada.Streams.Stream_IO;
      F : SIO.File_Type;
      S : SIO.Stream_Access;
   begin
      kv.avm.log.verbose := True;
      Put_Line("test A17");
      T.Load("test_a11.volea");

      SIO.Create(F, SIO.Out_File, "test_a11.volec");
      S := SIO.Stream(F);
      T.Builder.Write_Word_Code(S);
      SIO.Close(F);

      kv.avm.Actors.Empty_Actor_Map;

      Put_Line("test A17: reading back in the assembled word code.");
      SIO.Open(F, SIO.In_File, "test_a11.volec");
      S := SIO.Stream(F);
      Builder2.Initialize;
      Builder2.Read_Word_Code(S);
      SIO.Close(F);

      Builder2.Transfer_Actors_To_Store;

      Test_Cap.Prep_Once;
      T.Prep_VM(Local_VM'ACCESS, "Test_A11", "Go");
      Local_VM.Set_Capabilities(Test_Cap.Pool);
      T.Run_VM(Local_VM'ACCESS, 200);

      Assert(Test_Cap.Recorder.Super_Constructor_Called, "Super CONSTRUCTOR not called");
      Assert(Test_Cap.Recorder.Super_Message_2_Called, "Super Message_2 not called");
      Assert(Local_VM.Get_Steps = 21, "Wrong step count."&Natural'IMAGE(Local_VM.Get_Steps));
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A18) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A18: XXX.");
   end Name;

   procedure Run_Test (T : in out Test_A18) is
   begin
      null;
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A19) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A19: XXX.");
   end Name;

   procedure Run_Test (T : in out Test_A19) is
   begin
      null;
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_A20) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test A20: XXX.");
   end Name;

   procedure Run_Test (T : in out Test_A20) is
   begin
      null;
   end Run_Test;


end kv.avm.Asm_Tests;
