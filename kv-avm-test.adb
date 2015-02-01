with Interfaces; use Interfaces;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with AUnit.Assertions; use AUnit.Assertions;

with String_Ops; use String_Ops;

with kv.avm.references; use kv.avm.references;
with kv.avm.Instructions;
with kv.avm.Registers;
with kv.avm.Memories; use kv.avm.Memories;
with kv.avm.Processors;
with kv.avm.Instances;
with kv.avm.Actors;
with kv.avm.Control;
with kv.avm.Messages;
with kv.avm.Tuples;
with kv.avm.Actor_References;
with kv.avm.actor_pool;
with kv.avm.Machines;
with kv.avm.Frames;
with kv.avm.Assemblers;
with kv.avm.Log;
with kv.avm.Methods; use kv.avm.Methods;
with kv.avm.Executables; use kv.avm.Executables;
with kv.avm.Executable_Lists;
with kv.avm.Test.Runners; -- test stub
with kv.avm.Actor_References.Sets;

package body kv.avm.Test is

   use kv.avm.Instructions;
   use kv.avm.Registers;
   use kv.avm.Processors;
   use kv.avm.Instances;
   use kv.avm.Actors;
   use kv.avm.Control;
   use kv.avm.Messages;
   use kv.avm.Tuples;
   use kv.avm.Actor_References;


   procedure Free is new Ada.Unchecked_Deallocation(Code_Type, Code_Access);
   procedure Free is new Ada.Unchecked_Deallocation(Actor_Type, Actor_Access);
   procedure Free is new Ada.Unchecked_Deallocation(Instance_Type, Instance_Access);
   procedure Free is new Ada.Unchecked_Deallocation(String, String_Ops.String_Pointer_Type);

   function Convert is new Ada.Unchecked_Conversion(Interfaces.Integer_32, Interfaces.Unsigned_32);

   --
   --
   --
   package Fake_Vm is
      type Post_Command_Behavior_Type is (Normal, Deferred);
      type Fake_Vm_Type is new Control_Interface with
         record
            Posted       : Interfaces.Unsigned_32 := 0;
            Source       : kv.avm.Actor_References.Actor_Reference_Type;
            Reply_To     : kv.avm.Actor_References.Actor_Reference_Type;
            Destination  : kv.avm.Actor_References.Actor_Reference_Type;
            Message_Name : String_Ops.String_Pointer_Type;
            Data         : kv.avm.Tuples.Tuple_Type;
            Future       : Interfaces.Unsigned_32;
            Post_Command_Behavior : Post_Command_Behavior_Type := Normal;
         end record;
      overriding procedure New_Actor
         (Self     : in out Fake_Vm_Type;
          Name     : in     String;
          Instance :    out kv.avm.Actor_References.Actor_Reference_Type);
      overriding procedure Post_Message
         (Self    : in out Fake_Vm_Type;
          Message : in     kv.avm.Messages.Message_Type;
          Status  :    out Status_Type);
      overriding procedure Post_Response
         (Self         : in out Fake_Vm_Type;
          Reply_To     : in     kv.avm.Actor_References.Actor_Reference_Type;
          Answer       : in     kv.avm.Tuples.Tuple_Type;
          Future       : in     Interfaces.Unsigned_32);
      overriding procedure Generate_Next_Future
         (Self   : in out Fake_Vm_Type;
          Future :    out Interfaces.Unsigned_32);
      overriding procedure Trap_To_The_Machine
         (Self   : in out Fake_Vm_Type;
          Trap   : in     String;
          Data   : in     kv.avm.Registers.Register_Type;
          Answer :    out kv.avm.Registers.Register_Type;
          Status :    out kv.avm.Control.Status_Type);
      overriding
      procedure Activate_Instance
         (Self     : in out Fake_Vm_Type;
          Instance : in     kv.avm.Actor_References.Actor_Reference_Type);
   end Fake_Vm;

   Fake_Actor_Name : aliased constant String := "FakeActor";
   Fake_Instance : aliased kv.avm.Instances.Instance_Type;
   Fake_Ref : aliased kv.avm.Actor_References.Actor_Reference_Type;
   Fake_Invoker : aliased kv.avm.Instances.Instance_Type;
   Fake_Invoker_Ref : aliased kv.avm.Actor_References.Actor_Reference_Type;

   package body Fake_Vm is
      Available_Future : Interfaces.Unsigned_32 := 3;
      procedure New_Actor
         (Self     : in out Fake_Vm_Type;
          Name     : in     String;
          Instance :    out kv.avm.Actor_References.Actor_Reference_Type) is
      begin
         kv.avm.Log.Put_Line("Fake_Vm_Type.New_Actor called with name = '"&Name&"'");
         kv.avm.actor_pool.Add(Fake_Instance'ACCESS, Fake_Ref);
         Instance := Fake_Ref;
      end New_Actor;
      procedure Post_Message
         (Self    : in out Fake_Vm_Type;
          Message : in     kv.avm.Messages.Message_Type;
          Status  :    out Status_Type) is
      begin
         kv.avm.Log.Put_Line("Fake_Vm_Type.Post_Message called with name = '"&Message.Get_Name&"'.");
         if Self.Post_Command_Behavior = Normal then
            Status := kv.avm.Control.Active;
            Self.Posted := 27;
            Self.Source := Message.Get_Source;
            Self.Reply_To := Message.Get_Reply_To;
            Self.Destination := Message.Get_Destination;
            if Self.Message_Name /= null then
               Free(Self.Message_Name);
            end if;
            Self.Message_Name := new String'(Message.Get_Name);
            Self.Data := Message.Get_Data;
            Self.Future := Message.Get_Future;
         else
            Status := kv.avm.Control.Deferred;
         end if;
      end Post_Message;
      procedure Post_Response
         (Self         : in out Fake_Vm_Type;
          Reply_To     : in     kv.avm.Actor_References.Actor_Reference_Type;
          Answer       : in     kv.avm.Tuples.Tuple_Type;
          Future       : in     Interfaces.Unsigned_32) is
      begin
         Self.Posted := 73;
      end Post_Response;
      procedure Generate_Next_Future
         (Self   : in out Fake_Vm_Type;
          Future :    out Interfaces.Unsigned_32) is
      begin
         Future := Available_Future;
         Available_Future := Available_Future + 1;
      end Generate_Next_Future;
      procedure Trap_To_The_Machine
         (Self   : in out Fake_Vm_Type;
          Trap   : in     String;
          Data   : in     kv.avm.Registers.Register_Type;
          Answer :    out kv.avm.Registers.Register_Type;
          Status :    out kv.avm.Control.Status_Type) is
      begin
         kv.avm.Log.Put_Line("Fake_Vm_Type.Trap_To_The_Machine called with name = '"&Trap&"'");
         Answer := Make_U(0);
         Status := kv.avm.Control.Active;
      end Trap_To_The_Machine;
      procedure Activate_Instance
         (Self     : in out Fake_Vm_Type;
          Instance : in     kv.avm.Actor_References.Actor_Reference_Type) is
      begin
         null;
      end Activate_Instance;
   end Fake_Vm;

   Fake_Machine : aliased Fake_Vm.Fake_Vm_Type;
   --
   --
   --

   Test_Message_Name : aliased constant String := "DefaultTestMessage";
   Msg_Name : aliased constant String := "FooBar";
   Actor_Name : aliased constant String := "TestActor";


   procedure Set_Up (T : in out Instruction_Test_Case) is
      Tup : kv.avm.Tuples.Tuple_Type;
      Ref : Actor_Reference_Type;
      Mem_Local : kv.avm.Memories.Register_Array_Type;
      Mem_Fixed : kv.avm.Memories.Register_Array_Type;
      Mem_Attr : kv.avm.Memories.Register_Array_Type;
   begin
      kv.avm.Log.Verbose := False;
      Put_Line("==========================================================");
      T.i := new kv.avm.Instances.Instance_Type;
      T.c := new Code_Type(0..31);
      T.f := new Code_Type(0..31);
      Mem_Local.Allocate(32);
      Mem_Fixed.Allocate(32);
      Mem_Attr.Allocate(32);
      T.m.Set(Local, Mem_Local);
      T.m.Set(fixed, Mem_Fixed);
      T.m.Set(Attribute, Mem_Attr);
      T.a := new Actor_Type;
      T.p.Initialize(Fake_Machine'ACCESS);
      T.a.Initialize(Actor_Name'ACCESS, T.c, Fixed_Registers => Mem_Fixed);
      T.a.Add_Method(New_Method(Test_Message_Name, T.c));
      T.a.Add_Method(New_Method(Msg_Name, T.f));
      kv.avm.Actor_Pool.Add(kv.avm.Executables.Executable_Access(T.i), Ref);
      T.i.Initialize(T.a, T.m, Ref);
      T.x.Initialize(Ref, Ref, Ref, Test_Message_Name, Tup, 0);
      T.i.Process_Message(T.x);
   end Set_Up;

   procedure Tear_Down (T : in out Instruction_Test_Case) is
   begin
      kv.avm.Log.Verbose := False;
      --Free(T.x);
      Free(T.a);
      T.m.Deallocate;
      Free(T.f);
      Free(T.c);
      Free(T.i);
      Put_Line("==========================================================");
   end Tear_Down;


   -----------------------------------------------------------------------------
   procedure Mem_Set
      (T   : in out Instruction_Test_Case;
       Ref : in reference_type;
       Val : in kv.avm.Registers.Register_Type) is
   begin
      T.m.Write(Ref, Val);
   end Mem_Set;


   -----------------------------------------------------------------------------
   function Mem_Get
      (T   : in     Instruction_Test_Case;
       Ref : in     reference_type) return kv.avm.Registers.Register_Type is
   begin
      return T.m.Read(Ref);
   end Mem_Get;

   -----------------------------------------------------------------------------
   procedure Step(T : in out Instruction_Test_Case) is
   begin
      T.i.Step(T.p'ACCESS, T.s);
   end Step;

   L0 : constant reference_type := (memory => local, index => 0);
   L1 : constant reference_type := (memory => local, index => 1);
   L2 : constant reference_type := (memory => local, index => 2);
   L3 : constant reference_type := (memory => local, index => 3);
   L4 : constant reference_type := (memory => local, index => 4);
   L5 : constant reference_type := (memory => local, index => 5);
   L6 : constant reference_type := (memory => local, index => 6);
   L7 : constant reference_type := (memory => local, index => 7);
   L8 : constant reference_type := (memory => local, index => 8);
   L9 : constant reference_type := (memory => local, index => 9);
   L10 : constant reference_type := (memory => local, index => 10);
   L11 : constant reference_type := (memory => local, index => 11);
   L12 : constant reference_type := (memory => local, index => 12);
   L13 : constant reference_type := (memory => local, index => 13);
   L14 : constant reference_type := (memory => local, index => 14);
   L15 : constant reference_type := (memory => local, index => 15);
   L16 : constant reference_type := (memory => local, index => 16);
   L17 : constant reference_type := (memory => local, index => 17);
   L18 : constant reference_type := (memory => local, index => 18);
   L19 : constant reference_type := (memory => local, index => 19);

   F0 : constant reference_type := (memory => fixed, index => 0);
   F1 : constant reference_type := (memory => fixed, index => 1);
   F2 : constant reference_type := (memory => fixed, index => 2);
   F3 : constant reference_type := (memory => fixed, index => 3);
   F4 : constant reference_type := (memory => fixed, index => 4);
   F5 : constant reference_type := (memory => fixed, index => 5);
   F6 : constant reference_type := (memory => fixed, index => 6);
   F7 : constant reference_type := (memory => fixed, index => 7);
   F8 : constant reference_type := (memory => fixed, index => 8);
   F9 : constant reference_type := (memory => fixed, index => 9);
   F10 : constant reference_type := (memory => fixed, index => 10);
   F11 : constant reference_type := (memory => fixed, index => 11);
   F12 : constant reference_type := (memory => fixed, index => 12);
   F13 : constant reference_type := (memory => fixed, index => 13);
   F14 : constant reference_type := (memory => fixed, index => 14);
   F15 : constant reference_type := (memory => fixed, index => 15);
   F16 : constant reference_type := (memory => fixed, index => 16);
   F17 : constant reference_type := (memory => fixed, index => 17);
   F18 : constant reference_type := (memory => fixed, index => 18);
   F19 : constant reference_type := (memory => fixed, index => 19);

   A0 : constant reference_type := (memory => Attribute, index => 0);
   A1 : constant reference_type := (memory => Attribute, index => 1);

   I0 : constant reference_type := (memory => Input, index => 0);
   I1 : constant reference_type := (memory => Input, index => 1);
   I2 : constant reference_type := (memory => Input, index => 2);


   -----------------------------------------------------------------------------
   function S(Value : Interfaces.Integer_64) return kv.avm.Registers.Register_Type is
   begin
      return (format => Signed_Integer, signed_value => Value);
   end S;

   -----------------------------------------------------------------------------
   function U(Value : Interfaces.Unsigned_64) return kv.avm.Registers.Register_Type is
   begin
      return (format => Unsigned_Integer, unsigned_value => Value);
   end U;

   -----------------------------------------------------------------------------
   function B(Value : Boolean) return kv.avm.Registers.Register_Type is
   begin
      return (format => Bit_Or_Boolean, bit => Value);
   end B;


   -----------------------------------------------------------------------------
   function Name (T : Test_1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 01: NO_OP.");
   end Name;

   procedure Run_Test (T : in out Test_1) is
   begin
      Put_Line("test 01");
      Assert(T.i.Program_Counter = 0, "Initial pc was not 0.");
      T.c(0) := (op_code => no_op);
      T.Step;
      Assert(T.i.Program_Counter = 1, "instruction did not advance pc to 1.");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_2) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 02: SET.");
   end Name;

   procedure Run_Test (T : in out Test_2) is
      x : reference_type := (memory => local, index => 0);
      y : reference_type := (memory => local, index => 1);
   begin
      Put_Line("test 02");
      Assert(T.Mem_Get(L0).format = Unset, "Local(0) was not 'Unset'.");
      T.c(0) := (op_code => SET, lhs => x, rhs => y);
      T.Mem_Set(L1, (format => Signed_Integer, signed_value => -13));
      T.Step;
      Assert(T.Mem_Get(L0).format = Signed_Integer, "Local(0) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L0).signed_value = -13, "Local(0) was not -13.");
      Assert(T.i.Program_Counter = 1, "instruction did not advance pc to 1.");

      T.c(1) := (op_code => set, lhs => (Attribute, 0), rhs => y);
      T.Step;
      Assert(T.Mem_Get(A0).format = Signed_Integer, "Instance(0) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(A0).signed_value = -13, "Instance(0) was not -13.");
      Assert(T.i.Program_Counter = 2, "instruction did not advance pc to 2.");

      begin
         T.c(2) := (op_code => set, lhs => (Attribute, 0), rhs => (memory => local, index => 2));
         T.Step;
         Assert(False, "Set of an invalid rhs did not raise an exception!");
      exception
         when others =>
            null;
      end;

      begin
         T.c(3) := (op_code => set, lhs => (Fixed, 0), rhs => y);
         T.Step;
         Assert(False, "Set of an invalid lhs did not raise an exception!");
      exception
         when others =>
            null;
      end;
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_3) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 03: JUMP.");
   end Name;

   procedure Run_Test (T : in out Test_3) is
      destination : reference_type := (memory => local, index => 0);
      DESTINATION_PC : constant := 27;
      use kv.avm.Assemblers;
   begin
      Put_Line("test 03");
      --T.c(0) := (op_code => jump, value => destination);
      T.c(0) := Decode_Op_Code("JUMP L0");
      T.Mem_Set(L0, U(DESTINATION_PC));
      T.Step;
      Assert(T.i.Program_Counter = DESTINATION_PC, "jump did not advance pc to L0.");
      T.c(27) := Decode_Op_Code("JUMP_ABS 13");
      T.Step;
      Assert(T.i.Program_Counter = 13, "JUMP_ABS did not advance pc to 13.");
      T.c(13) := Decode_Op_Code("JUMP_REL -1");
      T.Step;
      Assert(T.i.Program_Counter = 13, "JUMP_REL did not back 1.");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_4) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 04: ADD, SUBTRACT, MULTIPLY, DIVIDE.");
   end Name;

   procedure Run_Test (T : in out Test_4) is
      a : reference_type := (memory => local, index => 0);
      x : reference_type := (memory => local, index => 1);
      y : reference_type := (memory => local, index => 2);
      s : reference_type := (memory => local, index => 3);
      m : reference_type := (memory => local, index => 4);
      d : reference_type := (memory => local, index => 5);
   begin
      Put_Line("test 04");
      T.c(0) := (op_code => COMPUTE, action => add, result => a, left => x, right => y);
      T.c(1) := (op_code => COMPUTE, action => sub, result => s, left => y, right => x);
      T.c(2) := (op_code => COMPUTE, action => mul, result => m, left => x, right => y);
      T.c(3) := (op_code => COMPUTE, action => div, result => d, left => y, right => x);
      T.Mem_Set(L1, (format => Signed_Integer, signed_value => 3));
      T.Mem_Set(L2, (format => Signed_Integer, signed_value => 5));

      T.Step;
      Assert(T.Mem_Get(L0).format = Signed_Integer, "Local(0) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L0).signed_value = 8, "Local(0) was not 3+5=8.");
      Assert(T.i.Program_Counter = 1, "instruction did not advance pc to 1.");

      T.Step;
      Assert(T.Mem_Get(L3).format = Signed_Integer, "Local(3) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L3).signed_value = 2, "Local(3) was not 5-3=2.");
      Assert(T.i.Program_Counter = 2, "instruction did not advance pc to 2.");

      T.Step;
      Assert(T.Mem_Get(L4).format = Signed_Integer, "Local(4) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L4).signed_value = 15, "Local(4) was not 3*5=15.");
      Assert(T.i.Program_Counter = 3, "instruction did not advance pc to 3.");

      T.Step;
      Assert(T.Mem_Get(L5).format = Signed_Integer, "Local(5) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L5).signed_value = 1, "Local(5) was not 5/3=1.");
      Assert(T.i.Program_Counter = 4, "instruction did not advance pc to 4.");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_5) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 05: BRANCH.");
   end Name;

   procedure Run_Test (T : in out Test_5) is
      condition_false : reference_type := (memory => local, index => 0);
      condition_true : reference_type := (memory => local, index => 1);
      use kv.avm.Assemblers;
   begin
      Put_Line("test 05");
      T.Mem_Set(L0, (format => Bit_Or_Boolean, bit => False));
      T.Mem_Set(L1, (format => Bit_Or_Boolean, bit => True));
      T.c(0) := (op_code => branch_abs, condition => condition_true, target => 3);
      T.Step;
      Assert(T.i.Program_Counter = 3, "branch_abs did not jump pc to 3.");

      T.c(3) := (op_code => branch_rel, condition => condition_true, target => Convert(-3)); -- jump back 3 from next PC
      T.Step;
      Assert(T.i.Program_Counter = 1, "branch_rel did not jump pc to 1.");

      T.c(1) := (op_code => branch_abs, condition => condition_false, target => 4);
      T.Step;
      Assert(T.i.Program_Counter = 2, "branch_abs did not advance pc to 2.");

      T.c(2) := (op_code => branch_rel, condition => condition_false, target => 4);
      T.Step;
      Assert(T.i.Program_Counter = 3, "branch_rel did not advance pc to 3.");

      T.c(3) := (op_code => branch_rel, condition => condition_true, target => 1); -- skip the following single instruction
      T.Step;
      Assert(T.i.Program_Counter = 5, "branch_rel did not skip forward one instruction.");

      T.c(5) := Decode_Op_Code("BRANCH_NEQ L0 5");
      T.Step;
      Assert(T.i.Program_Counter = 11, "BRANCH_NEQ did not skip forward 5 instructions.");
   end Run_Test;



   -----------------------------------------------------------------------------
   function Name (T : Test_6) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 06: REPLY.");
   end Name;

   procedure Run_Test (T : in out Test_6) is
      answer : reference_type := (memory => local, index => 7);
      Save : kv.avm.Frames.Frame_Access := T.i.Get_Frame;
      my_tuple : kv.avm.Tuples.Tuple_Type;
   begin
      Put_Line("test 06");
      T.i.Get_Frame.Set_Invoker(Fake_Invoker_Ref);
      kv.avm.actor_pool.Add(Fake_Invoker'ACCESS, Fake_Invoker_Ref);
      T.Mem_Set(L7, (format => kv.avm.Registers.Tuple, Folded_Tuple => my_tuple));
      T.c(0) := (op_code => REPLY, value => answer);
      Put_Line("pre-step");
      T.Step;
      Put_Line("post-step");
      Assert(Fake_Machine.Posted = 73, "response message was not posted.");
      begin
         T.Step;
         Assert(False, "Error, an exception was not raised when a non-running frame was processed.");
      exception
         when Frame_Stopped_Error =>
            null; -- Pass
         when others =>
            null; -- Pass
      end;
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_6b) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 06b: STOP_FRAME.");
   end Name;

   procedure Run_Test (T : in out Test_6b) is
      answer : reference_type := (memory => local, index => 7);
      Save : kv.avm.Frames.Frame_Access := T.i.Get_Frame;
      my_tuple : kv.avm.Tuples.Tuple_Type;
   begin
      Put_Line("test 06b");
      T.c(0) := (op_code => STOP_FRAME);
      Put_Line("pre-step");
      T.Step;
      Put_Line("post-step");
      begin
         T.Step;
         Assert(False, "Error, an exception was not raised when a non-running frame was processed.");
      exception
         when Frame_Stopped_Error =>
            null; -- Pass
         when others =>
            null; -- Pass
      end;
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_7) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 07: NEW_ACTOR.");
   end Name;

   procedure Run_Test (T : in out Test_7) is
      target : reference_type := (memory => local, index => 0);
      actor_def : reference_type := (memory => fixed, index => 0);
      constuctor_args : reference_type := (memory => local, index => 1);
      my_tuple : kv.avm.Tuples.Tuple_Type;
      use kv.avm.Registers;
   begin
      Put_Line("test 07");
      T.c(0) := (op_code => new_actor, a => target, y => actor_def, x => constuctor_args);
      T.Mem_Set(L1, (format => kv.avm.Registers.Tuple, Folded_Tuple => my_tuple)); -- Empty constructor
      T.Mem_Set(F0, (format => Actor_Definition, Actor_Kind => +Fake_Actor_Name));
      T.Step;
      Assert(T.i.Program_Counter = 1, "instruction did not advance pc to 1.");
      Assert(T.Mem_Get(L0).format = kv.avm.Registers.Actor_Reference, "Target register of new_actor is not 'Actor_Reference'.");
      Assert(kv.avm.actor_pool.Resolve(T.Mem_Get(L0).Instance) = Fake_Instance'ACCESS, "Value of new_actor reference is not what was expected.");
      Assert(Fake_Machine.Posted = 27, "constructor message was not posted.");
      Assert(Fake_Machine.Message_Name.all = "CONSTRUCTOR", "message name error.");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_8) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 08: new tuple stuff.");
   end Name;

   procedure Run_Test (T : in out Test_8) is
      My_Data : aliased kv.avm.Memories.Register_Array_Type;
      My_Tuple : aliased kv.avm.Tuples.Tuple_Type;
      My_Data2 : aliased kv.avm.Memories.Register_Array_Type;
   begin
      Put_Line("test 08");
      My_Data.Initialize((U(99), U(98), U(97)));
      My_Tuple.Fold(My_Data);
      My_Data2.Initialize(My_Tuple);
      Assert(My_Data.Read(2) = My_Data2.Read(2), "Register array not initialized from tuple.");
   end Run_Test;



   -----------------------------------------------------------------------------
   procedure Set_Up (T : in out Machine_Test_Case) is
   begin
      kv.avm.Log.Verbose := False;
      Put_Line("==========================================================");
   end Set_Up;

   -----------------------------------------------------------------------------
   procedure Tear_Down (T : in out Machine_Test_Case) is
   begin
      kv.avm.Log.Verbose := False;
      kv.avm.Actors.Empty_Actor_Map;
      Put_Line("==========================================================");
   end Tear_Down;


   -----------------------------------------------------------------------------
   function Name (T : Test_9) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 09: Start_With.");
   end Name;

   procedure Run_Test (T : in out Test_9) is
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Factory : aliased kv.avm.Test.Runners.Runner_Factory;
      Data : kv.avm.Memories.Register_Set_Type(0..8);
      Registers : kv.avm.Memories.Register_Array_Type;
      Instance : kv.avm.Executables.Executable_Access;
      Runner : kv.avm.Test.Runners.Runner_Access;
   begin
      Put_Line("test 09");
      --kv.avm.Log.Verbose := True;
      Builder.Initialize;
      Builder.Parse_Input_File("test_a1.volea");
      Builder.Transfer_Actors_To_Store;
      VM.Initialize(CPU'UNCHECKED_ACCESS, Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);
      Registers.Initialize((Make_U(0), Make_U(1)));
      VM.Start_With("Test_A1", "Go", Registers);
      VM.Step;
      Instance := VM.Current_Instance;
      Assert(Instance.all in kv.avm.Test.Runners.Runner_Type'CLASS, "Incorrect executable class");
      Assert(Factory.Get_Allocated_Count = 1, "Incorrect number of runners allocated");
      Runner := Factory.Get_Runner_By_ID(1); -- ID = count
      Assert(VM.Get_Queue_Size = 1, "wrong number of queued messages");
      Assert(Runner.Last_Msg.Get_Name = "CONSTRUCTOR", "the 'CONSTRUCTOR' message wasn't delivered");
      VM.Step;
      VM.Step;
      VM.Step;
      VM.Step;
      VM.Step;
      Assert(VM.Get_Queue_Size = 0, "wrong number of queued messages");
      Assert(Runner.Last_Msg.Get_Name = "Go", "the 'Go' message wasn't delivered");
      while not VM.Done loop
         VM.Step;
         Assert(VM.Get_Steps < 20, "VM did not complete in time");
      end loop;
      Assert(VM.Get_Total = 1, "wrong number of executables in the VM");
      Assert(VM.Get_Active = 0, "wrong number of active executables");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_9b) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 9b: machine utilities.");
   end Name;

   procedure Run_Test (T : in out Test_9b) is
      Factory : aliased kv.avm.Test.Runners.Runner_Factory;
      Instance : kv.avm.Executables.Executable_Access;
      Executable : kv.avm.Executables.Executable_Access;
      Reference : kv.avm.Actor_References.Actor_Reference_Type;
      List_1 : kv.avm.Executable_Lists.Executable_Holder_Type;
      List_2 : kv.avm.Executable_Lists.Executable_Holder_Type;
      Place : kv.avm.Executable_Lists.Cursor_Type;
      Position : kv.avm.Executable_Lists.Cursor_Type;
      Handle : kv.avm.Executable_Lists.Executable_Handle_Access;
      use kv.avm.Executable_Lists;
   begin
      Put_Line("test 9b");
      Factory.New_Executable(null, null, Instance, Reference);
      begin
         List_1.Add(Instance, Reference);
         Assert(False, "Using an uninitialized list did not raise an exception.");
      exception
         when Constraint_Error =>
            null; -- expected
         when others =>
            Assert(False, "Expected constraint error");
      end;

      List_1.Initialize(kv.avm.Control.Active);
      List_2.Initialize(kv.avm.Control.Blocked);

      List_1.Add(Instance, Reference);
      Assert(List_1.Is_In(Instance), "couldn't find instance");
      Place := List_1.Find(Instance);
      Executable := List_1.Get(Place);
      Assert(Instance = Executable, "find or get didn't work");

      List_2.Acquire_From(Place, List_1);

      Assert(not List_1.Is_In(Instance), "found removed instance");

      Place := List_2.Find(Instance);
      Handle := List_2.Get_Handle(Place);
      Position := Handle.Get_Cursor;
      Assert(Place = Position, "wrong place");
      Assert(Handle.Get_List = kv.avm.Control.Blocked, "wrong list");

      Factory.New_Executable(null, null, Executable, Reference);
      List_2.Add(Executable, Reference);

      Place := List_2.Find(Executable);
      Handle := List_2.Get_Handle(Place);
      Position := Handle.Get_Cursor;
      Assert(Position = 2, "wrong place");

      List_2.Drop(Instance);
      Assert(not List_2.Is_In(Instance), "found deleted instance");

      Position := Handle.Get_Cursor;
      Assert(Position = 1, "wrong place"); -- My handle should be updated to reflect the new location in List_2

      Factory.New_Executable(null, null, Instance, Reference);
      List_1.Add(Instance, Reference);
      Factory.New_Executable(null, null, Executable, Reference);
      List_1.Add(Executable, Reference);

      Handle := List_1.Get_Handle(List_1.Find(Executable));
      List_1.Drop(Executable);
      Position := Handle.Get_Cursor;
      Assert(Position = 0, "dropped executable not flagged as being dropped");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_9c) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 9c: garbage collection.");
   end Name;

   procedure Run_Test (T : in out Test_9c) is
      Empty_Tuple : aliased kv.avm.Tuples.Tuple_Type;
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Factory : aliased kv.avm.Test.Runners.Runner_Factory;
      Runner : kv.avm.Test.Runners.Runner_Access;
      Actor_1 : kv.avm.Actor_References.Actor_Reference_Type;
      Status : kv.avm.Control.Status_Type;
      Message : kv.avm.Messages.Message_Type;
   begin
      Put_Line("test 9c");
      Empty_Tuple.Initialize;
      Empty_Tuple.Fold_Empty;
      Builder.Initialize;
      Builder.Parse_Input_File("test_a1.volea");
      Builder.Transfer_Actors_To_Store;
      --kv.avm.Log.Verbose := True;
      VM.Initialize(CPU'UNCHECKED_ACCESS, Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);
      VM.New_Actor("Test_A1", Actor_1);
      VM.Set_Garbage_Trigger(50);

      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Actor_1,
          Message_Name => "CONSTRUCTOR",
          Data         => Empty_Tuple,
          Future       => kv.avm.Control.NO_FUTURE);
      VM.Post_Message
         (Message => Message,
          Status  => Status);

      Runner := Factory.Get_Runner_By_ID(1); -- ID = count
      Runner.Set_Behavior_Spawn(3);
      for Step in 1 .. 30 loop
         VM.Step;
      end loop;
      Assert(VM.Get_Total >= 3, "wrong number of executables in the VM");
      for Step in 1 .. 500 loop
         VM.Step;
      end loop;
      Assert(VM.Get_Active <= 10, "too many active executables");
      Assert(VM.Get_Total <= 10, "too many executables in the VM");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_9d) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 9d: trap to machine.");
   end Name;

   procedure Run_Test (T : in out Test_9d) is
      Empty_Tuple : aliased kv.avm.Tuples.Tuple_Type;
--      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Factory : aliased kv.avm.Instances.Instance_Factory;
--      Status : kv.avm.Control.Status_Type;

      use kv.avm.Registers;
      use kv.avm.Assemblers;
      use kv.avm.Instances;
   begin
      Put_Line("test 9d");
      VM.Initialize(CPU'UNCHECKED_ACCESS, Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);
      T.p.Initialize(VM'UNCHECKED_ACCESS);
      T.Mem_Set(F0, Make_Tuple_Map((L0, L1, L2)));
      T.Mem_Set(F1, (format => kv.avm.Registers.Immutable_String, The_String => +"print"));

      T.Mem_Set(L0, U(113));
      T.Mem_Set(L1, U(4));
      T.Mem_Set(L2, U(8));
      --        L3 is the tuple (message arguments)
      --        L4 is the reply future if there is a reply

      T.c(0) := Decode_Op_Code("Fold L3 F0");
      T.c(1) := Decode_Op_Code("TRAP L4 := F1 L3");
      --kv.avm.Log.Verbose := True;
      T.Step;
      T.Step;
      Assert(kv.avm.Log.Get_Last_Log_Line = "Machine_Type.Trap_To_The_Machine called with name = 'print'", "wrong log message");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_10) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 10: Message Queue accumulation.");
   end Name;

   procedure Run_Test (T : in out Test_10) is
      -- Problem: a pair of actors, a producer and a consumer, run in parallel
      -- but the producer cycles faster than the consumer.  Messages accumulate
      -- in the Machine's message queue threatening to take all memory.
      Empty_Tuple : aliased kv.avm.Tuples.Tuple_Type;
      Builder : aliased kv.avm.Assemblers.Assembler_Type;
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Factory : aliased kv.avm.Test.Runners.Runner_Factory;
      Runner : kv.avm.Test.Runners.Runner_Access;
      Actor_1 : kv.avm.Actor_References.Actor_Reference_Type;
      Actor_2 : kv.avm.Actor_References.Actor_Reference_Type;
      Status : kv.avm.Control.Status_Type;
      Q_Limit : constant := 3;
      Message : kv.avm.Messages.Message_Type;
   begin
      Put_Line("test 10");
      Empty_Tuple.Initialize;
      Empty_Tuple.Fold_Empty;
      --kv.avm.Log.Verbose := True;
      Builder.Initialize;
      Builder.Parse_Input_File("test_a1.volea");
      Builder.Transfer_Actors_To_Store;
      VM.Initialize(CPU'UNCHECKED_ACCESS, Factory'UNCHECKED_ACCESS);
      VM.Set_Queue_Limit(Q_Limit);
      CPU.Initialize(VM'UNCHECKED_ACCESS);
      VM.New_Actor("Test_A1", Actor_1);
      VM.New_Actor("Test_A1", Actor_2);

      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Actor_1,
          Message_Name => "CONSTRUCTOR",
          Data         => Empty_Tuple,
          Future       => kv.avm.Control.NO_FUTURE);
      VM.Post_Message
         (Message => Message,
          Status  => Status);
      Message.Finalize;

      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Actor_2,
          Message_Name => "CONSTRUCTOR",
          Data         => Empty_Tuple,
          Future       => kv.avm.Control.NO_FUTURE);
      VM.Post_Message
         (Message => Message,
          Status       => Status);

      Assert(VM.Get_Queue_Size = 0, "wrong number of queued messages");
      Runner := Factory.Get_Runner_By_ID(1); -- ID = count
      Runner.Set_Behavior_Send(Actor_2, 1);
      loop
         exit when VM.Get_Steps > 500;
         VM.Step;
         Assert(VM.Get_Queue_Size <= Q_Limit, "queue limit exceeded");
      end loop;
      Assert(VM.Get_Total = 2, "wrong number of executables in the VM");
      Assert(VM.Get_Steps > 500, "wrong number of steps");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_11) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 11: FOLD and PEEK.");
   end Name;

   procedure Run_Test (T : in out Test_11) is
      Tuple_Index : constant := 3;
      new_tuple : reference_type := (memory => local, index => Tuple_Index);
      data_start : reference_type := (memory => local, index => 0);
      data_count : reference_type := (memory => fixed, index => 0);
      peek_1 : reference_type := (memory => fixed, index => 1);
      peek_2 : reference_type := (memory => fixed, index => 2);
      peek_3 : reference_type := (memory => fixed, index => 3);
      data_peek : reference_type := (memory => local, index => 4);
      Tuple_Layout : kv.avm.Tuples.Map_Type;
      Fold_List : aliased constant Reference_Array_Type := (F3, L1, L2);
      use kv.avm.Assemblers;
   begin
      Put_Line("test 11");
      Tuple_Layout.Set(Fold_List'ACCESS);
      Assert(Tuple_Layout.Get.all(0).Index = 3, "Tuple_Layout(0)'s index is not 3.");

      --T.c(0) := (op_code => FOLD, a => new_tuple, x => data_start, y => data_count);
      T.c(0) := (op_code => Fold, lhs => new_tuple, rhs => F4);
      T.Mem_Set(L0, U(2)); -- target of data_start
      T.Mem_Set(L1, U(4));
      T.Mem_Set(L2, U(8));
      T.Mem_Set(F0, U(3)); -- target of data_count
      T.Mem_Set(F1, U(0));
      T.Mem_Set(F2, U(1));
      T.Mem_Set(F3, U(2));
      T.Mem_Set(F4, (format => Tuple_Map, Map => Tuple_Layout));
      T.Step;
      Assert(T.i.Program_Counter = 1, "instruction did not advance pc to 1.");
      Assert(T.Mem_Get(L3).format = kv.avm.Registers.Tuple,
         "Target ('" & Data_Kind'IMAGE(T.Mem_Get(L3).format) & "') was not 'Tuple'.");

      T.c(1) := (op_code => PEEK, a => data_peek, x => new_tuple, y => peek_1);
      T.Step;
      Assert(T.Mem_Get(L4).format = kv.avm.Registers.Unsigned_Integer,
         "Peek 1 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') was not 'Unsigned_Integer'.");
      Assert(T.Mem_Get(L4).unsigned_value = 2, "Peek 1 was not 2.");
      --T.c(2) := (op_code => peek, a => data_peek, x => new_tuple, y => peek_2);
      T.c(2) := Decode_Op_Code("PEEK_IMMEDIATE L4 := L3 [ 1 ]");
      T.Step;
      Assert(T.Mem_Get(L4).format = kv.avm.Registers.Unsigned_Integer,
         "Peek 2 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') was not 'Unsigned_Integer'.");
      Assert(T.Mem_Get(L4).unsigned_value = 4, "Peek 2 was not 4.");
      T.c(3) := (op_code => peek, a => data_peek, x => new_tuple, y => peek_3);
      T.Step;
      Assert(T.Mem_Get(L4).format = kv.avm.Registers.Unsigned_Integer,
         "Peek 3 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') was not 'Unsigned_Integer'.");
      Assert(T.Mem_Get(L4).unsigned_value = 8, "Peek 3 was not 8.");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_12) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 12: Blocking operation.");
   end Name;

   procedure Run_Test (T : in out Test_12) is
      use kv.avm.Registers;
      use kv.avm.Assemblers;
      use kv.avm.Instances;
   begin
      Put_Line("test 12");
      --kv.avm.Log.Verbose := True;
      kv.avm.actor_pool.Add(Fake_Invoker'ACCESS, Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Invoker(Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Future(99);

      T.Mem_Set(F0, Make_Tuple_Map((L0, L1, L2)));
      T.Mem_Set(F1, (format => Message_Definition, Message_Name => +Msg_Name, Send_Count => 3, Reply_Count => 1));

      T.Mem_Set(L0, U(113));
      T.Mem_Set(L1, U(4));
      T.Mem_Set(L2, U(8));
      --        L3 is the tuple (message arguments)
      --        L4 is the reply future if there is a reply
      T.Mem_Set(L5, (format => kv.avm.Registers.Actor_Reference, Instance => Fake_Ref));

      T.c(0) := Decode_Op_Code("Fold L3 F0");
      T.c(1) := Decode_Op_Code("ACTOR_CALL L4 := L3 => L5 . F1");
      T.c(2) := Decode_Op_Code("NO_OP");
      T.c(3) := Decode_Op_Code("PEEK_IMMEDIATE L6 := L4 [ 0 ]");
      T.Step;
      T.Step;
      T.Step;
      T.Step;

      Assert(Fake_Machine.Message_Name.all = Msg_Name, "message was not posted.");

      Assert(T.Mem_Get(L4).format = kv.avm.Registers.Future,
         "L4 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') was not 'Future'.");
      Assert(T.Mem_Get(L4).ID = Fake_Machine.Future, "L4 was not = Fake.Future.");
      Assert(kv.avm.actor_pool.Resolve(Fake_Machine.Reply_To) = +T.i, "Reply_To error.");

      Assert(T.s = kv.avm.Control.Blocked, "Execution of PEEK_IMMEDIATE did not return blocked status.");
      Assert(T.i.Program_Counter = 3, "Program Counter did not hold at blocked instruction.");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_13) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 13: EMIT.");
   end Name;

   procedure Run_Test (T : in out Test_13) is
      L24 : reference_type := (memory => local, index => 24);
   begin
      Put_Line("test 13");
      T.c(0) := (op_code => emit, value => L24);
      T.Mem_Set(L24, U(13));
      --Put_Line("<<<<<<<<<<<< emit (13):");
      T.Step;
      --Put_Line(">>>>>>>>>>>>");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_14) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 14: SELF_TAIL_SEND.");
   end Name;

   procedure Run_Test (T : in out Test_14) is
      use kv.avm.Registers;
      use kv.avm.Assemblers;
   begin
      Put_Line("test 14");
      --kv.avm.Log.Verbose := True;
      kv.avm.actor_pool.Add(Fake_Invoker'ACCESS, Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Invoker(Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Future(88);

      T.Mem_Set(F0, Make_Tuple_Map((L0, L1, L2)));
      T.Mem_Set(F1, (format => Message_Definition, Message_Name => +Msg_Name, Send_Count => 3, Reply_Count => 1));

      T.Mem_Set(L0, U(113)); -- target of Fold
      T.Mem_Set(L1, U(4));
      T.Mem_Set(L2, U(8));
      --        L3 is the tuple (message arguments)

      T.c(0) := Decode_Op_Code("Fold L3 F0");
      T.c(1) := Decode_Op_Code("SELF_TAIL_SEND L3 => F1");
      T.Step;
      T.Step;

      T.f(0) := Decode_Op_Code("SET L13 I0");
      T.Step;
      Assert(T.Mem_Get(L13).unsigned_value = 113, "SELF_TAIL_SEND did not transfer control to the test message.");


      -- Now we should be running FooBar (in T.f)
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_15) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 15: HALT_ACTOR.");
   end Name;

   procedure Run_Test (T : in out Test_15) is
   begin
      Put_Line("test 15");
      Assert(T.i.Alive = True, "Healthy instance reports that it is *NOT* alive.");
      T.c(0) := (op_code => HALT_ACTOR);
      T.Step;
      Assert(T.i.Program_Counter = 0, "program counter did not stay at HALT_ACTOR instruction.");
      Assert(T.i.Alive = False, "Halted instance reports that it is still alive.");
      Assert(T.i.Is_Running = False, "Halted instance reports that it is still running.");
   end Run_Test;




   -----------------------------------------------------------------------------
   function Make_Code(Code : kv.avm.Instructions.Code_Type) return kv.avm.Instructions.Code_Access is
      Code_Pointer : kv.avm.Instructions.Code_Access;
   begin
      Code_Pointer := new kv.avm.Instructions.Code_Type'(Code);
      return Code_Pointer;
   end Make_Code;





   -----------------------------------------------------------------------------
   function Name (T : Test_16) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 16: Machine test.");
   end Name;

   Test_16_Actor_Name : aliased constant String := "Test_16_Actor";
   Test_16_Return_1 : aliased constant String := "Return_1";

   procedure Run_Test (T : in out Test_16) is
      pragma Unreferenced (T);
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Instance_Factory : aliased kv.avm.Instances.Instance_Factory;
      Actor_Pointer : kv.avm.Actors.Actor_Access;
      Constructor : kv.avm.Instructions.Code_Access;
      Subroutine : kv.avm.Instructions.Code_Access;
      Instance_1 : kv.avm.Actor_References.Actor_Reference_Type;
      Data : aliased kv.avm.Tuples.Tuple_Type;
      Constants : kv.avm.Memories.Register_Array_Type;
      Registers : kv.avm.Memories.Register_Array_Type;
      Status : kv.avm.Control.Status_Type;
      Message : kv.avm.Messages.Message_Type;
   begin
      Put_Line("test 16");
      VM.Initialize(CPU'UNCHECKED_ACCESS, Instance_Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);
      Constructor := Make_Code(((op_code => REPLY, value => F1),
                                (op_code => STOP_FRAME)));
      Constants.Initialize((U(0), U(1)));
      Actor_Pointer := kv.avm.Actors.New_Actor(Test_16_Actor_Name, Constructor, 3, Constants);
      Subroutine := Make_Code(((op_code => SET, lhs => L0, rhs => F1),
                               (op_code => EMIT, value => L0),
                               (op_code => REPLY, value => L0),
                               (op_code => STOP_FRAME)));
      Actor_Pointer.Add_Method(New_Method(Test_16_Return_1, Subroutine));
      VM.New_Actor(Test_16_Actor_Name, Instance_1);

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


      Assert(VM.Get_Total = 1, "There should be one actor total.");
      Assert(VM.Get_Active = 1, "There should be one active actor.");
      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Instance_1,
          Message_Name => Test_16_Return_1,
          Data         => Data,
          Future       => kv.avm.Control.NO_FUTURE);
      VM.Post_Message
         (Message => Message,
          Status  => Status);
      while not VM.Done loop
         VM.Step;
      end loop;
      Assert(VM.Get_Steps = 5, "5 steps should have been taken.");
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_17) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 17: Multi-actor messaging.");
   end Name;

   Test_17_Calculator : aliased constant String := "Test_17_Calculator";

   procedure Run_Test (T : in out Test_17) is
      pragma Unreferenced (T);
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Instance_Factory : aliased kv.avm.Instances.Instance_Factory;
      Actor_Pointer : kv.avm.Actors.Actor_Access;
      Actor_Pointer2 : kv.avm.Actors.Actor_Access;
      Constructor : kv.avm.Instructions.Code_Access;
      Constructor2 : kv.avm.Instructions.Code_Access;
      Subroutine : kv.avm.Instructions.Code_Access;
      Subroutine2 : kv.avm.Instructions.Code_Access;
      Constants : kv.avm.Memories.Register_Array_Type;
      ConstantI2 : kv.avm.Memories.Register_Array_Type;
      Instance_1 : kv.avm.Actor_References.Actor_Reference_Type;
      Data : aliased kv.avm.Tuples.Tuple_Type;
      T1 : aliased kv.avm.Tuples.Tuple_Type;
      T2 : aliased kv.avm.Tuples.Tuple_Type;
      Rp : access constant kv.avm.Memories.Register_Set_Type;
      Registers : kv.avm.Memories.Register_Array_Type;
      RegisterI2 : kv.avm.Memories.Register_Array_Type;
      Status : kv.avm.Control.Status_Type;
      Message : kv.avm.Messages.Message_Type;
      use kv.avm.Registers;
   begin
      Put_Line("test 17");
      Registers.Initialize((U(0), U(1)));
      T1.Initialize;
      T1.Fold(Registers);
      T2 := T1;
      Rp := T2.Unfolded;
      Assert(Rp /= null, "Rp should not be null.");

      VM.Initialize(CPU'UNCHECKED_ACCESS, Instance_Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);

      Constructor := Make_Code(((op_code => REPLY, value => F1),
                                (op_code => STOP_FRAME)));
      Constants.Initialize((U(0),
                                   U(1),
                                   U(2),
                                   U(3),
                                   U(127),
                                   U(137),
                                   U(147),
                                   U(227),
                                   U(237),
                                   U(247),
                                   U(327),
                                   U(337),
                                   U(347),
                                   (format => Actor_Definition, Actor_Kind => +Test_17_Calculator),
                                   (format => Message_Definition, Message_Name => +Msg_Name, Send_Count => 3, Reply_Count => 2),
                                   Make_Tuple_Map((F4, F5, F6)),
                                   Make_Tuple_Map((F7, F8, F9)),
                                   Make_Tuple_Map((F10, F11, F12))
                                   ));
      Actor_Pointer := kv.avm.Actors.New_Actor
         (Name            => "Actor17",
          Constructor     => Constructor,
          Attribute_Count => 3,
          Fixed_Registers => Constants,
          Parent          => null);

      Subroutine := Make_Code(((op_code => NEW_ACTOR, a => L1, y => F13, x => F1),
                               (op_code => NEW_ACTOR, a => L2, y => F13, x => F2),
                               (op_code => NEW_ACTOR, a => L3, y => F13, x => F3),
                               --(op_code => FOLD, a => L4, x => F4, y => F3),
                               (op_code => Fold, lhs => L4, rhs => F15),
                               --(op_code => SEND, reply => True, tail => False, message => L4, actor => L1, profile => F14),
                               (op_code => ACTOR_CALL, rply_5a1 => L5, args_5a1 => L4, actr_5a1 => L1, mdef_5a1 => F14),
                               --(op_code => FOLD, a => L6, x => F7, y => F3),
                               (op_code => Fold, lhs => L6, rhs => F16),
                               --(op_code => SEND, reply => True, tail => False, message => L6, actor => L2, profile => F14),
                               (op_code => ACTOR_CALL, rply_5a1 => L7, args_5a1 => L6, actr_5a1 => L2, mdef_5a1 => F14),
                               --(op_code => FOLD, a => L8, x => F10, y => F3),
                               (op_code => Fold, lhs => L8, rhs => F17),
                               --(op_code => SEND, reply => True, tail => False, message => L8, actor => L3, profile => F14),
                               (op_code => ACTOR_CALL, rply_5a1 => L9, args_5a1 => L8, actr_5a1 => L3, mdef_5a1 => F14),
                               (op_code => PEEK, a => L10, x => L5, y => F0),
                               (op_code => PEEK, a => L11, x => L5, y => F1),
                               (op_code => PEEK, a => L12, x => L7, y => F0),
                               (op_code => PEEK, a => L13, x => L7, y => F1),
                               (op_code => PEEK, a => L15, x => L9, y => F0),
                               (op_code => PEEK, a => L16, x => L9, y => F1),
                               (op_code => COMPUTE, action => add, result => L0, left => F0, right => L10),
                               (op_code => COMPUTE, action => add, result => L0, left => L0, right => L11),
                               (op_code => COMPUTE, action => add, result => L0, left => L0, right => L12),
                               (op_code => COMPUTE, action => add, result => L0, left => L0, right => L13),
                               (op_code => COMPUTE, action => add, result => L0, left => L0, right => L15),
                               (op_code => COMPUTE, action => add, result => L0, left => L0, right => L16),
                               (op_code => EMIT, value => L0),
                               (op_code => TRAP, a => L17, x => F1, y => L0),
                               (op_code => REPLY, value => L0),
                               (op_code => STOP_FRAME)));
      Actor_Pointer.Add_Method(New_Method("Go", Subroutine));

      VM.New_Actor("Actor17", Instance_1);
      Assert(VM.Get_Total = 1, "There should be one actor total.");
      Assert(VM.Get_Active = 1, "There should be one active actor.");

      RegisterI2.Initialize((U(0), U(1)));
      Data.Initialize;
      Data.Fold(RegisterI2);

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

      Constructor2 := Make_Code(((op_code => SET, lhs => A0, rhs => I0),
                                 (op_code => EMIT, value => A0),
                                 (op_code => REPLY, value => F1),
                                 (op_code => STOP_FRAME)));
      ConstantI2.Initialize((U(0),
                                    U(1),
                                    U(2),
                                    U(3),
                                    Make_Tuple_Map((L2, L3))
                                   ));
      Actor_Pointer2 := kv.avm.Actors.New_Actor
         (Name            => Test_17_Calculator,
          Constructor     => Constructor2,
          Attribute_Count => 3,
          Fixed_Registers => ConstantI2,
          Parent          => null);
      Subroutine2 := Make_Code(((op_code => COMPUTE, action => add, result => L0, left => I0, right => I1),
                                (op_code => COMPUTE, action => add, result => L1, left => I1, right => I2),
                                (op_code => COMPUTE, action => sub, result => L2, left => L1, right => L0),
                                (op_code => SET, lhs => L3, rhs => A0),
                                --(op_code => FOLD, a => L4, x => L2, y => F2),
                                (op_code => Fold, lhs => L4, rhs => F4),
                                (op_code => REPLY, value => L4),
                                (op_code => STOP_FRAME)));
      Actor_Pointer2.Add_Method(New_Method(Msg_Name, Subroutine2));


      while not VM.Done loop
         VM.Step;
         Assert(VM.Get_Steps < 200, "Too many steps have been taken.");
      end loop;
      Assert(VM.Get_Steps = 60, "Wrong step count."&Natural'IMAGE(VM.Get_Steps));
   end Run_Test;


   -----------------------------------------------------------------------------
   function Name (T : Test_18) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 18: ADD, SUBTRACT, MULTIPLY, DIVIDE.");
   end Name;

   procedure Run_Test (T : in out Test_18) is
      a : reference_type := (memory => local, index => 0);
      x : reference_type := (memory => local, index => 1);
      y : reference_type := (memory => local, index => 2);
      s : reference_type := (memory => local, index => 3);
      m : reference_type := (memory => local, index => 4);
      d : reference_type := (memory => local, index => 5);
      b : reference_type := (memory => local, index => 6);
   begin
      Put_Line("test 18");
      T.Mem_Set(L1, (format => Signed_Integer, signed_value => 3));
      T.Mem_Set(L2, (format => Signed_Integer, signed_value => 5));
      T.c(0) := (op_code => compute, action => Add, result => a, left => x, right => y);
      T.c(1) := (op_code => compute, action => Sub, result => s, left => y, right => x);
      T.c(2) := (op_code => compute, action => Mul, result => m, left => x, right => y);
      T.c(3) := (op_code => compute, action => Div, result => d, left => y, right => x);

      T.c(4) := (op_code => compute, action => Eq , result => b, left => y, right => x);
      T.c(5) := (op_code => compute, action => Neq, result => b, left => y, right => x);
      T.c(6) := (op_code => compute, action => L_t, result => b, left => x, right => x);
      T.c(7) := (op_code => compute, action => Lte, result => b, left => x, right => x);
      T.c(8) := (op_code => compute, action => G_t, result => b, left => x, right => y);
      T.c(9) := (op_code => compute, action => Gte, result => b, left => y, right => x);

      T.Step;
      Assert(T.Mem_Get(L0).format = Signed_Integer, "Local(0) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L0).signed_value = 8, "Local(0) was not 3+5=8.");
      Assert(T.i.Program_Counter = 1, "instruction did not advance pc to 1.");

      T.Step;
      Assert(T.Mem_Get(L3).format = Signed_Integer, "Local(3) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L3).signed_value = 2, "Local(3) was not 5-3=2.");
      Assert(T.i.Program_Counter = 2, "instruction did not advance pc to 2.");

      T.Step;
      Assert(T.Mem_Get(L4).format = Signed_Integer, "Local(4) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L4).signed_value = 15, "Local(4) was not 3*5=15.");
      Assert(T.i.Program_Counter = 3, "instruction did not advance pc to 3.");

      T.Step;
      Assert(T.Mem_Get(L5).format = Signed_Integer, "Local(5) was not 'Signed_Integer'.");
      Assert(T.Mem_Get(L5).signed_value = 1, "Local(5) was not 5/3=1.");
      Assert(T.i.Program_Counter = 4, "instruction did not advance pc to 4.");

      T.Step;
      Assert(T.Mem_Get(L6).format = Bit_Or_Boolean, "Local(6) was not 'Bit_Or_Boolean'.");
      Assert(T.Mem_Get(L6).bit = False, "Local(6) was not correct.");

      T.Step;
      Assert(T.Mem_Get(L6).bit = True, "Local(6) was not correct.");

      T.Step;
      Assert(T.Mem_Get(L6).bit = False, "Local(6) was not correct.");

      T.Step;
      Assert(T.Mem_Get(L6).bit = True, "Local(6) was not correct.");

      T.Step;
      Assert(T.Mem_Get(L6).bit = False, "Local(6) was not correct.");

      T.Step;
      Assert(T.Mem_Get(L6).bit = True, "Local(6) was not correct.");
   end Run_Test;


   -----------------------------------------------------------------------------
   procedure Send_Reply_Tail_2
      (T        : in out Instruction_Test_Case;
       Do_Reply : in     Boolean;
       Do_Tail  : in     Boolean) is

      use kv.avm.Registers;
      use kv.avm.Assemblers;
   begin
      --kv.avm.Log.Verbose := True;
      kv.avm.actor_pool.Add(Fake_Invoker'ACCESS, Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Invoker(Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Future(1027);

      T.Mem_Set(F0, Make_Tuple_Map((L0, L1, L2)));
      T.Mem_Set(F1, (format => Message_Definition, Message_Name => +Msg_Name, Send_Count => 3, Reply_Count => 1));
      T.Mem_Set(F2, U(0)); -- peek_0
      T.Mem_Set(F3, U(3));

      T.Mem_Set(L0, U(113)); -- target of Fold
      T.Mem_Set(L1, U(4));
      T.Mem_Set(L2, U(8));
      --        L3 is the tuple (message arguments)
      --        L4 is the reply future if there is a reply
      T.Mem_Set(L5, (format => kv.avm.Registers.Actor_Reference, Instance => Fake_Ref));

      T.c(0) := Decode_Op_Code("Fold L3 F0");
      if Do_Reply then if Do_Tail then T.c(1) := Decode_Op_Code("ACTOR_TAIL_CALL  L3 => L5 . F1"); --!@#$
                       else            T.c(1) := Decode_Op_Code("ACTOR_CALL L4 := L3 => L5 . F1"); end if;
      else             if Do_Tail then T.c(1) := Decode_Op_Code("ACTOR_TAIL_SEND  L3 => L5 . F1"); --!@#$
                       else            T.c(1) := Decode_Op_Code("ACTOR_SEND       L3 => L5 . F1"); end if;
      end if;
      T.Step;
      T.Step;

      Assert(Fake_Machine.Posted = 27, "message was not posted.");
      Assert(Fake_Machine.Message_Name.all = Msg_Name, "message name error.");
      Assert(kv.avm.actor_pool.Resolve(Fake_Machine.Destination) = +(Fake_Instance'ACCESS), "Destination error.");
      Assert(kv.avm.actor_pool.Resolve(Fake_Machine.Source) = +T.i, "Source error.");
      Assert(Fake_Machine.Data.Peek(0).unsigned_value = 113, "Tuple error.");
      if Do_Reply then
         if Do_Tail then
            Assert(T.Mem_Get(L4).format /= kv.avm.Registers.Future,
               "local 4 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') should not be 'Future'.");
            Assert(Fake_Machine.Reply_To = T.i.Get_Frame.Get_Invoker, "Do_Tail Reply_To error.");
            Assert(Fake_Machine.Future = 1027, "Fake.Future was not 1027.");
         else
            Assert(T.Mem_Get(L4).format = kv.avm.Registers.Future,
               "local 4 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') was not 'Future'.");
            Assert(T.Mem_Get(L4).ID >= 3, "local 4 was not >= 3.");
            Assert(T.Mem_Get(L4).ID = Fake_Machine.Future, "local 4 was not = Fake.Future.");
            Assert(kv.avm.actor_pool.Resolve(Fake_Machine.Reply_To) = +T.i, "Reply_To error.");
         end if;
      else
         if Do_Tail then
            Assert(T.Mem_Get(L4).format /= kv.avm.Registers.Future,
               "local 4 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') should not be 'Future'.");
            Assert(Fake_Machine.Reply_To = T.i.Get_Frame.Get_Invoker, "Do_Tail Reply_To error.");
         else
            Assert(kv.avm.actor_pool.Resolve(Fake_Machine.Reply_To) = +T.i, "Reply_To error.");
         end if;
      end if;
   end Send_Reply_Tail_2;


   -----------------------------------------------------------------------------
   function Name (T : Test_19) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 19: SELF_CALL.");
   end Name;

   procedure Run_Test (T : in out Test_19) is
      use kv.avm.Registers;
      use kv.avm.Assemblers;
   begin
      --kv.avm.Log.Verbose := True;
      Put_Line("test 19");

      kv.avm.actor_pool.Add(Fake_Invoker'ACCESS, Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Invoker(Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Future(1027);

      T.Mem_Set(F0, String_To_Tuple_Map("[ L0 L1 L2]"));
      T.Mem_Set(F1, (format => Message_Definition, Message_Name => +Msg_Name, Send_Count => 3, Reply_Count => 1));
      T.Mem_Set(F2, U(0)); -- peek_0
      T.Mem_Set(F3, U(3));

      T.Mem_Set(L0, U(113)); -- target of Fold
      T.Mem_Set(L1, U(4));
      T.Mem_Set(L2, U(8));
      -- local 3 is the tuple (message arguments)
      -- local 4 is the reply future if there is a reply

      T.c(0) := Decode_Op_Code("Fold L3 F0");
      T.c(1) := Decode_Op_Code("SELF_CALL L4 := L3 => F1");
      T.c(2) := Decode_Op_Code("PEEK L7 L4 F2");
      T.Step;
      T.Step;
      -- L4 should be a future
      Assert(T.Mem_Get(L4).format = kv.avm.Registers.Future,
         "local 4 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') was not 'Future'.");
      Assert(T.Mem_Get(L4).ID >= 1, "local 4 was not >= 1.");

      T.f(0) := Decode_Op_Code("COMPUTE L6 := I0 + F3");
      T.f(1) := Decode_Op_Code("REPLY L6");
      T.Step;
      T.Step;
      -- L4 should now be a tuple returned from FooBar.
      Assert(T.Mem_Get(L4).format = kv.avm.Registers.Tuple,
         "local 4 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') was not 'Tuple'.");

      T.Step;
      Assert(T.Mem_Get(L7).format = kv.avm.Registers.Unsigned_Integer,
         "Peek 0 ('" & Data_Kind'IMAGE(T.Mem_Get(L4).format) & "') was not 'Unsigned_Integer'.");
      Assert(T.Mem_Get(L7).unsigned_value = 116, "Peek 0 was not 116.");

   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_20) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 20: ACTOR_CALL.");
   end Name;

   procedure Run_Test (T : in out Test_20) is
   begin
      Put_Line("test 20");
      Send_Reply_Tail_2(Instruction_Test_Case(T), true, false);
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_21) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 21: ACTOR_TAIL_CALL.");
   end Name;

   procedure Run_Test (T : in out Test_21) is
   begin
      Put_Line("test 21");
      Send_Reply_Tail_2(Instruction_Test_Case(T), true, true);
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_22) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 22: ACTOR_TAIL_SEND.");
   end Name;

   procedure Run_Test (T : in out Test_22) is
   begin
      Put_Line("test 22");
      Send_Reply_Tail_2(Instruction_Test_Case(T), false, true);
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_23) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 23: ACTOR_SEND.");
   end Name;

   procedure Run_Test (T : in out Test_23) is
   begin
      Put_Line("test 23");
      Send_Reply_Tail_2(Instruction_Test_Case(T), false, false);
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_24) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 24: ASSERT.");
   end Name;

   procedure Run_Test (T : in out Test_24) is
      use kv.avm.Assemblers;
   begin
      Put_Line("test 24");
      --kv.avm.Log.Verbose := True;
      T.Mem_Set(L0, B(True));
      T.Mem_Set(L1, B(False));
      T.c(0) := Decode_Op_Code("ASSERT L0");
      T.c(1) := Decode_Op_Code("ASSERT L1");
      T.Step;
      Assert(T.i.Program_Counter = 1, "instruction did not advance pc to 1.");
      T.Step;
      Assert(T.i.Program_Counter = 1, "pc did not stay at 1 (failed assertion).");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_25) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 25: SELF_SEND.");
   end Name;

   procedure Run_Test (T : in out Test_25) is
      use kv.avm.Registers;
      use kv.avm.Assemblers;
   begin
      Put_Line("test 25");
      --kv.avm.Log.Verbose := True;

      kv.avm.actor_pool.Add(Fake_Invoker'ACCESS, Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Invoker(Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Future(13);

      T.Mem_Set(F0, String_To_Tuple_Map("[ L0 L1 L2]"));
      T.Mem_Set(F1, (format => Message_Definition, Message_Name => +Msg_Name, Send_Count => 3, Reply_Count => 1));

      T.Mem_Set(L0, U(113)); -- target of Fold
      T.Mem_Set(L1, U(4));
      T.Mem_Set(L2, U(8));
      -- local 3 is the tuple (message arguments)

      T.c(0) := Decode_Op_Code("FOLD L3 F0");
      T.c(1) := Decode_Op_Code("SELF_SEND L3 => F1");
      T.c(2) := Decode_Op_Code("REPLY L3");

      T.Step;
      Put_Line("after step 1");
      T.Step;
      Put_Line("after step 2");

      Assert(Fake_Machine.Posted = 27, "message was not posted.");
      Assert(Fake_Machine.Message_Name.all = Msg_Name, "message name error.");
      Assert(kv.avm.actor_pool.Resolve(Fake_Machine.Source) = +T.i, "Source error.");
      Assert(kv.avm.actor_pool.Resolve(Fake_Machine.Destination) = +T.i, "Destination error.");

      T.Step;
      Put_Line("after step 3");
      Assert(T.i.Program_Counter = 0, "processing did not complete normally.");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name (T : Test_26) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test 26: Deferred.");
   end Name;

   procedure Run_Test (T : in out Test_26) is
      use kv.avm.Registers;
      use kv.avm.Assemblers;
   begin
      Put_Line("test 26");
      kv.avm.Log.Verbose := True;
      -- Send a message but have it not go out because it is deferred
      Fake_Machine.Post_Command_Behavior := Fake_Vm.Deferred;
      -- undefer instances
      -- see that the message is sent properly

      kv.avm.actor_pool.Add(Fake_Invoker'ACCESS, Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Invoker(Fake_Invoker_Ref);
      T.i.Get_Frame.Set_Future(13);

      T.Mem_Set(F0, String_To_Tuple_Map("[ L0 L1 L2]"));
      T.Mem_Set(F1, (format => Message_Definition, Message_Name => +Msg_Name, Send_Count => 3, Reply_Count => 1));

      T.Mem_Set(L0, U(113)); -- target of Fold
      T.Mem_Set(L1, U(4));
      T.Mem_Set(L2, U(8));
      -- local 3 is the tuple (message arguments)
      T.Mem_Set(L5, (format => kv.avm.Registers.Actor_Reference, Instance => Fake_Ref));

      T.c(0) := Decode_Op_Code("FOLD L3 F0");
      T.c(1) := Decode_Op_Code("ACTOR_SEND L3 => L5 . F1");
      T.c(2) := Decode_Op_Code("SET L6 := L1 = L2");
      T.c(3) := Decode_Op_Code("ASSERT L6"); -- this will fail if executed (it should not be executed

      T.Step;
      Put_Line("after step 1");
      T.Step;
      Put_Line("after step 2");
      Assert(T.s = Deferred, "Fake machine should have returned deferred status.");
      Assert(T.i.Program_Counter = 1, "PC didn't stay at deferred instruction.");
      T.Step;
      Put_Line("after step 3");
      Assert(T.s = Deferred, "Fake machine should have returned deferred status.");
      Assert(T.i.Program_Counter = 1, "PC didn't stay at deferred instruction.");

      Fake_Machine.Post_Command_Behavior := Fake_Vm.Normal;
      T.Step;
      Put_Line("after step 4");
      Assert(T.s = Active, "Fake machine should have returned active status.");
      Assert(T.i.Program_Counter = 2, "PC didn't advance.");

      T.Step;
      Put_Line("after step 5");
      Assert(T.i.Program_Counter = 3, "processing did not advance normally.");
   end Run_Test;

end kv.avm.Test;
