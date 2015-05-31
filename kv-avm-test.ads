with AUnit;
with AUnit.Simple_Test_Cases;

with kv.avm.Instructions;
with kv.avm.Registers;
with kv.avm.Processors;
with kv.avm.Instances;
with kv.avm.Actors;
with kv.avm.Messages;
with kv.avm.references; use kv.avm.references;
with kv.avm.Memories;
with kv.avm.Control;

package kv.avm.Test is

   type Instruction_Test_Case is abstract new AUnit.Simple_Test_Cases.Test_Case with
      record
         p : aliased kv.avm.Processors.Processor_Type;
         i : kv.avm.Instances.Instance_Access;
         c : kv.avm.Instructions.Code_Access;
         f : kv.avm.Instructions.Code_Access; -- FooBar code
         a : kv.avm.Actors.Actor_Access;
         m : kv.avm.Memories.Memory_Type;
         s : kv.avm.Control.Status_Type;
         x : kv.avm.Messages.Message_Type;
      end record;
   procedure Set_Up (T : in out Instruction_Test_Case);
   procedure Tear_Down (T : in out Instruction_Test_Case);

   procedure Mem_Set
      (T   : in out Instruction_Test_Case;
       Ref : in     reference_type;
       Val : in     kv.avm.Registers.Register_Type);
   function Mem_Get
      (T   : in     Instruction_Test_Case;
       Ref : in     reference_type) return kv.avm.Registers.Register_Type;
   procedure Step(T : in out Instruction_Test_Case);

   type Test_1 is new Instruction_Test_Case with null record;
   function Name (T : Test_1) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_1);

   type Test_2 is new Instruction_Test_Case with null record;
   function Name (T : Test_2) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_2);

   type Test_3 is new Instruction_Test_Case with null record;
   function Name (T : Test_3) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_3);

   type Test_4 is new Instruction_Test_Case with null record;
   function Name (T : Test_4) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_4);

   type Test_4b is new Instruction_Test_Case with null record;
   function Name (T : Test_4b) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_4b);

   type Test_5 is new Instruction_Test_Case with null record;
   function Name (T : Test_5) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_5);

   type Test_6 is new Instruction_Test_Case with null record;
   function Name (T : Test_6) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_6);

   type Test_6b is new Instruction_Test_Case with null record;
   function Name (T : Test_6b) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_6b);

   type Test_7 is new Instruction_Test_Case with null record;
   function Name (T : Test_7) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_7);

   type Test_8 is new Instruction_Test_Case with null record;
   function Name (T : Test_8) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_8);


   type Machine_Test_Case is abstract new AUnit.Simple_Test_Cases.Test_Case with
      record
         null;
      end record;
   procedure Set_Up (T : in out Machine_Test_Case);
   procedure Tear_Down (T : in out Machine_Test_Case);


   type Test_9 is new Machine_Test_Case with null record;
   function Name (T : Test_9) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_9);

   type Test_9b is new Machine_Test_Case with null record;
   function Name (T : Test_9b) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_9b);

   type Test_9c is new Machine_Test_Case with null record;
   function Name (T : Test_9c) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_9c);

   type Test_9d is new Instruction_Test_Case with null record;
   function Name (T : Test_9d) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_9d);

   type Test_10 is new Machine_Test_Case with null record;
   function Name (T : Test_10) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_10);

   type Test_11 is new Instruction_Test_Case with null record;
   function Name (T : Test_11) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_11);

   type Test_12 is new Instruction_Test_Case with null record;
   function Name (T : Test_12) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_12);

   type Test_13 is new Instruction_Test_Case with null record;
   function Name (T : Test_13) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_13);

   type Test_14 is new Instruction_Test_Case with null record;
   function Name (T : Test_14) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_14);

   type Test_15 is new Instruction_Test_Case with null record;
   function Name (T : Test_15) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_15);

   type Test_16 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   function Name (T : Test_16) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_16);

   type Test_17 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   function Name (T : Test_17) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_17);

   type Test_18 is new Instruction_Test_Case with null record;
   function Name (T : Test_18) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_18);

   type Test_19 is new Instruction_Test_Case with null record;
   function Name (T : Test_19) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_19);

   type Test_20 is new Instruction_Test_Case with null record;
   function Name (T : Test_20) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_20);

   type Test_21 is new Instruction_Test_Case with null record;
   function Name (T : Test_21) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_21);

   type Test_22 is new Instruction_Test_Case with null record;
   function Name (T : Test_22) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_22);

   type Test_23 is new Instruction_Test_Case with null record;
   function Name (T : Test_23) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_23);

   type Test_24 is new Instruction_Test_Case with null record;
   function Name (T : Test_24) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_24);

   type Test_25 is new Instruction_Test_Case with null record;
   function Name (T : Test_25) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_25);

   type Test_26 is new Instruction_Test_Case with null record;
   function Name (T : Test_26) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_26);

end kv.avm.Test;
