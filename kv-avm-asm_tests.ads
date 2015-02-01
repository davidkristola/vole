with AUnit;
with AUnit.Simple_Test_Cases;

with kv.avm.Assemblers;
with kv.avm.Instances;
with kv.avm.Machines;
with kv.avm.Memories;
with kv.avm.Processors;

package kv.avm.Asm_Tests is

   type Test_A1 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A1) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A1);

   type Test_A2 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A2) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A2);

   type Test_A3 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A3) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A3);

   type Test_A4 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A4) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A4);

   type Test_A5 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A5) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A5);

   type Test_A6 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A6) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A6);

   type Test_A7 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A7) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A7);

   type Test_A8 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A8) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A8);

   type Test_A9 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A9) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A9);

   type Test_A10 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A10) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A10);

   type Superclass_Test_Case is abstract new AUnit.Simple_Test_Cases.Test_Case with
      record
         Builder : aliased kv.avm.Assemblers.Assembler_Type;
         --VM  : aliased kv.avm.Machines.Machine_Type; -- This leads to uninitialized attribute of VM
         CPU : aliased kv.avm.Processors.Processor_Type;
         Instance_Factory : aliased kv.avm.Instances.Instance_Factory;
         Registers : aliased kv.avm.Memories.Register_Array_Type;
      end record;
   overriding
   procedure Set_Up (T : in out Superclass_Test_Case);
   overriding
   procedure Tear_Down (T : in out Superclass_Test_Case);
   not overriding
   procedure Load (T : in out Superclass_Test_Case; Volea : in String);
   not overriding
   procedure Prep_VM (T : in out Superclass_Test_Case; VM : access kv.avm.Machines.Machine_Type; Start_Actor : in String; Start_Message : in String);
   not overriding
   procedure Run_VM (T : in out Superclass_Test_Case; VM : access kv.avm.Machines.Machine_Type; Steps : in Positive);

   type Test_A11 is new Superclass_Test_Case with null record;
   overriding
   function Name (T : Test_A11) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A11);

   type Test_A12 is new Superclass_Test_Case with null record;
   overriding
   function Name (T : Test_A12) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A12);

   type Test_A13 is new Superclass_Test_Case with null record;
   overriding
   function Name (T : Test_A13) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A13);

   type Test_A14 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A14) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A14);

   type Test_A15 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A15) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A15);

   type Test_A16 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A16) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A16);

   type Test_A17 is new Superclass_Test_Case with null record;
   overriding
   function Name (T : Test_A17) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A17);

   type Test_A18 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A18) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A18);

   type Test_A19 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A19) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A19);

   type Test_A20 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   overriding
   function Name (T : Test_A20) return AUnit.Message_String;
   overriding
   procedure Run_Test (T : in out Test_A20);

end kv.avm.Asm_Tests;
