with Ada.Finalization;
with Interfaces;

with kv.avm.References;
with kv.avm.Registers;
with kv.avm.Instructions;
limited with kv.avm.Tuples;
with kv.avm.Actor_References.Sets;

package kv.avm.Memories is


   type Register_Set_Type is array (Interfaces.Unsigned_32 range <>) of aliased kv.avm.Registers.Register_Type;
   type Register_Set_Access is access all Register_Set_Type;

   function Reachable(Registers : Register_Set_Access) return kv.avm.Actor_References.Sets.Set;

   type Register_Array_Type is new Ada.Finalization.Controlled with private;

   overriding procedure Initialize (Self : in out Register_Array_Type);
   overriding procedure Adjust     (Self : in out Register_Array_Type);
   overriding procedure Finalize   (Self : in out Register_Array_Type);


   procedure Initialize(Self : in out Register_Array_Type; Data : Register_Set_Type);
   procedure Initialize(Self : in out Register_Array_Type; Tuple : kv.avm.Tuples.Tuple_Type);
   function Is_Set(Self : Register_Array_Type) return Boolean;
   function Get(Self : Register_Array_Type) return Register_Set_Access;
   procedure Set(Self : in out Register_Array_Type; Registers : in Register_Set_Access);
   procedure Write(Self : in out Register_Array_Type; Where : kv.avm.References.Offset_Type; Data : kv.avm.Registers.Register_Type);
   function Read(Self : Register_Array_Type; Where : kv.avm.References.Offset_Type) return kv.avm.Registers.Register_Type;
   procedure Allocate(Self : in out Register_Array_Type; Count : in Positive);
   procedure Deallocate(Self : in out Register_Array_Type);

   procedure Find_Future
      (Self     : in     Register_Array_Type;
       Future   : in     Interfaces.Unsigned_32;
       Found    :    out Boolean;
       Location :    out kv.avm.References.Offset_Type);

   function Reachable(Self : Register_Array_Type) return kv.avm.Actor_References.Sets.Set;


   type Memory_Type is tagged private;

   procedure Set(Self : in out Memory_Type; Bank : kv.avm.references.Register_Bank_Type; Registers : Register_Array_Type'CLASS);
   function Get(Self : Memory_Type; Bank : kv.avm.references.Register_Bank_Type) return Register_Array_Type'CLASS;
   procedure Write(Self : in out Memory_Type; Where : kv.avm.References.Reference_Type; Data : kv.avm.Registers.Register_Type);
   function Read(Self : Memory_Type; Where : kv.avm.References.Reference_Type) return kv.avm.Registers.Register_Type;

   procedure Find_Future
      (Self     : in     Memory_Type;
       Bank     : in     kv.avm.References.Register_Bank_Type;
       Future   : in     Interfaces.Unsigned_32;
       Found    :    out Boolean;
       Location :    out kv.avm.References.Reference_Type);

   function Reachable(Self : Memory_Type; Bank : kv.avm.References.Register_Bank_Type) return kv.avm.Actor_References.Sets.Set;

   procedure Deallocate(Self : in out Memory_Type);


   function To_String(Registers : Register_Set_Type) return String;

   function Instruction_Image(Inst : kv.avm.Instructions.Instruction_Type; M : Memory_Type) return String;

private

   type Register_Array_Reference_Counter_Type;
   type Register_Array_Reference_Counter_Access is access all Register_Array_Reference_Counter_Type;

   type Register_Array_Type is new Ada.Finalization.Controlled with
      record
         Ref : Register_Array_Reference_Counter_Access;
      end record;

   type Memory_Set_Type is array (kv.avm.References.Register_Bank_Type) of Register_Array_Type;
   type Memory_Type is tagged
      record
         vq : aliased Memory_Set_Type; --!@#$ "vq" is simply a short unique sequence for use in a wide refactoring.  It will be gone when done.
      end record;

end kv.avm.Memories;
