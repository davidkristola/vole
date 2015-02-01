with Interfaces;
with Ada.Finalization;
with Ada.Streams;

with kv.avm.references; use kv.avm.references;
limited with kv.avm.Memories;
limited with kv.avm.Registers;
with kv.avm.Actor_References.Sets;

package kv.avm.Tuples is

   use Interfaces;

   Immutability_Error : exception;

   -- A (constant) register map used to create a tuple
   type Map_Type is new Ada.Finalization.Controlled with private;

   overriding
   procedure Adjust
      (Self : in out Map_Type);
   overriding
   procedure Finalize
      (Self : in out Map_Type);
   not overriding
   procedure Set
      (Self : in out Map_Type;
       Data : access constant kv.avm.References.Reference_Array_Type);
   not overriding
   function Get(Self : Map_Type) return access constant kv.avm.References.Reference_Array_Type;

   procedure Tuple_Map_Write(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : in Map_Type);
   for Map_Type'WRITE use Tuple_Map_Write;
   procedure Tuple_Map_Read(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : out Map_Type);
   for Map_Type'READ use Tuple_Map_Read;

   overriding
   function "="(L, R: Map_Type) return Boolean;



   type Tuple_Type is new Ada.Finalization.Controlled with private;

   overriding
   procedure Initialize
      (Self : in out Tuple_Type);
   overriding
   procedure Adjust
      (Self : in out Tuple_Type);
   overriding
   procedure Finalize
      (Self : in out Tuple_Type);

   not overriding
   procedure Fold
      (Self : in out Tuple_Type;
       Data : in     kv.avm.Memories.Register_Array_Type);
   not overriding
   procedure Fold_Empty
      (Self : in out Tuple_Type);
   not overriding
   procedure Fold_One
      (Self : in out Tuple_Type;
       Data : access kv.avm.Registers.Register_Type);
   not overriding
   procedure Fold
      (Self : in out Tuple_Type;
       Data : in     kv.avm.Memories.Memory_Type;
       Map  : in     Map_Type'CLASS);

   not overriding
   function Peek
      (Self  : in     Tuple_Type;
       Index : in     Interfaces.Unsigned_32) return access constant kv.avm.Registers.Register_Type;

   not overriding
   function Unfolded(Self : Tuple_Type) return access constant kv.avm.Memories.Register_Set_Type;

   not overriding
   function To_String(Self : Tuple_Type) return String;

   not overriding
   function Reachable(Self : Tuple_Type) return kv.avm.Actor_References.Sets.Set;

   not overriding
   function Length(Self : Tuple_Type) return Natural;

   overriding
   function "="(L, R: Tuple_Type) return Boolean;

   procedure Tuple_Write(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : in Tuple_Type);
   for Tuple_Type'WRITE use Tuple_Write;
   procedure Tuple_Read(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : out Tuple_Type);
   for Tuple_Type'READ use Tuple_Read;




   -- A (constant) abstract ("definition" because "abstract" is a reserved word)
   -- of a tuple.
   type Definition_Type is new Ada.Finalization.Controlled with private;

   overriding
   procedure Initialize
      (Self : in out Definition_Type);
   overriding
   procedure Adjust
      (Self : in out Definition_Type);
   overriding
   procedure Finalize
      (Self : in out Definition_Type);

   overriding
   function "="(L, R: Definition_Type) return Boolean;

   not overriding
   procedure Make
      (Self  : in out Definition_Type;
       Tuple : in     Tuple_Type'CLASS);

   not overriding
   function To_String(Self : Definition_Type) return String;

   not overriding
   function Length(Self : Definition_Type) return Natural;


private

   type Tuple_Reference_Counter_Type;
   type Tuple_Reference_Counter_Access is access all Tuple_Reference_Counter_Type;

   type Tuple_Type is new Ada.Finalization.Controlled with
      record
         Ref : Tuple_Reference_Counter_Access;
      end record;


   type Map_Reference_Counter_Type;
   type Map_Reference_Counter_Access is access all Map_Reference_Counter_Type;

   type Map_Type is new Ada.Finalization.Controlled with
      record
         Ref : Map_Reference_Counter_Access;
      end record;

   type Definition_Reference_Counter_Type;
   type Definition_Reference_Counter_Access is access all Definition_Reference_Counter_Type;

   type Definition_Type is new Ada.Finalization.Controlled with
      record
         Ref : Definition_Reference_Counter_Access;
      end record;

end kv.avm.Tuples;
