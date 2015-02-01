with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Ordered_Maps;

with kv.avm.Log;

package body kv.avm.Capabilities is

   use kv.avm.Log;

   package Resources is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => Capability_Access);

   use Resources;

   type Resource_Access is access Resources.Map;


   type Capabilities_Reference_Counter_Type is
      record
         Count : Natural := 0;
         Data  : Resource_Access;
      end record;

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Capabilities_Reference_Counter_Type, Capabilities_Reference_Counter_Access);
   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Resources.Map, Resource_Access);

   -----------------------------------------------------------------------------
   procedure Initialize (Self : in out Capabilities_Type) is
   begin
      Self.Ref := new Capabilities_Reference_Counter_Type;
      Self.Ref.Count := 1;
      Self.Ref.Data := new Resources.Map;
   end Initialize;

   -----------------------------------------------------------------------------
   procedure Adjust     (Self : in out Capabilities_Type) is
      Ref : Capabilities_Reference_Counter_Access := Self.Ref;
   begin
      if Ref /= null then
         Ref.Count := Ref.Count + 1;
      end if;
   end Adjust;

   -----------------------------------------------------------------------------
   procedure Finalize   (Self : in out Capabilities_Type) is
      Ref : Capabilities_Reference_Counter_Access := Self.Ref;
   begin
      Self.Ref := null;
      if Ref /= null then
         Ref.Count := Ref.Count - 1;
         if Ref.Count = 0 then
            Free(Ref.Data);
            Free(Ref);
         end if;
      end if;
   end Finalize;

   -----------------------------------------------------------------------------
   function Has(Self : Capabilities_Type; Key : String) return Boolean is
   begin
      return Self.Ref.Data.Contains(Key);
   end Has;

   -----------------------------------------------------------------------------
   function Lookup(Self : Capabilities_Type; Key : String) return Capability_Access is
      Location : Cursor;
   begin
      Location := Self.Ref.Data.Find(Key);
      if Location /= No_Element then
         return Element(Location);
      end if;
      return null;
   end Lookup;

   -----------------------------------------------------------------------------
   procedure Add
      (Self  : in out Capabilities_Type;
       Key   : in     String;
       Value : in     Capability_Access) is
   begin
      Self.Ref.Data.Insert(Key, Value);
   end Add;

   -----------------------------------------------------------------------------
   procedure Execute
      (Self    : in     Capabilities_Type;
       Key     : in     String;
       Machine : in out kv.avm.Control.Control_Interface'CLASS;
       Input   : in     kv.avm.Registers.Register_Type;
       Output  :    out kv.avm.Registers.Register_Type;
       Status  :    out kv.avm.Control.Status_Type) is

      Capability : Capability_Access;

   begin
      Capability := Self.Lookup(Key);
      if Capability /= null then
         Capability.Execute(Machine, Input, Output, Status);
      else
         Put_Error("Capabilities_Type.Execute could not find '" & Key & "', returning an Error status.");
         Output := kv.avm.Registers.Make_U(0);
         Status := kv.avm.Control.Error;
      end if;
   end Execute;

end kv.avm.Capabilities;
