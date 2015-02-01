with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;


package body kv.avm.Actor_Pool is

   use kv.avm.Executables;

   package Actors is new Ada.Containers.Ordered_Maps
      (Key_Type     => Interfaces.Unsigned_32,
       Element_Type => kv.avm.Executables.Executable_Access ); -- kv.avm.Instances.Instance_Access );

   Pool : Actors.Map;
   Count : Interfaces.Unsigned_32 := 0;

   -----------------------------------------------------------------------------
   procedure Add
      (Actor     : in     kv.avm.Executables.Executable_Access;
       Reference :    out kv.avm.Actor_References.Actor_Reference_Type) is
   begin
      Count := Count + 1;
      Pool.Insert(Count, Actor);
      Reference.Initialize(Count);
   end Add;


   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(kv.avm.Executables.Executable_Interface'CLASS, kv.avm.Executables.Executable_Access);


   -----------------------------------------------------------------------------
   procedure Delete
      (Reference : in     kv.avm.Actor_References.Actor_Reference_Type) is

      Location : Actors.Cursor;
      Instance : kv.avm.Executables.Executable_Access;
      use Actors;

   begin
      Location := Pool.Find(Reference.Get_Key);
      if Location /= Actors.No_Element then
         Instance := Actors.Element(Location);
         Pool.Delete(Location);
         Free(Instance);
      end if;
   end Delete;


   -----------------------------------------------------------------------------
   function Resolve(Reference : kv.avm.Actor_References.Actor_Reference_Type) return kv.avm.Executables.Executable_Access is

      Location : Actors.Cursor;
      use Actors;

   begin
      Location := Pool.Find(Reference.Get_Key);
      if Location = Actors.No_Element then
         return null;
      else
         return Actors.Element(Location);
      end if;
   end Resolve;

   -----------------------------------------------------------------------------
   procedure Empty_Actor_Pool is
   begin
      Pool.Clear;
   end Empty_Actor_Pool;

end kv.avm.Actor_Pool;
