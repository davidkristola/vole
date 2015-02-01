
with kv.avm.Executables;
with kv.avm.Actor_References;

package kv.avm.Actor_Pool is

   procedure Add
      (Actor     : in     kv.avm.Executables.Executable_Access;
       Reference :    out kv.avm.Actor_References.Actor_Reference_Type);

   procedure Delete
      (Reference : in     kv.avm.Actor_References.Actor_Reference_Type);

   function Resolve(Reference : kv.avm.Actor_References.Actor_Reference_Type) return kv.avm.Executables.Executable_Access;

   procedure Empty_Actor_Pool;

end kv.avm.Actor_Pool;
