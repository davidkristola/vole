with Ada.Finalization;

with kv.avm.Control;
limited with kv.avm.Instances;

package kv.avm.Warrants is

   type Warrant_Type is new Ada.Finalization.Controlled with private;

   overriding procedure Initialize (Self : in out Warrant_Type);
   overriding procedure Adjust     (Self : in out Warrant_Type);
   overriding procedure Finalize   (Self : in out Warrant_Type);
   procedure Initialize
      (Self     : in out Warrant_Type;
       Machine  : in     kv.avm.Control.Control_Access;
       Instance : access kv.avm.Instances.Instance_Type);

private

   type Warrant_Type is new Ada.Finalization.Controlled with
      record
         Machine  : kv.avm.Control.Control_Access;
         Instance : access kv.avm.Instances.Instance_Type;
      end record;

end kv.avm.Warrants;
