with kv.avm.Instances;

package body kv.avm.Warrants is

   -----------------------------------------------------------------------------
   procedure Initialize (Self : in out Warrant_Type) is
   begin
      null;
   end Initialize;

   -----------------------------------------------------------------------------
   procedure Adjust     (Self : in out Warrant_Type) is
   begin
      null;
   end Adjust;

   -----------------------------------------------------------------------------
   procedure Finalize   (Self : in out Warrant_Type) is
   begin
      null;
   end Finalize;

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self     : in out Warrant_Type;
       Machine  : in     kv.avm.Control.Control_Access;
       Instance : access kv.avm.Instances.Instance_Type) is
   begin
      Self.Machine := Machine;
      Self.Instance := Instance;
   end Initialize;

end kv.avm.Warrants;
