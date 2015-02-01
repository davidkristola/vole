with kv.avm.Actor_References;
with kv.avm.Control;

package kv.avm.Services is

   -- Machines have a collection of services that will send messages
   -- to their destination when the time is right.  The messages are
   -- service dependent, as is the timing.
   --
   -- Since services may monitor asynchronous events, they may involve
   -- different threads of execution.  This must be managed in their
   -- Execute method which is called from the Machine's thread.  If
   -- a message is to be sent, it must be sent from this call tree.

   type Service_Interface is interface;
   type Service_Access is access Service_Interface'CLASS;

   procedure Set_Destination
      (Self        : in out Service_Interface;
       Destination : in     kv.avm.Actor_References.Actor_Reference_Type) is abstract;

   procedure Set_Machine
      (Self    : in out Service_Interface;
       Machine : in     kv.avm.Control.Control_Access) is abstract;

   procedure Execute
      (Self : in out Service_Interface) is abstract;

   function Get_Destination(Self : Service_Interface) return kv.avm.Actor_References.Actor_Reference_Type is abstract;


   procedure Foo;

end kv.avm.Services;
