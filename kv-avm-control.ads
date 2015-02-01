with Interfaces;

with kv.avm.Actor_References;
with kv.avm.Tuples;
with kv.avm.Registers;
with kv.avm.Messages;

package kv.avm.Control is

   NO_FUTURE : constant Interfaces.Unsigned_32 := 0;

   type Status_Type is (Active, Blocked, Deferred, Idle, Error);
   subtype Running_Status_Type is Status_Type range Active .. Deferred;

   type Control_Interface is interface;
   type Control_Access is access all Control_Interface'CLASS;

   procedure New_Actor
      (Self     : in out Control_Interface;
       Name     : in     String;
       Instance :    out kv.avm.Actor_References.Actor_Reference_Type) is abstract;

   procedure Post_Message
      (Self    : in out Control_Interface;
       Message : in     kv.avm.Messages.Message_Type;
       Status  :    out Status_Type) is abstract;

   procedure Post_Response
      (Self         : in out Control_Interface;
       Reply_To     : in     kv.avm.Actor_References.Actor_Reference_Type;
       Answer       : in     kv.avm.Tuples.Tuple_Type;
       Future       : in     Interfaces.Unsigned_32) is abstract;

   procedure Generate_Next_Future
      (Self   : in out Control_Interface;
       Future :    out Interfaces.Unsigned_32) is abstract;

   procedure Trap_To_The_Machine
      (Self   : in out Control_Interface;
       Trap   : in     String;
       Data   : in     kv.avm.Registers.Register_Type;
       Answer :    out kv.avm.Registers.Register_Type;
       Status :    out Status_Type) is abstract;

   procedure Activate_Instance
      (Self     : in out Control_Interface;
       Instance : in     kv.avm.Actor_References.Actor_Reference_Type) is abstract;

end kv.avm.Control;
