with Interfaces;

with kv.avm.Actor_References;
with kv.avm.Actor_References.Sets;
with kv.avm.Tuples;
with kv.avm.Registers;
with kv.avm.Memories;
with kv.avm.Executables;
with kv.avm.Processors;
with kv.avm.Control;
with kv.avm.Messages;
with kv.avm.Executable_Lists;
with kv.avm.Capabilities;
with kv.avm.Routers;

package kv.avm.Machines is

   Machine_Error : exception;

   type Machine_Type is new kv.avm.Control.Control_Interface with private;
   type Machine_Access is access all Machine_Type;

   procedure Initialize
      (Self      : in out Machine_Type;
       Processor : in     kv.avm.Processors.Processor_Access;
       Factory   : in     kv.avm.Executables.Factory_Access);

   function Get_Router(Self : Machine_Type) return kv.avm.Routers.Router_Type;

   procedure Step
      (Self : in out Machine_Type);

   procedure Deliver_Messages
      (Self : in out Machine_Type);


   function Current_Instance(Self : Machine_Type) return kv.avm.Executables.Executable_Access;

   function Done(Self : Machine_Type) return Boolean;
   function Get_Steps(Self : Machine_Type) return Natural;
   function Get_Total(Self : Machine_Type) return Natural;
   function Get_Active(Self : Machine_Type) return Natural;
   function Get_Idle(Self : Machine_Type) return Natural;
   function Get_Blocked(Self : Machine_Type) return Natural;
   function Get_Deferred(Self : Machine_Type) return Natural;
   function Get_Queue_Size(Self : Machine_Type) return Natural;
   procedure Set_Queue_Limit(Self : in out Machine_Type; Queue_Limit : in Natural);
   function Get_Cycles(Self : Machine_Type) return Natural;
   function Get_Reaped(Self : Machine_Type) return Natural;
   procedure Set_Capabilities(Self : in out Machine_Type; Capabilities : in kv.avm.Capabilities.Capabilities_Type);
   procedure Set_Garbage_Trigger(Self : in out Machine_Type; Garbage_Trigger : in Natural);

   overriding
   procedure New_Actor
      (Self     : in out Machine_Type;
       Name     : in     String;
       Instance :    out kv.avm.Actor_References.Actor_Reference_Type);
   overriding
   procedure Post_Message
      (Self    : in out Machine_Type;
       Message : in     kv.avm.Messages.Message_Type;
       Status  :    out kv.avm.Control.Status_Type);
   overriding
   procedure Post_Response
      (Self         : in out Machine_Type;
       Reply_To     : in     kv.avm.Actor_References.Actor_Reference_Type;
       Answer       : in     kv.avm.Tuples.Tuple_Type;
       Future       : in     Interfaces.Unsigned_32);
   overriding
   procedure Generate_Next_Future
      (Self   : in out Machine_Type;
       Future :    out Interfaces.Unsigned_32);
   overriding
   procedure Trap_To_The_Machine
      (Self   : in out Machine_Type;
       Trap   : in     String;
       Data   : in     kv.avm.Registers.Register_Type;
       Answer :    out kv.avm.Registers.Register_Type;
       Status :    out kv.avm.Control.Status_Type);
   overriding
   procedure Activate_Instance
      (Self     : in out Machine_Type;
       Instance : in     kv.avm.Actor_References.Actor_Reference_Type);

   -- Create an instance of the Actor, sending it an empty CONSTRUCTOR message,
   -- and then sending it Message containing Data.
   --
   procedure Start_With
      (Self    : in out Machine_Type;
       Actor   : in     String;
       Message : in     String;
       Data    : in     kv.avm.Memories.Register_Array_Type);

private

   subtype Executable_State_Type is kv.avm.Control.Status_Type range kv.avm.Control.Active .. kv.avm.Control.Idle;
   type Lists_Type is array (Executable_State_Type) of kv.avm.Executable_Lists.Executable_Holder_Type;

   type Machine_Type is new kv.avm.control.Control_Interface with
      record
         Processor : kv.avm.Processors.Processor_Access;
         Factory   : kv.avm.Executables.Factory_Access;
         Future    : Interfaces.Unsigned_32;
         Lists     : Lists_Type;
         Router    : kv.avm.Routers.Router_Type;
         Cursor    : kv.avm.Executable_Lists.Cursor_Type := 0;
         Capabilities : kv.avm.Capabilities.Capabilities_Type;
         Steps     : Natural;
         Cycles    : Natural;
         Reaped    : Natural;
         Old_Idle  : Natural := 0;
         Garbage_Trigger : Natural := 500;
      end record;


   function Check_For_Beginning_Of_Cycle(Self : Machine_Type) return Boolean;
   function Check_Message_Delivery_Policy(Self : Machine_Type) return Boolean;
   function Check_Undeferral_Policy(Self : Machine_Type) return Boolean;
   function Check_Garbage_Collection_Policy(Self : Machine_Type) return Boolean;

   procedure Beginning_Of_Cycle
      (Self : in out Machine_Type);
   procedure Process_Current_Executable
      (Self : in out Machine_Type);

   procedure Activate_Instance
      (Self     : in out Machine_Type;
       Instance : in     kv.avm.Executables.Executable_Access);

   procedure Undefer
      (Self : in out Machine_Type);

   function Non_Idle(Self : Machine_Type) return kv.avm.Actor_References.Sets.Set;

   function Expand_Reachable_Set
      (Self     : Machine_Type;
       Starting : kv.avm.Actor_References.Sets.Set) return kv.avm.Actor_References.Sets.Set;

   procedure Delete_Unreachable_Executables
      (Self      : in out Machine_Type;
       Reachable : in     kv.avm.Actor_References.Sets.Set);

   procedure Garbage_Collection
      (Self : in out Machine_Type);

end kv.avm.Machines;
