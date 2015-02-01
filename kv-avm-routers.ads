with Ada.Containers.Doubly_Linked_Lists;
with Interfaces;

with kv.Ref_Counting_Mixin;
with kv.avm.Control;
with kv.avm.Messages;
with kv.avm.Executables;
with kv.avm.Actor_References;
with kv.avm.Tuples;
with kv.avm.Actor_References.Sets;
with kv.avm.Affiliates;

package kv.avm.Routers is

   type Router_Type is tagged private;

   procedure Initialize
      (Self    : in out Router_Type;
       Machine : in     kv.avm.Control.Control_Access);

   function Get_Queue_Size(Self : Router_Type) return Natural;

   procedure Set_Queue_Limit(Self : in out Router_Type; Queue_Limit : in Natural);

   procedure Post_Message
      (Self    : in out Router_Type;
       Message : in     kv.avm.Messages.Message_Type;
       Status  :    out kv.avm.Control.Status_Type);

   procedure Post_Response
      (Self     : in out Router_Type;
       Reply_To : in     kv.avm.Actor_References.Actor_Reference_Type;
       Answer   : in     kv.avm.Tuples.Tuple_Type;
       Future   : in     Interfaces.Unsigned_32);

   procedure Deliver_Messages
      (Self : in out Router_Type);

   function Reachable_From_Messages(Self : Router_Type) return kv.avm.Actor_References.Sets.Set;

   function Get_Affiliator(Self : Router_Type) return kv.avm.Affiliates.Affiliates_Type;

private

   type Message_Control_Type is
      record
         Message  : kv.avm.Messages.Message_Type;
         Instance : kv.avm.Executables.Executable_Access;
      end record;
   package Message_Queue is new Ada.Containers.Doubly_Linked_Lists(Message_Control_Type);

   type Router_Data_Type is
      record
         Machine     : kv.avm.Control.Control_Access;
         Queue       : Message_Queue.List;
         Queue_Limit : Natural := 200;
         Affiliator  : kv.avm.Affiliates.Affiliates_Type;
      end record;

   type Router_Data_Access is access Router_Data_Type;

   package Ref_Count is new kv.Ref_Counting_Mixin(Router_Data_Type, Router_Data_Access);

   type Router_Type is tagged
      record
         Ref : Ref_Count.Ref_Type;
      end record;

end kv.avm.Routers;
