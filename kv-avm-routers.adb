with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

with kv.avm.Log; use kv.avm.Log;
with kv.avm.Messages;
with kv.avm.Actor_Pool;
with kv.avm.Executables;
with kv.avm.Clients;
with kv.avm.Transactions.Basic;

package body kv.avm.Routers is

   use Interfaces;
   use kv.avm.Control;

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self    : in out Router_Type;
       Machine : in     kv.avm.Control.Control_Access) is
      My_Data : Router_Data_Access := Self.Ref.Get;
   begin
      My_Data.Machine := Machine;
   end Initialize;

   -----------------------------------------------------------------------------
   function Get_Queue_Size(Self : Router_Type) return Natural is
      My_Data : Router_Data_Access := Self.Ref.Get;
   begin
      return Natural(My_Data.Queue.Length);
   end Get_Queue_Size;

   -----------------------------------------------------------------------------
   procedure Set_Queue_Limit(Self : in out Router_Type; Queue_Limit : in Natural) is
      My_Data : Router_Data_Access := Self.Ref.Get;
   begin
      My_Data.Queue_Limit := Queue_Limit;
   end Set_Queue_Limit;

   -----------------------------------------------------------------------------
   procedure Deliver_Local_Message
      (My_Data  : in     Router_Data_Access;
       Message  : in     kv.avm.Messages.Message_Type;
       Instance : in     kv.avm.Executables.Executable_Access;
       Status   :    out kv.avm.Control.Status_Type) is

   begin
      Status := kv.avm.Control.Active;
      --Put_Line("Machine Message Queue: expediting "&Message.Get_Name);
      Instance.Process_Message(Message);
      My_Data.Machine.Activate_Instance(Message.Get_Destination);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Deliver_Local_Message): " & Exception_Information(Error));
         raise;
   end Deliver_Local_Message;

   -----------------------------------------------------------------------------
   function Is_Queue_Too_Full(My_Data : Router_Data_Access) return Boolean is
   begin
      return (Natural(My_Data.Queue.Length) >= My_Data.Queue_Limit);
   end Is_Queue_Too_Full;

   -----------------------------------------------------------------------------
   procedure Queue_Message
      (My_Data  : in     Router_Data_Access;
       Message  : in     kv.avm.Messages.Message_Type;
       Instance : in     kv.avm.Executables.Executable_Access;
       Status   :    out kv.avm.Control.Status_Type) is

   begin
      if Is_Queue_Too_Full(My_Data) then
         -- By refusing to queue the message (and setting the status to Deferred),
         -- we will cause the machine to suspend execution of the actor instance
         -- until such time that it can attempt to post the message again.
         --
         --Put_Line("Machine Message Queue: refusing " & Message.Get_Name);
         Status := kv.avm.Control.Deferred;
      else
         --Put_Line("Machine Message Queue: queueing " & Message.Get_Name);
         My_Data.Queue.Append((Message => Message, Instance => Instance));
         Status := kv.avm.Control.Active;
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Queue_Message): " & Exception_Information(Error));
         raise;
   end Queue_Message;

   -----------------------------------------------------------------------------
   procedure Post_Local_Message
      (My_Data : in     Router_Data_Access;
       Message : in     kv.avm.Messages.Message_Type;
       Status  :    out kv.avm.Control.Status_Type) is

      Instance : kv.avm.Executables.Executable_Access;

   begin
      Instance := kv.avm.Actor_Pool.Resolve(Message.Get_Destination);
      if Instance.Can_Accept_Message_Now(Message) then
         Deliver_Local_Message(My_Data, Message, Instance, Status);
      else
         Queue_Message(My_Data, Message, Instance, Status);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Post_Local_Message): " & Exception_Information(Error));
         raise;
   end Post_Local_Message;

   -----------------------------------------------------------------------------
   procedure Post_Remote_Message
      (My_Data : in     Router_Data_Access;
       Message : in     kv.avm.Messages.Message_Type;
       Status  :    out kv.avm.Control.Status_Type) is

      Destination : kv.avm.Actor_References.Actor_Reference_Type;
      Domain : Interfaces.Unsigned_32;
      Domain_Client : kv.avm.Clients.Client_Access;
      Send_Transaction : kv.avm.Transactions.Basic.Send_Message_Type;

      use kv.avm.Clients;

   begin
      Put_Line("Post_Remote_Message " & Message.Debug);
      --!@#$ find the domain's affiliate, create a transaction, send
      Status := kv.avm.Control.Active;
      Destination := Message.Get_Destination;
      Domain := Destination.Get_Domain;
      Domain_Client := My_Data.Affiliator.Get_Domain_Client(Domain);
      if Domain_Client = null then
         Put_Error("ERROR: Could not find client for" & Destination.Image);
         Status := kv.avm.Control.Error;
         return;
      end if;
      Send_Transaction.Set_Message(Message);
      Domain_Client.Send_Transaction(Send_Transaction);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Post_Remote_Message): " & Exception_Information(Error));
         raise;
   end Post_Remote_Message;

   -----------------------------------------------------------------------------
   procedure Post_Message
      (Self    : in out Router_Type;
       Message : in     kv.avm.Messages.Message_Type;
       Status  :    out kv.avm.Control.Status_Type) is

      My_Data : Router_Data_Access := Self.Ref.Get;

   begin
      if Message.Get_Destination.Is_Local then
         Post_Local_Message(My_Data, Message, Status);
      else
         Post_Remote_Message(My_Data, Message, Status);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Post_Message): " & Exception_Information(Error));
         raise;
   end Post_Message;

   -----------------------------------------------------------------------------
   procedure Post_Local_Response
      (My_Data  : in     Router_Data_Access;
       Reply_To : in     kv.avm.Actor_References.Actor_Reference_Type;
       Answer   : in     kv.avm.Tuples.Tuple_Type;
       Future   : in     Interfaces.Unsigned_32) is

      Instance : kv.avm.Executables.Executable_Access;

   begin
      Instance := kv.avm.actor_pool.Resolve(Reply_To);
      Put_Line("Sending Future=" & Interfaces.Unsigned_32'IMAGE(Future) & " to local instance=" & Instance.Image);
      Instance.Resolve_Future(Answer, Future);
      My_Data.Machine.Activate_Instance(Reply_To);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Post_Local_Response): " & Exception_Information(Error));
         raise;
   end Post_Local_Response;

   -----------------------------------------------------------------------------
   procedure Post_Remote_Response
      (My_Data  : in     Router_Data_Access;
       Reply_To : in     kv.avm.Actor_References.Actor_Reference_Type;
       Answer   : in     kv.avm.Tuples.Tuple_Type;
       Future   : in     Interfaces.Unsigned_32) is

      Domain : Interfaces.Unsigned_32;
      Domain_Client : kv.avm.Clients.Client_Access;
      Response_Transaction : kv.avm.Transactions.Basic.Reply_Transaction_Type;

      use kv.avm.Clients;

   begin
      Put_Line("Sending Future=" & Interfaces.Unsigned_32'IMAGE(Future) & " to remote actor=" & Reply_To.Image);

      Domain := Reply_To.Get_Domain;
      Domain_Client := My_Data.Affiliator.Get_Domain_Client(Domain);
      if Domain_Client = null then
         Put_Error("ERROR: Could not find client for" & Reply_To.Image);
         return;
      end if;
      Response_Transaction.Set_Reply_To(Reply_To);
      Response_Transaction.Set_Data(Answer);
      Response_Transaction.Set_Future(Future);
      Domain_Client.Send_Transaction(Response_Transaction);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Post_Remote_Response): " & Exception_Information(Error));
         raise;
   end Post_Remote_Response;

   -----------------------------------------------------------------------------
   procedure Post_Response
      (Self     : in out Router_Type;
       Reply_To : in     kv.avm.Actor_References.Actor_Reference_Type;
       Answer   : in     kv.avm.Tuples.Tuple_Type;
       Future   : in     Interfaces.Unsigned_32) is

      My_Data : Router_Data_Access := Self.Ref.Get;

      use kv.avm.Actor_References;

   begin
      Put_Line("kv.avm.machine.Post_Response "&Reply_To.Image);
      if Future = kv.avm.control.NO_FUTURE then
         Put_Line("Dropping unwanted response.");
         return;
      end if;
      if Reply_To = kv.avm.Actor_References.Null_Reference then
         Put_Line("Dropping unaddressed response.");
         return;
      end if;
      if Reply_To.Is_Local then
         Post_Local_Response(My_Data, Reply_To, Answer, Future);
      else
         Post_Remote_Response(My_Data, Reply_To, Answer, Future);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Post_Response): " & Exception_Information(Error));
         raise;
   end Post_Response;

   -----------------------------------------------------------------------------
   procedure Deliver_Messages
      (Self : in out Router_Type) is

      My_Data : Router_Data_Access := Self.Ref.Get;
      Current : Message_Queue.Cursor;
      Go_Next : Message_Queue.Cursor;
      Data    : Message_Control_Type;

      use Message_Queue;

   begin
      --!@#$ need to refactor this because it takes much too much time
      --!@#$ idea: keep a map of queues indexed by recipient
      --!@#$ check first Can_Accept_Some_Message_Now and if true try the messages
      --!@#$ else skip rest of messages queued to this instance
      Current := My_Data.Queue.First;
      loop
      exit when Current = No_Element;
         Data := Element(Current);
         --!@#$ Message.Get_Destination.Is_Local
         if Data.Instance.Can_Accept_Message_Now(Data.Message) then
            Put_Line("Machine Message Queue: delivering " & Data.Message.Get_Name & ", leaving " & Natural'IMAGE(Natural(My_Data.Queue.Length)-1) & " in the queue.");
            Data.Instance.Process_Message(Data.Message);
            My_Data.Machine.Activate_Instance(Data.Message.Get_Destination); -- Data.Instance);
            Go_Next := Next(Current);
            My_Data.Queue.Delete(Current);
         else
            Put_Line("Machine Message Queue: holding "&Data.Message.Get_Name);
            Go_Next := Next(Current);
         end if;
         Current := Go_Next;
      end loop;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Deliver_Messages): " & Exception_Information(Error));
         raise;
   end Deliver_Messages;

   -----------------------------------------------------------------------------
   function Reachable_From_Messages(Self : Router_Type) return kv.avm.Actor_References.Sets.Set is

      My_Data : Router_Data_Access := Self.Ref.Get;
      use kv.avm.Actor_References.Sets;
      use Message_Queue;
      Could_Run : Set := Empty_Set;
      Message   : Message_Queue.Cursor;

   begin
      Message := My_Data.Queue.First;
      while Message /= Message_Queue.No_Element loop
         Could_Run.Include(Element(Message).Message.Get_Source);
         Could_Run.Include(Element(Message).Message.Get_Reply_To);
         Could_Run.Include(Element(Message).Message.Get_Destination);
         Could_Run.Union(Element(Message).Message.Reachable);
         Next(Message);
      end loop;
      return Could_Run;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Reachable_From_Messages): " & Exception_Information(Error));
         raise;
   end Reachable_From_Messages;

   -----------------------------------------------------------------------------
   function Get_Affiliator(Self : Router_Type) return kv.avm.Affiliates.Affiliates_Type is
      My_Data : Router_Data_Access := Self.Ref.Get;
   begin
      return My_Data.Affiliator;
   end Get_Affiliator;

end kv.avm.Routers;
