with Ada.Streams;
with Ada.Strings.Unbounded;
with Interfaces;

with kv.avm.Actor_References;
with kv.avm.Tuples;
with kv.avm.Messages;

package kv.avm.Transactions.Basic is

   -- Responsibility: represent an inter-VM transaction to close the transporting connection.
   type Close_Connections_Type is new Transactions_Interface with private;
   type Close_Connections_Access is access all Close_Connections_Type;

   overriding
   procedure Initialize
      (Self : in out Close_Connections_Type);

   overriding
   function Equals
      (Left  : Close_Connections_Type;
       Right : Transactions_Interface'CLASS) return Boolean;

   overriding
   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Close_Connections_Type;

   overriding
   function New_Copy(Self : Close_Connections_Type) return Transactions_Access;



   -- Responsibility: represent an inter-VM transaction to register an actor instance in the other VM's broker.
   type Register_Instance_Type is new Transactions_Interface with private;
   type Register_Instance_Access is access all Register_Instance_Type;

   overriding
   procedure Initialize
      (Self : in out Register_Instance_Type);

   overriding
   function Equals
      (Left  : Register_Instance_Type;
       Right : Transactions_Interface'CLASS) return Boolean;

   overriding
   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Register_Instance_Type;

   overriding
   function New_Copy(Self : Register_Instance_Type) return Transactions_Access;

   procedure Set_Name(Self : in out Register_Instance_Type; Name : String);
   procedure Set_Instance(Self : in out Register_Instance_Type; Instance : kv.avm.Actor_References.Actor_Reference_Type);
   function Get_Name(Self : Register_Instance_Type) return String;
   function Get_Instance(Self : Register_Instance_Type) return kv.avm.Actor_References.Actor_Reference_Type;




   -- Responsibility: represent an inter-VM transaction to send a message to an actor instance in the other VM.
   type Send_Message_Type is new Transactions_Interface with private;
   type Send_Message_Access is access all Send_Message_Type;

   overriding
   procedure Initialize
      (Self : in out Send_Message_Type);

   overriding
   function Equals
      (Left  : Send_Message_Type;
       Right : Transactions_Interface'CLASS) return Boolean;

   overriding
   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Send_Message_Type;

   overriding
   function New_Copy(Self : Send_Message_Type) return Transactions_Access;

   overriding
   procedure Execute
      (Self    : in out Send_Message_Type;
       Machine : in     kv.avm.Control.Control_Access);

   procedure Set_Message(Self : in out Send_Message_Type; Message : kv.avm.Messages.Message_Type);
   function Get_Message(Self : Send_Message_Type) return kv.avm.Messages.Message_Type;






   -- Responsibility: represent an inter-VM transaction to reply to a message from an instance on the other VM.
   type Reply_Transaction_Type is new Transactions_Interface with private;
   type Reply_Transaction_Access is access all Reply_Transaction_Type;

   overriding
   procedure Initialize
      (Self : in out Reply_Transaction_Type);

   overriding
   function Equals
      (Left  : Reply_Transaction_Type;
       Right : Transactions_Interface'CLASS) return Boolean;

   overriding
   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Reply_Transaction_Type;

   overriding
   function New_Copy(Self : Reply_Transaction_Type) return Transactions_Access;

   overriding
   procedure Execute
      (Self    : in out Reply_Transaction_Type;
       Machine : in     kv.avm.Control.Control_Access);

   procedure Set_Reply_To(Self : in out Reply_Transaction_Type; Instance : kv.avm.Actor_References.Actor_Reference_Type);
   function Get_Reply_To(Self : Reply_Transaction_Type) return kv.avm.Actor_References.Actor_Reference_Type;
   procedure Set_Data(Self : in out Reply_Transaction_Type; Data : kv.avm.Tuples.Tuple_Type);
   function Get_Data(Self : Reply_Transaction_Type) return kv.avm.Tuples.Tuple_Type;
   procedure Set_Future(Self : in out Reply_Transaction_Type; Future : Interfaces.Unsigned_32);
   function Get_Future(Self : Reply_Transaction_Type) return Interfaces.Unsigned_32;


private

   type Close_Connections_Type is new Transactions_Interface with
      record
         Sequence_Number : Natural := 0;
      end record;

   type Register_Instance_Type is new Transactions_Interface with
      record
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Instance : kv.avm.Actor_References.Actor_Reference_Type := kv.avm.Actor_References.Null_Reference;
      end record;

   type Send_Message_Type is new Transactions_Interface with
      record
         Message : kv.avm.Messages.Message_Type;
      end record;

   type Reply_Transaction_Type is new Transactions_Interface with
      record
         Reply_To : kv.avm.Actor_References.Actor_Reference_Type := kv.avm.Actor_References.Null_Reference;
         Data     : kv.avm.Tuples.Tuple_Type;
         Future   : Interfaces.Unsigned_32;
      end record;

end kv.avm.Transactions.Basic;
