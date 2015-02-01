with Ada.Tags;

with kv.avm.Control;

package body kv.avm.Transactions.Basic is

   Sequence_Number : Natural := 0;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Close_Connections_Type) is
   begin
      Sequence_Number := Sequence_Number + 1;
      Self.Sequence_Number := Sequence_Number;
   end Initialize;

   ----------------------------------------------------------------------------
   function Equals
      (Left  : Close_Connections_Type;
       Right : Transactions_Interface'CLASS) return Boolean is
      use Ada.Tags;
   begin
      if Right not in Close_Connections_Type then
         return False;
      end if;
      return Left.Sequence_Number = Close_Connections_Type(Right).Sequence_Number;
   end Equals;

   ----------------------------------------------------------------------------
   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Close_Connections_Type is
      Answer : Close_Connections_Type;
   begin
      return Answer;
   end Constructor;

   ----------------------------------------------------------------------------
   function New_Copy(Self : Close_Connections_Type) return Transactions_Access is
      Copy : Close_Connections_Access;
   begin
      Copy := new Close_Connections_Type'(Self);
      return Transactions_Access(Copy);
   end New_Copy;



   use Ada.Strings.Unbounded;
   use kv.avm.Actor_References;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Register_Instance_Type) is
   begin
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   function Equals
      (Left  : Register_Instance_Type;
       Right : Transactions_Interface'CLASS) return Boolean is
      use Ada.Tags;
   begin
      if Right not in Register_Instance_Type then
         return False;
      end if;
      return Left.Name = Register_Instance_Type(Right).Name and then
             Left.Instance = Register_Instance_Type(Right).Instance;
   end Equals;

   ----------------------------------------------------------------------------
   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Register_Instance_Type is
      Answer : Register_Instance_Type;
   begin
      return Answer;
   end Constructor;

   ----------------------------------------------------------------------------
   function New_Copy(Self : Register_Instance_Type) return Transactions_Access is
      Copy : Register_Instance_Access;
   begin
      Copy := new Register_Instance_Type'(Self);
      return Transactions_Access(Copy);
   end New_Copy;

   ----------------------------------------------------------------------------
   procedure Set_Name(Self : in out Register_Instance_Type; Name : String) is
   begin
      Self.Name := To_Unbounded_String(Name);
   end Set_Name;

   ----------------------------------------------------------------------------
   procedure Set_Instance(Self : in out Register_Instance_Type; Instance : kv.avm.Actor_References.Actor_Reference_Type) is
   begin
      Self.Instance := Instance;
   end Set_Instance;

   ----------------------------------------------------------------------------
   function Get_Name(Self : Register_Instance_Type) return String is
   begin
      return To_String(Self.Name);
   end Get_Name;

   ----------------------------------------------------------------------------
   function Get_Instance(Self : Register_Instance_Type) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Instance;
   end Get_Instance;





   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Send_Message_Type) is
   begin
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   function Equals
      (Left  : Send_Message_Type;
       Right : Transactions_Interface'CLASS) return Boolean is
      use Ada.Tags;
   begin
      if Right not in Send_Message_Type then
         return False;
      end if;
      return Left = Send_Message_Type(Right);
   end Equals;

   ----------------------------------------------------------------------------
   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Send_Message_Type is
      Answer : Send_Message_Type;
   begin
      return Answer;
   end Constructor;

   ----------------------------------------------------------------------------
   function New_Copy(Self : Send_Message_Type) return Transactions_Access is
      Copy : Send_Message_Access;
   begin
      Copy := new Send_Message_Type'(Self);
      return Transactions_Access(Copy);
   end New_Copy;

   ----------------------------------------------------------------------------
   procedure Execute
      (Self    : in out Send_Message_Type;
       Machine : in     kv.avm.Control.Control_Access) is
      Status : kv.avm.Control.Status_Type;
   begin
      Machine.Post_Message(Self.Message, Status);
   end Execute;

   ----------------------------------------------------------------------------
   procedure Set_Message(Self : in out Send_Message_Type; Message : kv.avm.Messages.Message_Type) is
   begin
      Self.Message := Message;
   end Set_Message;

   ----------------------------------------------------------------------------
   function Get_Message(Self : Send_Message_Type) return kv.avm.Messages.Message_Type is
   begin
      return Self.Message;
   end Get_Message;






   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Reply_Transaction_Type) is
   begin
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   function Equals
      (Left  : Reply_Transaction_Type;
       Right : Transactions_Interface'CLASS) return Boolean is
      use Ada.Tags;
   begin
      if Right not in Reply_Transaction_Type then
         return False;
      end if;
      return Left = Reply_Transaction_Type(Right);
   end Equals;

   ----------------------------------------------------------------------------
   function Constructor
      (Info : not null access Obligatory_Parameter_Type) return Reply_Transaction_Type is
      Answer : Reply_Transaction_Type;
   begin
      Answer.Future := kv.avm.Control.NO_FUTURE;
      return Answer;
   end Constructor;

   ----------------------------------------------------------------------------
   function New_Copy(Self : Reply_Transaction_Type) return Transactions_Access is
      Copy : Reply_Transaction_Access;
   begin
      Copy := new Reply_Transaction_Type'(Self);
      return Transactions_Access(Copy);
   end New_Copy;

   ----------------------------------------------------------------------------
   procedure Execute
      (Self    : in out Reply_Transaction_Type;
       Machine : in     kv.avm.Control.Control_Access) is
   begin
      Machine.Post_Response(Self.Reply_To, Self.Data, Self.Future);
   end Execute;

   ----------------------------------------------------------------------------
   procedure Set_Reply_To(Self : in out Reply_Transaction_Type; Instance : kv.avm.Actor_References.Actor_Reference_Type) is
   begin
      Self.Reply_To := Instance;
   end Set_Reply_To;

   ----------------------------------------------------------------------------
   function Get_Reply_To(Self : Reply_Transaction_Type) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Reply_To;
   end Get_Reply_To;

   ----------------------------------------------------------------------------
   procedure Set_Data(Self : in out Reply_Transaction_Type; Data : kv.avm.Tuples.Tuple_Type) is
   begin
      Self.Data := Data;
   end Set_Data;

   ----------------------------------------------------------------------------
   function Get_Data(Self : Reply_Transaction_Type) return kv.avm.Tuples.Tuple_Type is
   begin
      return Self.Data;
   end Get_Data;

   ----------------------------------------------------------------------------
   procedure Set_Future(Self : in out Reply_Transaction_Type; Future : Interfaces.Unsigned_32) is
   begin
      Self.Future := Future;
   end Set_Future;

   ----------------------------------------------------------------------------
   function Get_Future(Self : Reply_Transaction_Type) return Interfaces.Unsigned_32 is
   begin
      return Self.Future;
   end Get_Future;


end kv.avm.Transactions.Basic;
