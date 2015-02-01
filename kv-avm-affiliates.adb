with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

with kv.avm.Log; use kv.avm.Log;
with kv.avm.Registers; use kv.avm.Registers;
with kv.avm.Tuples;
with kv.avm.Transactions;

package body kv.avm.Affiliates is

   use Client_Lists;
   use Interfaces;

   AFFILIATE_ADDRESS_KEY : constant String := "affiliate_address";

   ----------------------------------------------------------------------------
   procedure Add_Client
      (My_Data : in     Affiliate_Access;
       Client  : in     kv.avm.Clients.Client_Access) is
   begin
      My_Data.Clients.Append(Client);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Add_Client): " & Exception_Information(Error));
         raise;
   end Add_Client;

   ----------------------------------------------------------------------------
   procedure Initialize_Client_From_Settings
      (My_Data  : in     Affiliate_Access;
       Settings : in     kv.avm.Ini.Settings_Type;
       Client   : in     kv.avm.Clients.Client_Access;
       Index    : in     Positive) is

      Address : kv.avm.Tuples.Tuple_Type;
      Element : access constant kv.avm.Registers.Register_Type;

   begin
      Address := Settings.Lookup_As_Tuple(AFFILIATE_ADDRESS_KEY, Index);
      Element := Address.Peek(0);
      Client.Bind_Address(+Element.The_String);
      Element := Address.Peek(1);
      Client.Bind_Port(Positive(Element.Signed_Value));
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Initialize_Client_From_Settings): " & Exception_Information(Error));
         raise;
   end Initialize_Client_From_Settings;

   ----------------------------------------------------------------------------
   procedure Initialize_Clients
      (My_Data  : in     Affiliate_Access;
       Settings : in     kv.avm.Ini.Settings_Type;
       Factory  : in     kv.avm.Clients.Factory_Access) is

      Client  : kv.avm.Clients.Client_Access;

   begin
      for Client_Index in 1 .. Settings.Value_Count_For_Key(AFFILIATE_ADDRESS_KEY) loop
         Factory.New_Client(Client);
         Initialize_Client_From_Settings(My_Data, Settings, Client, Client_Index);
         Add_Client(My_Data, Client);
      end loop;
   end Initialize_Clients;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self     : in out Affiliates_Type;
       Settings : in     kv.avm.Ini.Settings_Type;
       Server   : in     kv.avm.Servers.Server_Access;
       Factory  : in     kv.avm.Clients.Factory_Access;
       Machine  : in     kv.avm.Control.Control_Access) is

      My_Data : Affiliate_Access := Self.Ref.Get;

   begin
      My_Data.Factory := Factory;
      My_Data.Server  := Server;
      My_Data.Machine := Machine;
      Initialize_Clients(My_Data, Settings, Factory);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Initialize): " & Exception_Information(Error));
         raise;
   end Initialize;

   ----------------------------------------------------------------------------
   function Client_Count(Self : Affiliates_Type) return Natural is
      My_Data : Affiliate_Access := Self.Ref.Get;
   begin
      return Natural(My_Data.Clients.Length);
   end Client_Count;

   ----------------------------------------------------------------------------
   function Get_Client(Self : Affiliates_Type; Index : Positive) return kv.avm.Clients.Client_Access is
      My_Data : Affiliate_Access := Self.Ref.Get;
   begin
      return My_Data.Clients.Element(Index);
   end Get_Client;

   ----------------------------------------------------------------------------
   function Get_Domain_Client(Self : Affiliates_Type; Domain : Interfaces.Unsigned_32) return kv.avm.Clients.Client_Access is

      My_Data : Affiliate_Access := Self.Ref.Get;
      Current : Cursor := My_Data.Clients.First;

   begin
      -- The simplest thing: search the existing vector.
      --TODO: consider deleting the vector and using just a map.
      while Current /= No_Element loop
         if Element(Current).Get_Domain = Domain then
            return Element(Current);
         end if;
         Next(Current);
      end loop;
      return null;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Get_Domain_Client): " & Exception_Information(Error));
         raise;
   end Get_Domain_Client;

   ----------------------------------------------------------------------------
   procedure Open_Clients
      (Self : in out Affiliates_Type) is

      My_Data : Affiliate_Access := Self.Ref.Get;
      Current : Cursor := My_Data.Clients.First;

   begin
      while Current /= No_Element loop
         Element(Current).Open;
         Next(Current);
      end loop;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Open_Clients): " & Exception_Information(Error));
         raise;
   end Open_Clients;

   ----------------------------------------------------------------------------
   procedure Check_Server
      (My_Data : Affiliate_Access) is
   begin
      if My_Data.Server.Is_Connection_Waiting then
         Put_Line("Server adding a new client connection.");
         Add_Client(My_Data, My_Data.Server.Get_Connection);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Check_Server): " & Exception_Information(Error));
         raise;
   end Check_Server;

   ----------------------------------------------------------------------------
   procedure Process_Transaction
      (My_Data : Affiliate_Access;
       Client  : kv.avm.Clients.Client_Access) is

      Transaction : kv.avm.Transactions.Transactions_Access;

   begin
      Put_Line("Processing transaction");
      Transaction := Client.Get_Transaction;
      Transaction.Execute(My_Data.Machine);
      Client.Conclude_Transaction;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Process_Transaction): " & Exception_Information(Error));
         raise;
   end Process_Transaction;

   ----------------------------------------------------------------------------
   procedure Check_Client
      (My_Data : Affiliate_Access;
       Client  : kv.avm.Clients.Client_Access) is
   begin
      if Client.Is_Transaction_Pending then
         Process_Transaction(My_Data, Client);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Check_Client): " & Exception_Information(Error));
         raise;
   end Check_Client;

   ----------------------------------------------------------------------------
   procedure Check_Clients
      (My_Data : Affiliate_Access) is

      Current : Cursor := My_Data.Clients.First;

   begin
      while Current /= No_Element loop
         Check_Client(My_Data, Element(Current));
         Next(Current);
      end loop;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Check_Clients): " & Exception_Information(Error));
         raise;
   end Check_Clients;

   ----------------------------------------------------------------------------
   procedure Periodic_Processing
      (Self : in out Affiliates_Type) is

      My_Data : Affiliate_Access := Self.Ref.Get;

   begin
      Check_Server(My_Data);
      Check_Clients(My_Data);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Periodic_Processing): " & Exception_Information(Error));
         raise;
   end Periodic_Processing;

end kv.avm.Affiliates;
