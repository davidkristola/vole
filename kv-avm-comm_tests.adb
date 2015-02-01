with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Interfaces;

with AUnit.Assertions; use AUnit.Assertions;

with kv.avm.Ini;
with kv.avm.Capabilities;
with kv.avm.Control;
with kv.avm.Registers;
with kv.avm.Machines;
with kv.avm.Services;
with kv.avm.Affiliates;
with kv.avm.Clients;
with kv.avm.Servers;
with kv.avm.Tuples;
with kv.avm.Transactions;
with kv.avm.Brokers;
with kv.avm.Actor_References;
with kv.avm.Memories;
with kv.avm.Log;
with kv.avm.Transactions.Basic;
with kv.avm.Messages;
with kv.avm.Processors;
with kv.avm.Instances;
with kv.avm.Test.Runners;
with kv.avm.Routers;
with kv.avm.Actors;

package body kv.avm.Comm_Tests is

   use Interfaces;

   package SIO renames Ada.Streams.Stream_IO;

   -----------------------------------------------------------------------------
   procedure Set_Up (T : in out Comm_Test_Case) is
   begin
      kv.avm.Log.Verbose := False;
   end Set_Up;

   -----------------------------------------------------------------------------
   procedure Tear_Down (T : in out Comm_Test_Case) is
   begin
      null;
   end Tear_Down;



   -----------------------------------------------------------------------------
   function Name(T : Test_01) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 01: ini.");
   end Name;

   procedure Run_Test(T : in out Test_01) is
      Settings : kv.avm.Ini.Settings_Type;
      Tuple : kv.avm.Tuples.Tuple_Type;
      Element : access constant kv.avm.Registers.Register_Type;
      use kv.avm.Registers;
   begin
      Put_Line("test 01");
      Settings.Parse_Line("#water = liquid");
      Settings.Parse_Line("port = 13");
      Settings.Parse_Line("pi = 3.14");
      Assert(not Settings.Has("water"), "invalid key error");
      Assert(Settings.Has("port"), "valid key error(port)");
      Assert(Settings.Has("pi"), "valid key error(pi)");
      Assert(Settings.Lookup_As_String("port") = "13", "string lookup error");
      Assert(Settings.Lookup_As_Integer("port") = 13, "integer lookup error");
      Assert(Settings.Lookup_As_String("pi") = "3.14", "string lookup error(pi)");
      Assert(Settings.Value_Count_For_Key("pi") = 1, "wrong value count for pi");
      Settings.Parse_Line("affiliate_address = [""localhost"", 29678]");
      Settings.Parse_Line("affiliate_address = [""197.87.37.7"", 29678]");
      Assert(Settings.Value_Count_For_Key("affiliate_address") = 2, "wrong value count for affiliate_address");
      Assert(Settings.Lookup_As_String("affiliate_address", 2) = "[""197.87.37.7"", 29678]", "indexed string lookup error");
      Put_Line("test 01 -- Lookup_As_Tuple");
      Tuple := Settings.Lookup_As_Tuple("affiliate_address", 2);
      Put_Line("test 01 -- Peek");
      Element := Tuple.Peek(0);
      Assert(Element.The_String = "197.87.37.7", "wrong IP address (got '" & (+Element.The_String) & "', expexted '197.87.37.7'");
      Element := Tuple.Peek(1);
      Assert(Element.all = Make_S(29678), "tuple port lookup error");
   end Run_Test;


   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   package Test_Cap is
      type Fake_Cap_Type is new kv.avm.Capabilities.Capability_Interface with
         record
            Called : Boolean := False;
         end record;
      overriding procedure Execute
         (Self    : in out Fake_Cap_Type;
          Machine : in out kv.avm.Control.Control_Interface'CLASS;
          Input   : in     kv.avm.Registers.Register_Type;
          Output  :    out kv.avm.Registers.Register_Type;
          Status  :    out kv.avm.Control.Status_Type);
   end Test_Cap;
   package body Test_Cap is
      procedure Execute
         (Self    : in out Fake_Cap_Type;
          Machine : in out kv.avm.Control.Control_Interface'CLASS;
          Input   : in     kv.avm.Registers.Register_Type;
          Output  :    out kv.avm.Registers.Register_Type;
          Status  :    out kv.avm.Control.Status_Type) is
      begin
         Self.Called := True;
         Output := kv.avm.Registers.Make_U(1397);
         Status := kv.avm.Control.Active;
      end Execute;
   end Test_Cap;


   -----------------------------------------------------------------------------
   function Name(T : Test_02) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 02: capabilities.");
   end Name;

   procedure Run_Test(T : in out Test_02) is
      Cap_Pool : kv.avm.Capabilities.Capabilities_Type;
      Cap : aliased Test_Cap.Fake_Cap_Type;
      Reg_In : kv.avm.Registers.Register_Type := kv.avm.Registers.Make_S(0);
      Reg_Out : kv.avm.Registers.Register_Type;
      VM : aliased kv.avm.Machines.Machine_Type;
      Status : kv.avm.Control.Status_Type;
      use kv.avm.Control;
   begin
      Put_Line("test 02");
      Cap_Pool.Add("Fake", Cap'UNCHECKED_ACCESS);
      Assert(Cap_Pool.Has("Fake"), "has error");
      Assert(not Cap_Pool.Has("Real"), "not has error");
      Assert(not Cap.Called, "Cap init'd to called");
      Cap_Pool.Execute("Fake", VM, Reg_In, Reg_Out, Status);
      Assert(Status = Active, "wrong good status");
      Assert(Cap.Called, "Cap not called");
      Cap_Pool.Execute("Real", VM, Reg_In, Reg_Out, Status);
      Assert(Status = Error, "wrong bad status");
   end Run_Test;


   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   package Test_Client is
      type Fake_Client_Type is new kv.avm.Clients.Client_Interface with
         record
            Status : kv.avm.Clients.Status_Type := kv.avm.Clients.Uninitialized;
            Port : Natural := 0;
            Address : Unbounded_String;
            Pending : kv.avm.Transactions.Transactions_Access;
            Domain : Interfaces.Unsigned_32;
            Drop_Transaction : Boolean := False;
            Drop_Count : Natural := 0;
         end record;
      type Fake_Client_Access is access all Fake_Client_Type;
      overriding procedure Bind_Address
         (Self    : in out Fake_Client_Type;
          Address : in     String);
      overriding procedure Bind_Port
         (Self : in out Fake_Client_Type;
          Port : in     Positive);
      overriding procedure Open
         (Self : in out Fake_Client_Type);
      overriding procedure Close
         (Self : in out Fake_Client_Type);
      overriding function Get_Status
         (Self : Fake_Client_Type) return kv.avm.Clients.Status_Type;
      overriding function Is_Open
         (Self : Fake_Client_Type) return Boolean;
      overriding procedure Send_Transaction
         (Self        : in out Fake_Client_Type;
          Transaction : in     kv.avm.Transactions.Transactions_Interface'CLASS);
      overriding procedure Conclude_Transaction
         (Self : in out Fake_Client_Type);
      overriding function Is_Transaction_Pending(Self : Fake_Client_Type) return Boolean;
      overriding function Get_Transaction(Self : Fake_Client_Type) return kv.avm.Transactions.Transactions_Access;
      overriding
      function Get_Domain(Self : Fake_Client_Type) return Interfaces.Unsigned_32;


      type Fake_Client_Factory is new kv.avm.Clients.Client_Factory with null record;
      overriding procedure New_Client
         (Self   : in out Fake_Client_Factory;
          Client :    out kv.avm.Clients.Client_Access);
   end Test_Client;

   package body Test_Client is

      -------------------------------------------------------------------------
      procedure Bind_Address
         (Self    : in out Fake_Client_Type;
          Address : in     String) is
      begin
         kv.avm.Log.Put_Line("Test_Client.Bind_Address called with <" & Address & ">");
         Self.Address := To_Unbounded_String(Address);
         if Self.Port /= 0 then
            Self.Status := kv.avm.Clients.Closed;
         end if;
      end;

      -------------------------------------------------------------------------
      procedure Bind_Port
         (Self : in out Fake_Client_Type;
          Port : in     Positive) is
      begin
         Self.Port := Port;
         if Self.Address /= "" then
            Self.Status := kv.avm.Clients.Closed;
         end if;
      end;

      -------------------------------------------------------------------------
      procedure Open
         (Self : in out Fake_Client_Type) is
      begin
         Self.Status := kv.avm.Clients.Running;
      end;

      -------------------------------------------------------------------------
      procedure Close
         (Self : in out Fake_Client_Type) is
      begin
         Self.Status := kv.avm.Clients.Closed;
      end;

      -------------------------------------------------------------------------
      function Get_Status
         (Self : Fake_Client_Type) return kv.avm.Clients.Status_Type is
      begin
         return Self.Status;
      end Get_Status;

      -------------------------------------------------------------------------
      function Is_Open
         (Self : Fake_Client_Type) return Boolean is
      begin
         return Self.Status in kv.avm.Clients.Open_Status_Type;
      end Is_Open;

      -------------------------------------------------------------------------
      procedure Send_Transaction
         (Self        : in out Fake_Client_Type;
          Transaction : in     kv.avm.Transactions.Transactions_Interface'CLASS) is
      begin
         if Self.Drop_Transaction then
            Self.Drop_Count := Self.Drop_Count + 1;
         else
            Self.Status := kv.avm.Clients.Transacting;
            Self.Pending := Transaction.New_Copy;
         end if;
      end Send_Transaction;

      -------------------------------------------------------------------------
      procedure Conclude_Transaction
         (Self : in out Fake_Client_Type) is
      begin
         kv.avm.Transactions.Free(Self.Pending);
         Self.Status := kv.avm.Clients.Running;
      end Conclude_Transaction;

      -------------------------------------------------------------------------
      function Is_Transaction_Pending(Self : Fake_Client_Type) return Boolean is
         use kv.avm.Transactions;
      begin
         return Self.Pending /= null;
      end Is_Transaction_Pending;

      -------------------------------------------------------------------------
      function Get_Transaction(Self : Fake_Client_Type) return kv.avm.Transactions.Transactions_Access is
      begin
         return Self.Pending;
      end Get_Transaction;

      -------------------------------------------------------------------------
      function Get_Domain(Self : Fake_Client_Type) return Interfaces.Unsigned_32 is
      begin
         return Self.Domain;
      end Get_Domain;


      -------------------------------------------------------------------------
      procedure New_Client
         (Self   : in out Fake_Client_Factory;
          Client :    out kv.avm.Clients.Client_Access) is
      begin
         kv.avm.Log.Put_Line("Test_Client.New_Client called");
         Client := new Fake_Client_Type;
      end;

   end Test_Client;

   -----------------------------------------------------------------------------
   function Name(T : Test_03) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 03: affiliates.");
   end Name;

   procedure Run_Test(T : in out Test_03) is
      Settings : kv.avm.Ini.Settings_Type;
      Affiliator : kv.avm.Affiliates.Affiliates_Type;
      Factory : aliased Test_Client.Fake_Client_Factory;
      Client : kv.avm.Clients.Client_Access;
      Fake_Client : Test_Client.Fake_Client_Access;
      use kv.avm.Clients;
   begin
      Put_Line("test 03");
      Settings.Parse_Line("server_port = localhost");
      Settings.Parse_Line("affiliate_address = [""localhost"", 29678]");
      Settings.Parse_Line("affiliate_address = [""197.87.37.7"", 29678]");
      Affiliator.Initialize(Settings, null, Factory'UNCHECKED_ACCESS, null);
      Assert(Affiliator.Client_Count = 2, "wrong number of clients");
      Client := Affiliator.Get_Client(1);
      Assert(not Client.Is_Open, "client 1 should not be open");
      Fake_Client := Test_Client.Fake_Client_Access(Client);
      Assert(Fake_Client.Port = 29678, "fake client 1 has wrong port");
      Fake_Client.Domain := 88;
      Client := Affiliator.Get_Client(2);
      Assert(not Client.Is_Open, "client 2 should not be open");
      Fake_Client := Test_Client.Fake_Client_Access(Client);
      Assert(Fake_Client.Address = "197.87.37.7", "fake client 2 has wrong address");
      Fake_Client.Domain := 77;
      Affiliator.Open_Clients;
      Assert(Client.Is_Open, "client 2 should be open");
      -- Check domain-related processing
      Client := Affiliator.Get_Domain_Client(66);
      Assert(Client = null, "Get_Domain_Client returned non-null for an invalid domain");
      Client := Affiliator.Get_Domain_Client(88);
      Assert(Client /= null, "Get_Domain_Client returned null for a valid domain (88)");
      Assert(Client.Get_Domain = 88, "expected 88, got" & Interfaces.Unsigned_32'IMAGE(Client.Get_Domain));
      Client := Affiliator.Get_Domain_Client(77);
      Assert(Client /= null, "Get_Domain_Client returned null for a valid domain (77)");
      Assert(Client.Get_Domain = 77, "expected 77, got" & Interfaces.Unsigned_32'IMAGE(Client.Get_Domain));
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name(T : Test_04) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 04: capabilities II.");
   end Name;

   procedure Run_Test(T : in out Test_04) is
      Cap_Pool : kv.avm.Capabilities.Capabilities_Type;
      Cap : aliased Test_Cap.Fake_Cap_Type;
      Reg_In : kv.avm.Registers.Register_Type := kv.avm.Registers.Make_S(0);
      Reg_Out : kv.avm.Registers.Register_Type;
      VM : aliased kv.avm.Machines.Machine_Type;
      Status : kv.avm.Control.Status_Type;
      use kv.avm.Control;
      use kv.avm.Registers;
   begin
      Put_Line("test 04");
      VM.Set_Capabilities(Cap_Pool);
      Cap_Pool.Add("Fake", Cap'UNCHECKED_ACCESS);
      Assert(not Cap.Called, "Cap init'd to called");
      VM.Trap_To_The_Machine("Fake", Reg_In, Reg_Out, Status);
      Assert(Status = Active, "wrong good status");
      Assert(Cap.Called, "Cap not called");
      Assert(Reg_Out = kv.avm.Registers.Make_U(1397), "trap output is wrong");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name(T : Test_05) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 05: brokers.");
   end Name;

   procedure Run_Test(T : in out Test_05) is
      Broker : aliased kv.avm.Brokers.Broker_Type;
      Cap_Pool : kv.avm.Capabilities.Capabilities_Type;
      Actor_Ref : kv.avm.Actor_References.Actor_Reference_Type;
      VM : aliased kv.avm.Machines.Machine_Type;
      Status : kv.avm.Control.Status_Type;
      Broker_Key : constant String := "Broker";
      Registered_Actor_Name : constant String := "Test_Actor";
      UnRegistered_Actor_Name : constant String := "Bad_Actor";
      Runtime_Added_Actor_Name : constant String := "Added_Later";
      Broker_Get_Operation : constant String := "Get";
      Broker_Has_Operation : constant String := "Has";
      Broker_Add_Operation : constant String := "Add";
      Reg_In : kv.avm.Registers.Register_Type;
      Reg_Out : kv.avm.Registers.Register_Type;
      use kv.avm.Control;
      use kv.avm.Registers;
      use kv.avm.Actor_References;

      function Make_Request(Parameters : kv.avm.Memories.Register_Set_Type) return kv.avm.Registers.Register_Type is
         Request_Information : kv.avm.Memories.Register_Array_Type;
         Request : kv.avm.Tuples.Tuple_Type;
      begin
         Request_Information.Initialize(Parameters);
         Request.Fold(Request_Information);
         return Make_Tuple(Request);
      end Make_Request;

   begin
      Put_Line("test 05");
      --kv.avm.Log.Verbose := True;
      VM.Set_Capabilities(Cap_Pool);
      Cap_Pool.Add(Broker_Key, Broker'UNCHECKED_ACCESS);
      Actor_Ref.Initialize(512);

      -- Add it to the broker with a name
      Broker.Add(Actor_Ref, Registered_Actor_Name);

      -- execute the broker passing in the name
      Reg_In := Make_Request( (Make_String(Broker_Get_Operation), Make_String(Registered_Actor_Name)) );
      VM.Trap_To_The_Machine(Broker_Key, Reg_In, Reg_Out, Status);

      -- check to see that the instance reference is returned
      Assert(Status = Active, "wrong status (Get)");
      Assert(Reg_Out.Format = Actor_Reference, "wrong return type (Get)");
      Assert(Reg_Out.Instance = Actor_Ref, "Broker.Get operation did not return the supplied actor reference");

      Reg_In := Make_Request( (Make_String(Broker_Has_Operation), Make_String(UnRegistered_Actor_Name)) );
      VM.Trap_To_The_Machine(Broker_Key, Reg_In, Reg_Out, Status);
      Assert(Status = Active, "wrong status (Has)"); -- Status is always Active unless there is a fatal error
      Assert(Reg_Out.Format = Bit_Or_Boolean, "wrong return type (Has)");
      Assert(not Reg_Out.Bit, "Broker.Has returned True when asked about the unregistered actor");

      Reg_In := Make_Request( (Make_String(Broker_Has_Operation), Make_String(Registered_Actor_Name)) );
      VM.Trap_To_The_Machine(Broker_Key, Reg_In, Reg_Out, Status);
      Assert(Reg_Out.Bit, "Broker.Has returned False when asked about the test actor");

      Actor_Ref.Initialize(87);
      Reg_In := Make_Request( (Make_String(Broker_Add_Operation), Make_String(Runtime_Added_Actor_Name), Make_Ref(Actor_Ref)) );
      VM.Trap_To_The_Machine(Broker_Key, Reg_In, Reg_Out, Status);
      Assert(Broker.Is_Available(Runtime_Added_Actor_Name), "Runtime add didn't work");

      Reg_In := Make_Request( (Make_String("Not_A_Valid_Operation"), Make_String(Runtime_Added_Actor_Name)) );
      VM.Trap_To_The_Machine(Broker_Key, Reg_In, Reg_Out, Status);
      Assert(Status = Error, "wrong status (Not_A_Valid_Operation)");
   end Run_Test;



   -----------------------------------------------------------------------------
   function Make_Tuple(Parameters : kv.avm.Memories.Register_Set_Type) return kv.avm.Tuples.Tuple_Type is
      Prepared_Data : kv.avm.Memories.Register_Array_Type;
      Folded_Data : kv.avm.Tuples.Tuple_Type;
   begin
      Prepared_Data.Initialize(Parameters);
      Folded_Data.Fold(Prepared_Data);
      return Folded_Data;
   end Make_Tuple;




   -----------------------------------------------------------------------------
   function Name(T : Test_06) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 06: transaction streaming.");
   end Name;

   procedure Run_Test(T : in out Test_06) is
      use kv.avm.Registers;
      File_Name  : constant String := "comm_test_06_stream_data.bin";
      File_Out   : SIO.File_Type;
      Stream_Out : SIO.Stream_Access;
      File_In    : SIO.File_Type;
      Stream_In  : SIO.Stream_Access;
      Reg_Out    : Register_Type;
      Reg_In     : Register_Type;
      Close_Out  : kv.avm.Transactions.Basic.Close_Connections_Type;
      Close_In   : kv.avm.Transactions.Transactions_Access;

      Broker_Out : kv.avm.Transactions.Basic.Register_Instance_Type;
      Broker_In  : kv.avm.Transactions.Transactions_Access;

      Reply_Out  : kv.avm.Transactions.Basic.Reply_Transaction_Type;
      Reply_In   : kv.avm.Transactions.Transactions_Access;

      Inner_Tuple : kv.avm.Tuples.Tuple_Type;
      Outer_Tuple : kv.avm.Tuples.Tuple_Type;

      Message_Out : kv.avm.Transactions.Basic.Send_Message_Type;
      Message_In  : kv.avm.Transactions.Transactions_Access;

      Test_Message : kv.avm.Messages.Message_Type;
      AR_Src : kv.avm.Actor_References.Actor_Reference_Type;
      AR_Rep : kv.avm.Actor_References.Actor_Reference_Type;
      AR_Dst : kv.avm.Actor_References.Actor_Reference_Type;

      use kv.avm.Transactions;
      use Interfaces;
      use kv.avm.Tuples;

   begin
      Put_Line("test 06");
      -- Set up output data
      Reg_Out := kv.avm.Registers.Make_String("Hello World!");
      Broker_Out.Set_Name("One");
      Inner_Tuple := Make_Tuple( (Reg_Out, kv.avm.Registers.Make_U(100)) );
      Outer_Tuple := Make_Tuple( (kv.avm.Registers.Make_S(-99), kv.avm.Registers.Make_Tuple(Inner_Tuple)) );
      Reply_Out.Set_Data(Outer_Tuple);
      Reply_Out.Set_Future(16384);
      AR_Src.Initialize(65);
      AR_Rep.Initialize(66);
      AR_Dst.Initialize(67);
      Test_Message.Initialize
         (Source       => AR_Src,
          Reply_To     => AR_Rep,
          Destination  => AR_Dst,
          Message_Name => "Test_Message",
          Data         => Inner_Tuple,
          Future       => 32768);
      Message_Out.Set_Message(Test_Message);

      SIO.Create(File_Out, SIO.Out_File, File_Name);
      Stream_Out := SIO.Stream(File_Out);
      Register_Type'WRITE(Stream_Out, Reg_Out);
      kv.avm.Transactions.Transactions_Interface'CLASS'OUTPUT(Stream_Out, Close_Out);
      kv.avm.Transactions.Transactions_Interface'CLASS'OUTPUT(Stream_Out, Broker_Out);
      kv.avm.Transactions.Transactions_Interface'CLASS'OUTPUT(Stream_Out, Reply_Out);
      kv.avm.Transactions.Transactions_Interface'CLASS'OUTPUT(Stream_Out, Message_Out);
      -- ...
      SIO.Close(File_Out);

      SIO.Open(File_In, SIO.In_File, File_Name);
      Stream_In := SIO.Stream(File_In);
      Register_Type'READ(Stream_In, Reg_In);
      Close_In := kv.avm.Transactions.New_Transaction_From_Stream(Stream_In);
      Broker_In := kv.avm.Transactions.New_Transaction_From_Stream(Stream_In);
      Reply_In := kv.avm.Transactions.New_Transaction_From_Stream(Stream_In);
      Message_In := kv.avm.Transactions.New_Transaction_From_Stream(Stream_In);
      -- ...
      SIO.Close(File_In);

      -- Check the data
      Assert(Reg_In = Reg_Out, "register streaming failed");
      Assert(Close_Out.Equals(Close_In.all), "close connection transaction streaming failed");
      Assert(Broker_Out.Equals(Broker_In.all), "register instance transaction streaming failed");
      Assert(Basic.Reply_Transaction_Type(Reply_In.all).Get_Future = 16384, "wrong future");
      Assert(Basic.Reply_Transaction_Type(Reply_In.all).Get_Data.Peek(1).Folded_Tuple.Peek(0).all = Reg_Out, "wrong Peek(1).Peek(0) from " & Basic.Reply_Transaction_Type(Reply_In.all).Get_Data.To_String);
      Assert(Basic.Reply_Transaction_Type(Reply_In.all).Get_Data = Outer_Tuple, "wrong tuple");
      Assert(Reply_Out.Equals(Reply_In.all), "reply transaction streaming failed");
      Assert(Message_Out.Equals(Message_In.all), "send message transaction streaming failed");
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name(T : Test_07) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 07: Routing.");
   end Name;

   procedure Run_Test(T : in out Test_07) is
      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Instance_Factory : aliased kv.avm.Test.Runners.Runner_Factory;
      Router : kv.avm.Routers.Router_Type;
      Runner : kv.avm.Test.Runners.Runner_Access;
      Local_Actor_Ref : kv.avm.Actor_References.Actor_Reference_Type;
      Remote_Actor_Ref : kv.avm.Actor_References.Actor_Reference_Type;
      Status : kv.avm.Control.Status_Type;
      Message : kv.avm.Messages.Message_Type;
      Fixed   : kv.avm.Memories.Register_Array_Type;
      Actor : kv.avm.Actors.Actor_Access;
      Data : kv.avm.Tuples.Tuple_Type;
      Settings : kv.avm.Ini.Settings_Type;
      Affiliator : kv.avm.Affiliates.Affiliates_Type;
      Client_Factory : aliased Test_Client.Fake_Client_Factory;
      Client : kv.avm.Clients.Client_Access;
      Fake_Client : Test_Client.Fake_Client_Access;
      TEST_DOMAIN : constant Interfaces.Unsigned_32 := 400;
      Transaction : kv.avm.Transactions.Transactions_Access;
      Sent_Transaction : kv.avm.Transactions.Basic.Send_Message_Access;
      Sent_Message : kv.avm.Messages.Message_Type;

      use kv.avm.Clients;
      use kv.avm.Control;
   begin
      Put_Line("test 07");
      --kv.avm.Log.Verbose := True;
      kv.avm.Actor_References.Set_Local_Domain(13);
      VM.Initialize(CPU'UNCHECKED_ACCESS, Instance_Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);
      Router := VM.Get_Router;
      Actor := kv.avm.Actors.New_Actor("A1", null, 0, Fixed);
      VM.New_Actor("A1", Local_Actor_Ref);
      Assert(Local_Actor_Ref.Is_Local, "local actor ref reports remote");
      Runner := Instance_Factory.Get_Runner_By_ID(1); -- ID = count
      Data.Fold_Empty;

      Settings.Parse_Line("server_port = localhost");
      Settings.Parse_Line("affiliate_address = [""localhost"", 29678]");
      Affiliator := Router.Get_Affiliator;
      Affiliator.Initialize(Settings, null, Client_Factory'UNCHECKED_ACCESS, VM'UNCHECKED_ACCESS);
      Client := Affiliator.Get_Client(1);
      Fake_Client := Test_Client.Fake_Client_Access(Client);
      Fake_Client.Domain := TEST_DOMAIN;

      -- Create a local message and make sure it is routed locally
      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Local_Actor_Ref,
          Message_Name => "Local",
          Data         => Data,
          Future       => 0);
      Router.Post_Message(Message, Status);
      Assert(Runner.Last_Msg.Get_Name = "Local", "the 'Local' message wasn't delivered");

      -- Create a remote message and make sure it is routed to the right client
      Remote_Actor_Ref.Initialize(Key => 800, Domain => TEST_DOMAIN);
      Assert(not Remote_Actor_Ref.Is_Local, "remote actor ref reports local");
      Put_Line(Remote_Actor_Ref.Image);
      Message.Finalize;
      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Remote_Actor_Ref,
          Message_Name => "Remote",
          Data         => Data,
          Future       => 0);
      Router.Post_Message(Message, Status);
      Assert(Status = Active, "Expected status to be Active, but it was " & kv.avm.Control.Status_Type'IMAGE(Status));
      Assert(Client.Is_Transaction_Pending, "Expected a transaction to be pending (due to send_transaction)");
      Transaction := Client.Get_Transaction;
      Assert(Transaction.all in kv.avm.Transactions.Basic.Send_Message_Type, "wrong transaction type");
      Sent_Transaction := kv.avm.Transactions.Basic.Send_Message_Access(Transaction);
      Sent_Message := Sent_Transaction.Get_Message;
      Put_Line("Sent_Message = " & Sent_Message.Debug);
      Assert(Sent_Message.Get_Name = "Remote", "wrong message");
      Client.Conclude_Transaction; -- Free the allocated copy of the transaction
      Assert(not Client.Is_Transaction_Pending, "Expected a transaction to be NOT pending");


      -- Check remote replies replies
      Router.Post_Response
         (Reply_To => Remote_Actor_Ref,
          Answer   => Data,
          Future   => 32);
      Assert(Client.Is_Transaction_Pending, "Expected a transaction to be pending (due to Reply_Transaction)");
      Transaction := Client.Get_Transaction;
      Assert(Transaction.all in kv.avm.Transactions.Basic.Reply_Transaction_Type, "wrong transaction type (expected Reply_Transaction_Type)");


      kv.avm.Clients.Free(Client);
   end Run_Test;



   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   package Fake_Servers is
      type Server_Type;
      type Server_Access is access all Server_Type;
      type Server_Type is new kv.avm.Servers.Server_Interface with
         record
            Myself     : Server_Access;
            Poll_Count : Natural := 0;
            Connection : Test_Client.Fake_Client_Access;
         end record;

      procedure Initialize(Self : access Server_Type);

      procedure Pend_Client
         (Self   : in out Server_Type;
          Domain : in     Interfaces.Unsigned_32);

      overriding
      function Is_Connection_Waiting(Self : Server_Type) return Boolean;

      overriding
      function Get_Connection(Self : Server_Type) return kv.avm.Clients.Client_Access;
   end Fake_Servers;

   package body Fake_Servers is

      -------------------------------------------------------------------------
      procedure Initialize(Self : access Server_Type) is
      begin
         Self.Myself := Self.all'UNCHECKED_ACCESS;
      end Initialize;

      -------------------------------------------------------------------------
      procedure Pend_Client
         (Self   : in out Server_Type;
          Domain : in     Interfaces.Unsigned_32) is
      begin
         Self.Connection := new Test_Client.Fake_Client_Type;
         Self.Connection.Open;
         Self.Connection.Domain := Domain;
      end Pend_Client;

      -------------------------------------------------------------------------
      function Is_Connection_Waiting(Self : Server_Type) return Boolean is
         use Test_Client;
      begin
         return Self.Connection /= null;
      end Is_Connection_Waiting;

      -------------------------------------------------------------------------
      function Get_Connection(Self : Server_Type) return kv.avm.Clients.Client_Access is
         Answer : kv.avm.Clients.Client_Access := kv.avm.Clients.Client_Access(Self.Connection);
      begin
         Self.Myself.Connection := null;
         return Answer;
      end Get_Connection;
   end Fake_Servers;



   -----------------------------------------------------------------------------
   function Name(T : Test_08) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 08: Affiliates -- receive side.");
   end Name;

   procedure Run_Test(T : in out Test_08) is

      VM  : aliased kv.avm.Machines.Machine_Type;
      CPU : aliased kv.avm.Processors.Processor_Type;
      Instance_Factory : aliased kv.avm.Test.Runners.Runner_Factory;
      Settings : kv.avm.Ini.Settings_Type;
      Affiliator : kv.avm.Affiliates.Affiliates_Type;
      Client_Factory : aliased Test_Client.Fake_Client_Factory;
      Client : kv.avm.Clients.Client_Access;
      Client_1 : Test_Client.Fake_Client_Access;
      Client_2 : Test_Client.Fake_Client_Access;
      TEST_DOMAIN : constant Interfaces.Unsigned_32 := 399;
      ADDED_DOMAIN : constant Interfaces.Unsigned_32 := 401;
      Server : aliased Fake_Servers.Server_Type;

      Send_Transaction : kv.avm.Transactions.Basic.Send_Message_Type;
      Reply_Transaction : kv.avm.Transactions.Basic.Reply_Transaction_Type;
      Actor_1_Ref : kv.avm.Actor_References.Actor_Reference_Type;
      Actor_2_Ref : kv.avm.Actor_References.Actor_Reference_Type;
      Message : kv.avm.Messages.Message_Type;
      Data : kv.avm.Tuples.Tuple_Type;

      use kv.avm.Clients;
      use kv.avm.Transactions;

   begin
      Put_Line("test 08");
      kv.avm.Log.Verbose := True;
      kv.avm.Actor_References.Set_Local_Domain(14);
      VM.Initialize(CPU'UNCHECKED_ACCESS, Instance_Factory'UNCHECKED_ACCESS);
      CPU.Initialize(VM'UNCHECKED_ACCESS);
      Affiliator := VM.Get_Router.Get_Affiliator;
      Settings.Parse_Line("server_port = localhost");
      Settings.Parse_Line("affiliate_address = [""localhost"", 29678]");
      Server.Initialize;
      Affiliator.Initialize(Settings, Server'UNCHECKED_ACCESS, Client_Factory'UNCHECKED_ACCESS, VM'UNCHECKED_ACCESS);
      Client := Affiliator.Get_Client(1);
      Client_1 := Test_Client.Fake_Client_Access(Client);
      Client_1.Domain := TEST_DOMAIN;
      Actor_1_Ref.Initialize(8, TEST_DOMAIN);
      Actor_2_Ref.Initialize(9, ADDED_DOMAIN);
      Data.Fold_Empty;

      -- poll the affiliator
      Affiliator.Periodic_Processing;
      Assert(Affiliator.Client_Count = 1, "should have 1 client");

      -- it will poll the server, which will add a new client
      Server.Pend_Client(ADDED_DOMAIN);
      Affiliator.Periodic_Processing;
      Assert(Affiliator.Client_Count = 2, "should have 2 clients");
      Client := Affiliator.Get_Domain_Client(ADDED_DOMAIN);
      Assert(Client /= null, "Get_Domain_Client returned null for a valid domain (ADDED_DOMAIN)");
      Client_2 := Test_Client.Fake_Client_Access(Client);

      -- a client will then send a message
      Message.Initialize
         (Source       => Actor_2_Ref,
          Reply_To     => Actor_2_Ref,
          Destination  => Actor_1_Ref,
          Message_Name => "Msg_2_To_1",
          Data         => Data,
          Future       => 5);
      Send_Transaction.Set_Message(Message);
      Client_2.Pending := Send_Transaction.New_Copy;
      Affiliator.Periodic_Processing;

      -- test to see if it is routed
      Assert(Client_1.Pending /= null, "Client_1 has not been given a transaction");

      -- replies
      Free(Client_1.Pending);
      Reply_Transaction.Set_Reply_To(Actor_2_Ref);
      Reply_Transaction.Set_Data(Data);
      Reply_Transaction.Set_Future(5);
      Client_1.Pending := Reply_Transaction.New_Copy;
      Client_2.Drop_Transaction := True; -- Have the fake behave properly and not re-reflect the transaction
      Affiliator.Periodic_Processing;
      Assert(Client_2.Drop_Count = 1, "response was not sent back to client 2");

      --!@#$ register actors
   end Run_Test;

   -----------------------------------------------------------------------------
   function Name(T : Test_09) return AUnit.Message_String is
      pragma Unreferenced(T);
   begin
      return AUnit.Format("comm test 09: tbd.");
   end Name;

   procedure Run_Test(T : in out Test_09) is
   begin
      Put_Line("test 09");
   end Run_Test;

end kv.avm.Comm_Tests;
