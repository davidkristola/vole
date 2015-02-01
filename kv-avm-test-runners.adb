with Ada.Unchecked_Deallocation;

with kv.avm.Log; use kv.avm.Log;
with kv.avm.Actor_Pool;

package body kv.avm.Test.Runners is

   use Interfaces;

   -----------------------------------------------------------------------------
   procedure Execute
      (Self   : in out Base_Behavior_Type;
       Runner : in out Runner_Type'CLASS) is
   begin
      Put_Line("Base_Behavior_Type for " & Runner.Image & ", Self.Remaining = " & Natural'IMAGE(Self.Remaining));
      if Self.Remaining = 0 then
         Do_It_Now(Base_Behavior_Type'CLASS(Self), Runner);
         Self.Remaining := Self.Cycle_Time;
      else
         Self.Remaining := Self.Remaining - 1;
      end if;
   end Execute;



   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Consume_After_X is new Base_Behavior_Type with null record;
   type Consume_After_X_Access is access all Consume_After_X;
   procedure Free is new Ada.Unchecked_Deallocation(Consume_After_X, Consume_After_X_Access);

   -----------------------------------------------------------------------------
   procedure Do_It_Now
      (Self   : in out Consume_After_X;
       Runner : in out Runner_Type'CLASS) is
   begin
      Runner.Running := False;
   end Do_It_Now;




   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Send_After_X is new Base_Behavior_Type with
      record
         Destination : kv.avm.Actor_References.Actor_Reference_Type;
      end record;
   type Send_After_X_Access is access all Send_After_X;
   procedure Free is new Ada.Unchecked_Deallocation(Send_After_X, Send_After_X_Access);

   -----------------------------------------------------------------------------
   procedure Execute
      (Self   : in out Send_After_X;
       Runner : in out Runner_Type'CLASS) is

      Machine : kv.avm.Control.Control_Access;
      Empty_Tuple : aliased kv.avm.Tuples.Tuple_Type;
      Message     : kv.avm.Messages.Message_Type;

   begin
      Put_Line("Send_After_X for " & Runner.Image & ", Self.Remaining = " & Natural'IMAGE(Self.Remaining));
      if Self.Remaining = 0 then
         Machine := Runner.CPU.Get_Machine;
         Empty_Tuple.Initialize;
         Empty_Tuple.Fold_Empty;
         Message.Initialize
            (Source       => kv.avm.Actor_References.Null_Reference,
             Reply_To     => kv.avm.Actor_References.Null_Reference,
             Destination  => Self.Destination,
             Message_Name => "Ping",
             Data         => Empty_Tuple,
             Future       => kv.avm.Control.NO_FUTURE);
         Machine.Post_Message
            (Message => Message,
             Status  => Runner.Status); -- Important to pass this through
         Self.Remaining := Self.Cycle_Time;
      else
         Runner.Status := kv.avm.Control.Active;
         Self.Remaining := Self.Remaining - 1;
      end if;
   end Execute;


   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   type Spawn_After_X is new Base_Behavior_Type with null record;
   type Spawn_After_X_Access is access all Spawn_After_X;
   procedure Free is new Ada.Unchecked_Deallocation(Spawn_After_X, Spawn_After_X_Access);

   -----------------------------------------------------------------------------
   procedure Do_It_Now
      (Self   : in out Spawn_After_X;
       Runner : in out Runner_Type'CLASS) is

      Machine     : kv.avm.Control.Control_Access;
      Actor       : kv.avm.Actor_References.Actor_Reference_Type;
      Empty_Tuple : aliased kv.avm.Tuples.Tuple_Type;
      Status      : kv.avm.Control.Status_Type;
      Message     : kv.avm.Messages.Message_Type;

   begin
      Machine := Runner.CPU.Get_Machine;
      Machine.New_Actor("Test_A1", Actor);

      Empty_Tuple.Initialize;
      Empty_Tuple.Fold_Empty;
      Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Actor,
          Message_Name => "CONSTRUCTOR",
          Data         => Empty_Tuple,
          Future       => kv.avm.Control.NO_FUTURE);
      Machine.Post_Message
         (Message => Message,
          Status  => Status);
   end Do_It_Now;




   -----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Runner_Type;
       ID   : in     Interfaces.Unsigned_32;
       Next : in     Runner_Access) is

      Behavior : Consume_After_X_Access;

   begin
      Self.ID := ID;
      Put_Line(Self.Image & ".Initialize");
      Self.Next := kv.avm.Executables.Executable_Access(Next);
      Behavior := new Consume_After_X;
      Behavior.Cycle_Time := 3;
      Behavior.Remaining := 3;
      Self.Behavior := Behavior_Access(Behavior);
   end Initialize;


   -----------------------------------------------------------------------------
   procedure Free_Behavior
      (Self : in out Runner_Type) is

      Free_Me : Consume_After_X_Access;

   begin
      if Self.Behavior /= null then
         if Self.Behavior.all in Consume_After_X'CLASS then
            Free_Me := Consume_After_X_Access(Self.Behavior);
            Free(Free_Me);
            Self.Behavior := null;
         end if;
      end if;
   end Free_Behavior;

   -----------------------------------------------------------------------------
   procedure Set_Behavior_Send
      (Self        : in out Runner_Type;
       Destination : in     kv.avm.Actor_References.Actor_Reference_Type;
       Cycle_Time  : in     Natural) is

      Behavior : Send_After_X_Access;

   begin
      Self.Free_Behavior;
      Behavior := new Send_After_X;
      Behavior.Cycle_Time := Cycle_Time;
      Behavior.Remaining := Cycle_Time;
      Behavior.Destination := Destination;
      Self.Behavior := Behavior_Access(Behavior);
   end Set_Behavior_Send;


   -----------------------------------------------------------------------------
   procedure Set_Behavior_Spawn
      (Self       : in out Runner_Type;
       Cycle_Time : in     Natural) is

      Behavior : Spawn_After_X_Access;

   begin
      Self.Free_Behavior;
      Behavior := new Spawn_After_X;
      Behavior.Cycle_Time := Cycle_Time;
      Behavior.Remaining := Cycle_Time;
      Self.Behavior := Behavior_Access(Behavior);
   end Set_Behavior_Spawn;


   -----------------------------------------------------------------------------
   procedure Process_Message
      (Self    : in out Runner_Type;
       Message : in     kv.avm.Messages.Message_Type) is

   begin
      Put_Line(Self.Image & ".Process_Message " & Message.Image);
      Self.Running := True;
      Self.Last_Msg := Message;
   end Process_Message;

   -----------------------------------------------------------------------------
   procedure Process_Gosub
      (Self      : access Runner_Type;
       Tailcall  : in     Boolean;
       Supercall : in     Boolean;
       Reply_To  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Method    : in     kv.avm.Registers.String_Type;
       Data      : access constant kv.avm.Memories.Register_Set_Type;
       Future    : in     Interfaces.Unsigned_32) is

   begin
      Put_Line(Self.Image & ".Process_Gosub");
   end Process_Gosub;

   -----------------------------------------------------------------------------
   function Can_Accept_Message_Now(Self : Runner_Type; Message : kv.avm.Messages.Message_Type) return Boolean is
   begin
      Put_Line(Self.Image & ".Can_Accept_Message_Now returning " & Boolean'IMAGE(not Self.Running));
      return not Self.Running;
   end Can_Accept_Message_Now;

   -----------------------------------------------------------------------------
   function Program_Counter
      (Self : in    Runner_Type) return Interfaces.Unsigned_32 is
   begin
      Put_Line(Self.Image & ".Program_Counter = " & Interfaces.Unsigned_32'IMAGE(Self.PC));
      return Self.PC;
   end Program_Counter;

   -----------------------------------------------------------------------------
   function Is_Running
      (Self : in    Runner_Type) return Boolean is
   begin
      Put_Line(Self.Image & ".Is_Running = " & Boolean'IMAGE(Self.Running));
      return Self.Running;
   end Is_Running;

   -----------------------------------------------------------------------------
   procedure Step
      (Self      : access Runner_Type;
       Processor : access kv.avm.Processors.Processor_Type;
       Status    :    out kv.avm.Control.Status_Type) is

   begin
      Put_Line(Self.Image & ".Step");
      Self.CPU := Processor.all'ACCESS;
      Self.PC := Self.PC + 1;
      if Self.Behavior /= null then
         Self.Behavior.Execute(Self.all);
      end if;
      Status := Self.Status;
   end Step;

   -----------------------------------------------------------------------------
   procedure Process_Internal_Response
      (Self   : in out Runner_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type) is
   begin
      Put_Line(Self.Image & ".Process_Internal_Response");
   end Process_Internal_Response;

   -----------------------------------------------------------------------------
   procedure Resolve_Future
      (Self   : in out Runner_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type;
       Future : in     Interfaces.Unsigned_32) is
   begin
      Put_Line(Self.Image & ".Resolve_Future");
   end Resolve_Future;

   -----------------------------------------------------------------------------
   procedure Halt_Actor
      (Self : in out Runner_Type) is
   begin
      Put_Line(Self.Image & ".Halt_Actor");
      Self.Running := False;
   end Halt_Actor;

   -----------------------------------------------------------------------------
   function Reachable(Self : Runner_Type) return kv.avm.Actor_References.Sets.Set is
   begin
      return kv.avm.Actor_References.Sets.Empty_Set;
   end Reachable;

   -----------------------------------------------------------------------------
   function Image(Self : Runner_Type) return String is
      Id_Img : constant String := Interfaces.Unsigned_32'IMAGE(Self.ID);
   begin
      return "Runner_"&Id_Img(2 .. Id_Img'LAST);
   end Image;

   -----------------------------------------------------------------------------
   function Debug_Info(Self : Runner_Type) return String is
   begin
      return Self.Image & ".Debug_Info";
   end Debug_Info;

   -----------------------------------------------------------------------------
   procedure New_Executable
      (Self       : in out Runner_Factory;
       Actor      : in     kv.avm.Actors.Actor_Access;
       Machine    : in     kv.avm.Control.Control_Access;
       Executable :    out kv.avm.Executables.Executable_Access;
       Reference  :    out kv.avm.Actor_References.Actor_Reference_Type) is

      Instance : Runner_Access;

   begin
      Put_Line("Runner_Factory.New_Executable");
      Instance := new Runner_Type;
      Self.Count := Self.Count + 1;
      Instance.Initialize(Self.Count, Self.Head);
      Self.Head := Instance;
      kv.avm.Actor_Pool.Add(kv.avm.Executables.Executable_Access(Instance), Reference);
      Executable := kv.avm.Executables.Executable_Access(Instance);
   end New_Executable;

   -----------------------------------------------------------------------------
   function Get_Allocated_Count(Self : Runner_Factory) return Natural is
   begin
      return Natural(Self.Count);
   end Get_Allocated_Count;

   -----------------------------------------------------------------------------
   function Runner_By_ID(Runner : Runner_Access; ID : Interfaces.Unsigned_32) return Runner_Access is
   begin
      if Runner.ID = ID then
         return Runner;
      else
         return Runner_By_ID(Runner_Access(Runner.Next), ID);
      end if;
   end Runner_By_ID;

   -----------------------------------------------------------------------------
   function Get_Runner_By_ID(Self : Runner_Factory; ID : Interfaces.Unsigned_32) return Runner_Access is
   begin
      return Runner_By_ID(Self.Head, ID);
   end Get_Runner_By_ID;


end kv.avm.Test.Runners;
