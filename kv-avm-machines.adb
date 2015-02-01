with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

with kv.avm.Log; use kv.avm.Log;
with kv.avm.Actors;
with kv.avm.Messages;
with kv.avm.Actor_Pool;
with kv.avm.Executables;
with kv.avm.Actor_References.Sets;

package body kv.avm.Machines is

   use Interfaces;
   use kv.avm.Control;
   use kv.avm.Executables;
   use kv.avm.Executable_Lists;





   -----------------------------------------------------------------------------
   procedure Initialize
      (Self      : in out Machine_Type;
       Processor : in     kv.avm.Processors.Processor_Access;
       Factory   : in     kv.avm.Executables.Factory_Access) is
   begin
      Self.Processor := Processor;
      Self.Factory := Factory;
      Self.Future := kv.avm.control.NO_FUTURE;
      Self.Router.Initialize(kv.avm.Control.Control_Interface(Self)'UNCHECKED_ACCESS);
      Self.Steps := 0;
      Self.Cursor := 0;
      Self.Reaped := 0;
      Self.Cycles := 0;
      for List in Self.Lists'RANGE loop
         Self.Lists(List).Initialize(List);
      end loop;
   end Initialize;

   -----------------------------------------------------------------------------
   function Get_Router(Self : Machine_Type) return kv.avm.Routers.Router_Type is
   begin
      return Self.Router;
   end Get_Router;

   -----------------------------------------------------------------------------
   function Check_For_Beginning_Of_Cycle(Self : Machine_Type) return Boolean is
   begin
      -- If the cursor has advanced down to zero (remember, it moves down)
      return Self.Cursor = 0;
   end Check_For_Beginning_Of_Cycle;

   -----------------------------------------------------------------------------
   function Check_Message_Delivery_Policy(Self : Machine_Type) return Boolean is
   begin
      -- Always deliver messages at the beginning of the cycle
      return True;
   end Check_Message_Delivery_Policy;

   -----------------------------------------------------------------------------
   function Check_Undeferral_Policy(Self : Machine_Type) return Boolean is
   begin
      -- If there are still no actor instances ready to run, undefer the deferred ones
      if Self.Router.Get_Queue_Size < 100 then
         return True;
      end if;
      return Self.Lists(Active).Get_Last = 0;
   end Check_Undeferral_Policy;

   -----------------------------------------------------------------------------
   function Check_Garbage_Collection_Policy(Self : Machine_Type) return Boolean is
   begin
      -- Collect garbage if we have a buildup of idle actor instances
      return Natural(Self.Lists(Idle).Get_Last) > (Self.Old_Idle + Self.Garbage_Trigger);
   end Check_Garbage_Collection_Policy;

   -----------------------------------------------------------------------------
   procedure Beginning_Of_Cycle
      (Self : in out Machine_Type) is
   begin
      Self.Cycles := Self.Cycles + 1;
      if self.Check_Message_Delivery_Policy then
         Self.Router.Deliver_Messages;
      end if;
      if Self.Check_Undeferral_Policy then
         Self.Undefer;
      end if;
      if self.Check_Garbage_Collection_Policy then
         Self.Garbage_Collection;
      end if;
      Self.Cursor := Self.Lists(Active).Get_Last; -- Restart the cursor
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Beginning_Of_Cycle): " & Exception_Information(Error));
         raise;
   end Beginning_Of_Cycle;

   -----------------------------------------------------------------------------
   procedure Process_Current_Executable
      (Self : in out Machine_Type) is

      Working : kv.avm.Executables.Executable_Access;
      Status : kv.avm.Control.Status_Type;

   begin
      Working := Self.Lists(Active).Get(Self.Cursor);
      if Working.Is_Running then
         Working.Step(Self.Processor, Status);
         if Status /= Active then
            --Put_Line("Machine Instance Control: deactivating " & Working.Image & " because it returned status = " & Status_Type'IMAGE(Status));
            --TODO: use the returned status to figure out what to do with the executable.
            Self.Lists(Status).Acquire_From(Self.Cursor, Self.Lists(Active));
         end if;
      else
         Put_Line("Non-running instance is in the active list, moving " & Working.Image & " to the idle list");
         Self.Lists(Idle).Acquire_From(Self.Cursor, Self.Lists(Active));
      end if;
      Self.Cursor := Self.Cursor - 1;
   end Process_Current_Executable;

   -----------------------------------------------------------------------------
   procedure Step
      (Self : in out Machine_Type) is
   begin
      Self.Steps := Self.Steps + 1;
      if Self.Check_For_Beginning_Of_Cycle then
         self.Beginning_Of_Cycle;
      end if;
      if Self.Cursor /= 0 then
         Self.Process_Current_Executable;
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Step): " & Exception_Information(Error));
         raise;
   end Step;


   -----------------------------------------------------------------------------
   function Current_Instance(Self : Machine_Type) return kv.avm.Executables.Executable_Access is
   begin
      if Self.Cursor = 0 then
         return Self.Lists(Active).Get(Self.Lists(Active).Get_Last);
      end if;
      return Self.Lists(Active).Get(Self.Cursor);
   end Current_Instance;


   -----------------------------------------------------------------------------
   function Done(Self : Machine_Type) return Boolean is
   begin
      return Self.Lists(Active).Get_Last = 0;
   end Done;


   -----------------------------------------------------------------------------
   function Get_Steps(Self : Machine_Type) return Natural is
   begin
      return Self.Steps;
   end Get_Steps;


   -----------------------------------------------------------------------------
   function Get_Total(Self : Machine_Type) return Natural is
      Count : Natural := 0;
   begin
      for Status in Active .. Idle loop
         Count := Count + Natural(Self.Lists(Status).Get_Last);
      end loop;
      return Count;
   end Get_Total;


   -----------------------------------------------------------------------------
   function Get_Active(Self : Machine_Type) return Natural is
   begin
      return Natural(Self.Lists(Active).Get_Last);
   end Get_Active;


   -----------------------------------------------------------------------------
   function Get_Idle(Self : Machine_Type) return Natural is
   begin
      return Natural(Self.Lists(Idle).Get_Last);
   end Get_Idle;


   -----------------------------------------------------------------------------
   function Get_Blocked(Self : Machine_Type) return Natural is
   begin
      return Natural(Self.Lists(Blocked).Get_Last);
   end Get_Blocked;


   -----------------------------------------------------------------------------
   function Get_Deferred(Self : Machine_Type) return Natural is
   begin
      return Natural(Self.Lists(Deferred).Get_Last);
   end Get_Deferred;


   -----------------------------------------------------------------------------
   function Get_Queue_Size(Self : Machine_Type) return Natural is
   begin
      return Self.Router.Get_Queue_Size;
   end Get_Queue_Size;


   -----------------------------------------------------------------------------
   procedure Set_Queue_Limit(Self : in out Machine_Type; Queue_Limit : in Natural) is
   begin
      Self.Router.Set_Queue_Limit(Queue_Limit);
   end Set_Queue_Limit;

   -----------------------------------------------------------------------------
   function Get_Cycles(Self : Machine_Type) return Natural is
   begin
      return Self.Cycles;
   end Get_Cycles;

   -----------------------------------------------------------------------------
   function Get_Reaped(Self : Machine_Type) return Natural is
   begin
      return Self.Reaped;
   end Get_Reaped;

   -----------------------------------------------------------------------------
   procedure Set_Capabilities(Self : in out Machine_Type; Capabilities : in kv.avm.Capabilities.Capabilities_Type) is
   begin
      Self.Capabilities := Capabilities;
   end Set_Capabilities;

   -----------------------------------------------------------------------------
   procedure Set_Garbage_Trigger(Self : in out Machine_Type; Garbage_Trigger : in Natural) is
   begin
      Self.Garbage_Trigger := Garbage_Trigger;
   end Set_Garbage_Trigger;




   -----------------------------------------------------------------------------
   procedure Deliver_Messages
      (Self : in out Machine_Type) is
   begin
      Self.Router.Deliver_Messages;
   end Deliver_Messages;


   -----------------------------------------------------------------------------
   -- Internal method
   procedure Undefer
      (Self : in out Machine_Type) is

      Current : kv.avm.Executable_Lists.Cursor_Type;
      Working : kv.avm.Executables.Executable_Access;

   begin
      Current := Self.Lists(Deferred).Get_Last;
      while Current /= 0 loop
         Working := Self.Lists(Deferred).Get(Current);
         if Working.Is_Running then
            Self.Lists(Active).Acquire_From(Current, Self.Lists(Deferred));
         end if;
         Current := Current - 1;
      end loop;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Undefer): " & Exception_Information(Error));
         raise;
   end Undefer;


   -----------------------------------------------------------------------------
   -- Internal method
   function Non_Idle(Self : Machine_Type) return kv.avm.Actor_References.Sets.Set is
      Current    : kv.avm.Executable_Lists.Cursor_Type;
      Working    : kv.avm.Executables.Executable_Access;
      Collection : kv.avm.Actor_References.Sets.Set := kv.avm.Actor_References.Sets.Empty_Set;
   begin
      for Status in Running_Status_Type loop
         Current := Self.Lists(Status).Get_Last;
         while Current /= 0 loop
            Working := Self.Lists(Status).Get(Current);
            Collection.Union(Working.Reachable);
            Current := Current - 1;
         end loop;
      end loop;
      return Collection;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Non_Idle): " & Exception_Information(Error));
         raise;
   end Non_Idle;


   -----------------------------------------------------------------------------
   -- Internal method
   function Expand_Reachable_Set
      (Self     : Machine_Type;
       Starting : kv.avm.Actor_References.Sets.Set) return kv.avm.Actor_References.Sets.Set is

      use kv.avm.Actor_References.Sets;
      use kv.avm.Actor_References;

      Working   : kv.avm.Executables.Executable_Access;
      Could_Run : Set;
      Added     : Set;
      Expand    : Set;
      Reach     : Set;
      Check     : Cursor;
      Reference : Actor_Reference_Type;

   begin
      Could_Run := Starting;
      Expand := Starting;
      loop
         -- Go through everything in Could_Run and see if all if its rachables are in the list.
         -- If not, add them and try it again.
         Added := Empty_Set;
         Check := Expand.First;
         while Check /= No_Element loop
            Reference := Element(Check);
            Working := kv.avm.Actor_Pool.Resolve(Reference);
            Reach := Working.Reachable;
            Added.Union(Reach - Could_Run);
            Next(Check);
         end loop;
      exit when Added = Empty_Set;
         --Ada.Text_IO.Put_Line("*** Garbage_Collection, adding some to Could_Run");
         Could_Run.Union(Added);
         Expand := Added;
      end loop;
      return Could_Run;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Expand_Reachable_Set): " & Exception_Information(Error));
         raise;
   end Expand_Reachable_Set;


   -----------------------------------------------------------------------------
   -- Internal method
   procedure Delete_Unreachable_Executables
      (Self      : in out Machine_Type;
       Reachable : in     kv.avm.Actor_References.Sets.Set) is

      Current   : kv.avm.Executable_Lists.Cursor_Type;
      Working   : kv.avm.Executables.Executable_Access;
      Reference : kv.avm.Actor_References.Actor_Reference_Type;

   begin
      Current := Self.Lists(Idle).Get_Last;
      while Current /= 0 loop
         Reference := Self.Lists(Idle).Get_Handle(Current).Get_Reference;
         if not Reachable.Contains(Reference) then
            Working := Self.Lists(Idle).Get(Current);
            Put_Line("Deleting unreachable idle executable " & Working.Image);
            Self.Lists(Idle).Delete(Current);
            kv.avm.Actor_Pool.Delete(Reference);
            Self.Reaped := Self.Reaped + 1;
         end if;
         Current := Current - 1;
      end loop;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Delete_Unreachable_Executables): " & Exception_Information(Error));
         raise;
   end Delete_Unreachable_Executables;


   -----------------------------------------------------------------------------
   -- Internal method
   procedure Garbage_Collection
      (Self : in out Machine_Type) is

      use kv.avm.Actor_References.Sets;

      Reachable : Set;

   begin
      Put_Line("Running Garbage_Collection");
      Reachable := Union(Self.Non_Idle, Self.Router.Reachable_From_Messages);
      --TODO: add all machine pending callbacks (not implemented yet)
      Reachable := Self.Expand_Reachable_Set(Reachable);
      Self.Delete_Unreachable_Executables(Reachable);
      Self.Old_Idle := Natural(Self.Lists(Idle).Get_Last);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Garbage_Collection): " & Exception_Information(Error));
         raise;
   end Garbage_Collection;


   -----------------------------------------------------------------------------
   -- Internal method
   procedure Activate_Instance
      (Self     : in out Machine_Type;
       Instance : in     kv.avm.Executables.Executable_Access) is
      Location : kv.avm.Executable_Lists.Cursor_Type;
   begin
      Location := Self.Lists(Deferred).Find(Instance);
      if Location /= 0 then
         Self.Lists(Active).Acquire_From(Location, Self.Lists(Deferred));
      else
         Location := Self.Lists(Idle).Find(Instance);
         if Location /= 0 then
            Self.Lists(Active).Acquire_From(Location, Self.Lists(Idle));
         else
            Location := Self.Lists(Blocked).Find(Instance);
            if Location /= 0 then
               Self.Lists(Active).Acquire_From(Location, Self.Lists(Blocked));
            end if;
         end if;
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Activate_Instance): " & Exception_Information(Error));
         raise;
   end Activate_Instance;


   -----------------------------------------------------------------------------
   procedure New_Actor
      (Self     : in out Machine_Type;
       Name     : in     String;
       Instance :    out kv.avm.Actor_References.Actor_Reference_Type) is

      Actor_Pointer : kv.avm.Actors.Actor_Access;
      Instance_Pointer : kv.avm.Executables.Executable_Access;
      Empty_Data : kv.avm.Tuples.Tuple_Type;

      use kv.avm.Actors;

   begin
      --Put_Line("kv.avm.machine.New_Actor "&Name);
      Actor_Pointer := kv.avm.Actors.Get_Actor_By_Name(Name);
      if Actor_Pointer = null then
         Put_Line("ERROR: kv.avm.machine.New_Actor, Actor_Pointer is null because <"&Name&"> could not be found.");
         raise Machine_Error;
      end if;
      Self.Factory.New_Executable(Actor_Pointer, Self'UNCHECKED_ACCESS, Instance_Pointer, Instance);
      Put_Line("Machine Instance Control: adding       " & Instance_Pointer.Image);
      Self.Lists(Active).Add(Instance_Pointer, Instance);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in New_Actor): " & Exception_Information(Error));
         raise;
   end New_Actor;

   -----------------------------------------------------------------------------
   procedure Post_Message
      (Self    : in out Machine_Type;
       Message : in     kv.avm.Messages.Message_Type;
       Status  :    out kv.avm.Control.Status_Type) is
   begin
      Self.Router.Post_Message(Message, Status);
   end Post_Message;

   -----------------------------------------------------------------------------
   procedure Post_Response
      (Self         : in out Machine_Type;
       Reply_To     : in     kv.avm.Actor_References.Actor_Reference_Type;
       Answer       : in     kv.avm.Tuples.Tuple_Type;
       Future       : in     Interfaces.Unsigned_32) is
   begin
      Self.Router.Post_Response(Reply_To, Answer, Future);
   end Post_Response;

   -----------------------------------------------------------------------------
   procedure Generate_Next_Future
      (Self   : in out Machine_Type;
       Future :    out Interfaces.Unsigned_32) is
   begin
      Self.Future := Self.Future + 1;
      Future := Self.Future;
   end Generate_Next_Future;

   -----------------------------------------------------------------------------
   procedure Trap_To_The_Machine
      (Self   : in out Machine_Type;
       Trap   : in     String;
       Data   : in     kv.avm.Registers.Register_Type;
       Answer :    out kv.avm.Registers.Register_Type;
       Status :    out kv.avm.Control.Status_Type) is
   begin
      Put_Line("Machine_Type.Trap_To_The_Machine called with name = '"&Trap&"'");
      Self.Capabilities.Execute(Trap, Self, Data, Answer, Status);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Trap_To_The_Machine): " & Exception_Information(Error));
         raise;
   end Trap_To_The_Machine;

   -----------------------------------------------------------------------------
   procedure Activate_Instance
      (Self     : in out Machine_Type;
       Instance : in     kv.avm.Actor_References.Actor_Reference_Type) is

      Executable : kv.avm.Executables.Executable_Access;

   begin
      Executable := kv.avm.actor_pool.Resolve(Instance);
      Self.Activate_Instance(Executable);
   end Activate_Instance;

   -----------------------------------------------------------------------------
   procedure Start_With
      (Self    : in out Machine_Type;
       Actor   : in     String;
       Message : in     String;
       Data    : in     kv.avm.Memories.Register_Array_Type) is

      Instance : kv.avm.Actor_References.Actor_Reference_Type;
      Content  : aliased kv.avm.Tuples.Tuple_Type;
      Empty_Tuple : aliased kv.avm.Tuples.Tuple_Type;
      Status : kv.avm.Control.Status_Type;
      Constructor_Message : kv.avm.Messages.Message_Type;
      Go_Message : kv.avm.Messages.Message_Type;

   begin
      Self.New_Actor(Actor, Instance);

      Empty_Tuple.Initialize;
      Empty_Tuple.Fold_Empty;

      Constructor_Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Instance,
          Message_Name => "CONSTRUCTOR",
          Data         => Empty_Tuple,
          Future       => kv.avm.Control.NO_FUTURE);
      Self.Post_Message
         (Message => Constructor_Message,
          Status  => Status);


      Content.Initialize;
      Content.Fold(Data);

      Go_Message.Initialize
         (Source       => kv.avm.Actor_References.Null_Reference,
          Reply_To     => kv.avm.Actor_References.Null_Reference,
          Destination  => Instance,
          Message_Name => Message,
          Data         => Content,
          Future       => kv.avm.Control.NO_FUTURE);
      Self.Post_Message
         (Message => Go_Message,
          Status  => Status);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Start_With): " & Exception_Information(Error));
         raise;
   end Start_With;


end kv.avm.Machines;
