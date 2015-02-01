with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with kv.avm.Log; use kv.avm.Log;
with kv.avm.references; use kv.avm.references;
with kv.avm.Actor_References;
with kv.avm.actor_pool;
with kv.avm.Methods;

package body kv.avm.Instances is

   use kv.avm.Instructions;
   use kv.avm.Registers;
   use kv.avm.Frames;

   type Constant_Access is access constant kv.avm.Memories.Register_Set_Type;

   function Convert is new Ada.Unchecked_Conversion
      (Source => Constant_Access,
       Target => kv.avm.Memories.Register_Set_Access);


   Fake_Name : aliased constant String := "Subroutine";

   -----------------------------------------------------------------------------
   function "+"(RHS : Instance_Access) return kv.avm.Executables.Executable_Access is
   begin
      return kv.avm.Executables.Executable_Access(RHS);
   end "+";

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self    : access Instance_Type;
       Actor   : in     kv.avm.Actors.Actor_Access;
       Memory  : in     kv.avm.Memories.Memory_Type;
       Myself  : in     kv.avm.Actor_References.Actor_Reference_Type) is
      use kv.avm.control;
      use kv.avm.Memories;
   begin
      Self.Alive := True;
      Self.Actor := Actor;
      Self.Memory := Memory;
      Self.Myself := Myself;
      if not Memory.Get(Attribute).Is_Set then
         Self.Attributes.Allocate(64);
      else
         Self.Attributes := Register_Array_Type(Memory.Get(Attribute)); -- Use the test set
      end if;
      if not Memory.Get(Fixed).Is_Set then
         Self.Constants := Actor.Get_Constants;
      else
         Self.Constants := Register_Array_Type(Memory.Get(Fixed)); -- Use the test set
      end if;
   end Initialize;

   -----------------------------------------------------------------------------
   function Get_Frame(Self : Instance_Type) return kv.avm.Frames.Frame_Access is
   begin
      return Self.Frame;
   end Get_Frame;

   -----------------------------------------------------------------------------
   procedure Process_Message
      (Self    : in out Instance_Type;
       Message : in     kv.avm.Messages.Message_Type) is

      Ref           : kv.avm.Actor_References.Actor_Reference_Type;
      Current_Frame : kv.avm.Frames.Frame_Access;
      Memories      : kv.avm.Memories.Memory_Type;
      Registers     : kv.avm.Memories.Register_Array_Type;

      function Log_Entry return String is
      begin
         return Self.Image&".Process_Message "&Message.Get_Name&
               ", Invoker="&Message.Get_Source.Image&
               ", Future="&Interfaces.Unsigned_32'IMAGE(Message.Get_Future);
      end Log_Entry;

      use kv.avm.Registers;
      use kv.avm.Memories;

   begin
      Log_If(Log_Entry'ACCESS);
      Current_Frame := Self.Frame;
      Self.Frame := new kv.avm.Frames.Frame_Type;
      Registers := Register_Array_Type(Self.Memory.Get(Local)); --!@#$ copy test set
      Memories.Set(Local, Registers);
      Memories.Set(Attribute, Self.Attributes);
      Memories.Set(Fixed, Self.Constants);
      Registers.Set(Convert(Constant_Access(Message.Get_Data.Unfolded)));
      Memories.Set(Input, Registers);
      if not Memories.Get(Local).Is_Set then
         Registers.Allocate(64);
         Memories.Set(Local, Registers);
      end if;

      Self.Frame.Initialize
         (Instance => Self.Myself,
          Name => +Message.Get_Name,
          Invoker => Message.Get_Reply_To, -- Replies go back to the invoker
          Future => Message.Get_Future,
          Code => Self.Actor.Get_Method(Message.Get_Name).Get_Code,
          Memory => Memories,
          Next => Current_Frame);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Process_Message): " & Exception_Information(Error));
         raise;
   end Process_Message;

   -----------------------------------------------------------------------------
   procedure Process_Gosub
      (Self      : access Instance_Type;
       Tailcall  : in     Boolean;
       Supercall : in     Boolean;
       Reply_To  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Method    : in     kv.avm.Registers.String_Type;
       Data      : access constant kv.avm.Memories.Register_Set_Type;
       Future    : in     Interfaces.Unsigned_32) is

      Current_Frame : kv.avm.Frames.Frame_Access;
      Memories      : kv.avm.Memories.Memory_Type;
      Registers     : kv.avm.Memories.Register_Array_Type;
      Method_Object : kv.avm.Methods.Method_Access;

      function Log_Entry return String is
         Is_Tail  : String := " Tail";
         Is_Super : String := " Super";
      begin
         if not Tailcall then
            Is_Tail := " Push";
         end if;
         if not Supercall then
            Is_Super := " Self ";
         end if;
         return Self.Image & ".Process_Gosub " & (+Method) & Is_Tail & Is_Super;
      end Log_Entry;

      use kv.avm.Registers;
      use kv.avm.Memories;

   begin
      --Put_Line(Self.Image & ".Process_Gosub " & (+Method));
      Log_If(Log_Entry'ACCESS);
      Registers := Register_Array_Type(Self.Memory.Get(Local)); --!@#$ copy test set
      Memories.Set(Local, Registers);
      Memories.Set(Attribute, Self.Attributes);
      Memories.Set(Fixed, Self.Constants);
      Registers.Set(Convert(Constant_Access(Data)));
      Memories.Set(Input, Registers);
      if not Memories.Get(Local).Is_Set then
         Registers.Allocate(64);
         Memories.Set(Local, Registers);
      end if;
      if Tailcall then
         -- Reuse the frame.
         Current_Frame := Self.Frame.Get_Next; -- Keep the current parent frame.
      else
         -- Allocate a new frame.
         Current_Frame := Self.Frame;
         Self.Frame := new kv.avm.Frames.Frame_Type; --!@#$ leak
      end if;
      if Supercall then
         Method_Object := Self.Actor.Get_Parent.Get_Method(+Method);
      else
         Method_Object := Self.Actor.Get_Method(+Method);
      end if;
      Self.Frame.Initialize
         (Instance => Self.Myself,
          Name     => Method,
          Invoker  => Reply_To, -- Replies go back to the invoker
          Future   => Future,
          Code     => Method_Object.Get_Code,
          Memory   => Memories,
          Next     => Current_Frame);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Process_Gosub): " & Exception_Information(Error));
         raise;
   end Process_Gosub;

   -----------------------------------------------------------------------------
   function Can_Accept_Message_Now(Self : Instance_Type; Message : kv.avm.Messages.Message_Type) return Boolean is
      Method : kv.avm.Methods.Method_Access;
      Predicate : kv.avm.References.Offset_Type;
      Register : kv.avm.Registers.Register_Type;
      use kv.avm.Methods;
   begin
      if Self.Frame = null then
         Method := Self.Actor.Get_Method(Message.Get_Name); -- Recurs up inheritance chain
         if Method = null then
            Put_Line("Could not find message " & Message.Get_Name);
            return False;
         else
            if Method.Has_Predicate then
               Predicate := Method.Get_Predicate;
               Register := Self.Attributes.Read(Predicate);
               Put_Line("Machine Predicate check for " & Message.Get_Name & ", accapt: " & Boolean'IMAGE(Register.Bit));
               return Register.Bit;
            else
               return True;
            end if;
         end if;
      end if;
      Put_Line("Can't accept message " & Message.Get_Name & " because frame " & Self.Frame.Image & " is running.");
      return False;
   end Can_Accept_Message_Now;


   -----------------------------------------------------------------------------
   function Program_Counter
      (Self : in    Instance_Type) return Interfaces.Unsigned_32 is
   begin
      if Self.Frame = null then
         return 0;
      end if;
      return Self.Frame.Program_Counter;
   end Program_Counter;

   -----------------------------------------------------------------------------
   function Is_Running
      (Self : in    Instance_Type) return Boolean is

   begin
      if not Self.Alive then
         return False;
      end if;
      if Self.Frame = null then
         return False;
      end if;
      --!@#$ what about blocked and deferred?
      --!@#$ Machine, which is the only thing that checks this, uses it in both senses.  :-(
      return True; -- We are not dead or idle so we are running.
   end Is_Running;

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(kv.avm.Frames.Frame_Type, kv.avm.Frames.Frame_Access);

   -----------------------------------------------------------------------------
   procedure Step
      (Self      : access Instance_Type;
       Processor : access kv.avm.Processors.Processor_Type;
       Status    :    out kv.avm.Control.Status_Type) is

      Done_Frame : kv.avm.Frames.Frame_Access;
      Message : kv.avm.Messages.Message_Type;

   begin
      --Put_Line("kv.avm.instance.Step "&Self.Image);
      Processor.Step(Self.Frame, Status);
      if Self.Frame.Is_Done then
         --Put_Line("Frame " & Self.Image & "@" & Self.Frame.Image & " has completed processing, removing it from the stack.");
         Put_Line("Frame " & Self.Frame.Image & " has completed processing, removing it from the stack.");
         -- This frame is done and needs to be deleted.
         Done_Frame := Self.Frame;
         Self.Frame := Self.Frame.Get_Next;
         -- Free the frame
         Done_Frame.Prepare_For_Deletion;
         Free(Done_Frame);
         if Self.Frame /= null then
            --Put_Line("Frame " & Self.Image & "@" & Self.Frame.Image & " has resumed.");
            Put_Line("Frame " & Self.Frame.Image & " has resumed.");
         end if;
         --TODO: figure out why this breaks the unit tests
         --if Self.Frame = null then
         --   Status := kv.avm.Control.Idle;
         --end if;
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Step): " & Exception_Information(Error));
         raise;
   end Step;

   -----------------------------------------------------------------------------
   procedure Process_Internal_Response
      (Self   : in out Instance_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type) is

      Done_Frame : kv.avm.Frames.Frame_Access;

   begin
      --Put_Line("kv.avm.instance.Process_Response");
      Done_Frame := Self.Frame;
      Self.Frame := Self.Frame.Get_Next;
      Free(Done_Frame);
      -- Call it's Process_Response to fill in the answer
      Self.Frame.Process_Gosub_Response(Answer);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Process_Internal_Response): " & Exception_Information(Error));
         raise;
   end Process_Internal_Response;


   -----------------------------------------------------------------------------
   procedure Resolve_Future
      (Self   : in out Instance_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type;
       Future : in     Interfaces.Unsigned_32) is

      Index : kv.avm.References.Offset_Type;
      Found : Boolean;

   begin
      --!@#$ this future could be in any local register in any frame or in an instance register
      Self.Attributes.Find_Future(Future, Found, Index);
      if Found then
         Self.Attributes.Write(Index, (Format => kv.avm.Registers.Tuple, folded_tuple => Answer));
         if Self.Frame /= null then
            Self.Frame.Set_Blocked(False);
         end if;
         return;
      end if;
      Put_Line("Future "&Interfaces.Unsigned_32'IMAGE(Future)&" not in an attribute, searching frames.");
      if Self.Frame = null then
         raise kv.avm.Executables.Corrupt_Executable_Error;
      else
         Self.Frame.Resolve_Future(Answer, Future);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in Resolve_Future): " & Exception_Information(Error));
         raise;
   end Resolve_Future;


   -----------------------------------------------------------------------------
   function Alive(Self : Instance_Type) return Boolean is
   begin
      return Self.Alive;
   end Alive;

   -----------------------------------------------------------------------------
   procedure Halt_Actor
      (Self : in out Instance_Type) is
   begin
      Self.Alive := False;
   end Halt_Actor;

   -----------------------------------------------------------------------------
   function Reachable(Self : Instance_Type) return kv.avm.Actor_References.Sets.Set is
      Can_Reach : kv.avm.Actor_References.Sets.Set := kv.avm.Actor_References.Sets.Empty_Set;
   begin
      Can_Reach.Include(Self.Myself);
      Can_Reach.Union(Self.Attributes.Reachable);
      Can_Reach.Union(Self.Constants.Reachable);
      if Self.Frame /= null then
         Can_Reach.Union(Self.Frame.Reachable);
      end if;
      return Can_Reach;
   end Reachable;

   -----------------------------------------------------------------------------
   function Image(Self : Instance_Type) return String is
   begin
      return Self.Actor.Image & Self.Myself.Image;
   end Image;

   -----------------------------------------------------------------------------
   function Debug_Info(Self : Instance_Type) return String is
   begin
      if Self.Frame = null then
         return Self.Actor.Image & Self.Myself.Image & " (no frame)";
      else
         return Self.Actor.Image & Self.Myself.Image & ", Frame:" & Self.Frame.Debug_Info;
      end if;
   end Debug_Info;




   -----------------------------------------------------------------------------
   procedure New_Executable
      (Self       : in out Instance_Factory;
       Actor      : in     kv.avm.Actors.Actor_Access;
       Machine    : in     kv.avm.Control.Control_Access;
       Executable :    out kv.avm.Executables.Executable_Access;
       Reference  :    out kv.avm.Actor_References.Actor_Reference_Type) is

      use kv.avm.Control;
      Empty : kv.avm.Memories.Memory_Type;
      Instance : Instance_Access;

   begin
      if Machine = null then
         Put_Error("WARNING: kv.avm.Instances.New_Executable called with Machine = null!");
      end if;
      Instance := new Instance_Type;
      kv.avm.Actor_Pool.Add(+Instance, Reference);
      Instance.Initialize(Actor, Empty, Reference);
      Executable := kv.avm.Executables.Executable_Access(Instance);
   exception
      when Error: others =>
         Put_Error("EXCEPTION (in New_Executable): " & Exception_Information(Error));
         raise;
   end New_Executable;


end kv.avm.Instances;
