with Interfaces;

with kv.avm.Executables;
with kv.avm.Instructions;
with kv.avm.Registers;
with kv.avm.Actors;
with kv.avm.Processors;
with kv.avm.Frames;
with kv.avm.Actor_References;
with kv.avm.Actor_References.Sets;
with kv.avm.Messages;
with kv.avm.Tuples;
with kv.avm.Control;
with kv.avm.Memories;

package kv.avm.Test.Runners is

   type Behavior_Type is interface;
   type Behavior_Access is access all Behavior_Type'CLASS;

   -- This is a mock for unit testing the Machine.  There isn't any point
   -- in creating getters and setters for everything.
   type Runner_Type is new kv.avm.Executables.Executable_Interface with
      record
         ID       : Interfaces.Unsigned_32 := 0;
         PC       : Interfaces.Unsigned_32 := 0;
         CPU      : kv.avm.Processors.Processor_Access;
         Behavior : Behavior_Access;
         Running  : Boolean := False;
         Status   : kv.avm.Control.Status_Type := kv.avm.Control.Active;
         Last_Msg : kv.avm.Messages.Message_Type;
         Next     : kv.avm.Executables.Executable_Access;
      end record;
   type Runner_Access is access all Runner_Type;

   procedure Execute
      (Self   : in out Behavior_Type;
       Runner : in out Runner_Type'CLASS) is abstract;

   procedure Initialize
      (Self : in out Runner_Type;
       ID   : in     Interfaces.Unsigned_32;
       Next : in     Runner_Access);

   procedure Set_Behavior_Send
      (Self        : in out Runner_Type;
       Destination : in     kv.avm.Actor_References.Actor_Reference_Type;
       Cycle_Time  : in     Natural);

   procedure Set_Behavior_Spawn
      (Self       : in out Runner_Type;
       Cycle_Time : in     Natural);


   overriding
   procedure Process_Message
      (Self    : in out Runner_Type;
       Message : in     kv.avm.Messages.Message_Type);

   overriding
   procedure Process_Gosub
      (Self      : access Runner_Type;
       Tailcall  : in     Boolean;
       Supercall : in     Boolean;
       Reply_To  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Method    : in     kv.avm.Registers.String_Type;
       Data      : access constant kv.avm.Memories.Register_Set_Type;
       Future    : in     Interfaces.Unsigned_32);

   overriding
   function Can_Accept_Message_Now(Self : Runner_Type; Message : kv.avm.Messages.Message_Type) return Boolean;

   overriding
   function Program_Counter
      (Self : in    Runner_Type) return Interfaces.Unsigned_32;

   overriding
   function Is_Running
      (Self : in    Runner_Type) return Boolean;

   overriding
   procedure Step
      (Self      : access Runner_Type;
       Processor : access kv.avm.Processors.Processor_Type;
       Status    :    out kv.avm.Control.Status_Type);

   overriding
   procedure Process_Internal_Response
      (Self   : in out Runner_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type);

   overriding
   procedure Resolve_Future
      (Self   : in out Runner_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type;
       Future : in     Interfaces.Unsigned_32);

   overriding
   procedure Halt_Actor
      (Self : in out Runner_Type);

   overriding
   function Reachable(Self : Runner_Type) return kv.avm.Actor_References.Sets.Set;

   overriding
   function Image(Self : Runner_Type) return String;

   overriding
   function Debug_Info(Self : Runner_Type) return String;


   type Runner_Factory is new kv.avm.Executables.Executable_Factory with
      record
         Count : Interfaces.Unsigned_32 := 0;
         Head  : Runner_Access;
      end record;

   overriding
   procedure New_Executable
      (Self       : in out Runner_Factory;
       Actor      : in     kv.avm.Actors.Actor_Access;
       Machine    : in     kv.avm.Control.Control_Access;
       Executable :    out kv.avm.Executables.Executable_Access;
       Reference  :    out kv.avm.Actor_References.Actor_Reference_Type);

   function Get_Allocated_Count(Self : Runner_Factory) return Natural;
   function Get_Runner_By_ID(Self : Runner_Factory; ID : Interfaces.Unsigned_32) return Runner_Access;

private

   procedure Free_Behavior
      (Self : in out Runner_Type);

   type Base_Behavior_Type is abstract new Behavior_Type with
      record
         Cycle_Time : Natural;
         Remaining  : Natural;
      end record;

   procedure Do_It_Now
      (Self   : in out Base_Behavior_Type;
       Runner : in out Runner_Type'CLASS) is null;

   procedure Execute
      (Self   : in out Base_Behavior_Type;
       Runner : in out Runner_Type'CLASS);


end kv.avm.Test.Runners;
