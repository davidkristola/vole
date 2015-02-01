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
with kv.avm.control;
with kv.avm.Memories;

package kv.avm.Instances is

   use Interfaces;
   use kv.avm.Messages;

   type Instance_Type is new kv.avm.Executables.Executable_Interface with private;
   type Instance_Access is access all Instance_Type;

   function "+"(RHS : Instance_Access) return kv.avm.Executables.Executable_Access;

   procedure Initialize
      (Self    : access Instance_Type;
       Actor   : in     kv.avm.Actors.Actor_Access;
       Memory  : in     kv.avm.Memories.Memory_Type;
       Myself  : in     kv.avm.Actor_References.Actor_Reference_Type);

   -- Routine used in unit tests
   function Get_Frame(Self : Instance_Type) return kv.avm.Frames.Frame_Access;

   overriding
   procedure Process_Message
      (Self    : in out Instance_Type;
       Message : in     kv.avm.Messages.Message_Type);

   overriding
   procedure Process_Gosub
      (Self      : access Instance_Type;
       Tailcall  : in     Boolean;
       Supercall : in     Boolean;
       Reply_To  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Method    : in     kv.avm.Registers.String_Type;
       Data      : access constant kv.avm.Memories.Register_Set_Type;
       Future    : in     Interfaces.Unsigned_32);

   overriding
   function Can_Accept_Message_Now(Self : Instance_Type; Message : kv.avm.Messages.Message_Type) return Boolean;

   overriding
   function Program_Counter
      (Self : in    Instance_Type) return Interfaces.Unsigned_32;

   overriding
   function Is_Running
      (Self : in    Instance_Type) return Boolean;

   overriding
   procedure Step
      (Self      : access Instance_Type;
       Processor : access kv.avm.Processors.Processor_Type;
       Status    :    out kv.avm.Control.Status_Type);

   overriding
   procedure Process_Internal_Response
      (Self   : in out Instance_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type);

   overriding
   procedure Resolve_Future
      (Self   : in out Instance_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type;
       Future : in     Interfaces.Unsigned_32);

   function Alive(Self : Instance_Type) return Boolean;

   overriding
   procedure Halt_Actor
      (Self : in out Instance_Type);

   overriding
   function Reachable(Self : Instance_Type) return kv.avm.Actor_References.Sets.Set;

   overriding
   function Image(Self : Instance_Type) return String;

   overriding
   function Debug_Info(Self : Instance_Type) return String;


   type Instance_Factory is new kv.avm.Executables.Executable_Factory with null record;

   overriding
   procedure New_Executable
      (Self       : in out Instance_Factory;
       Actor      : in     kv.avm.Actors.Actor_Access;
       Machine    : in     kv.avm.Control.Control_Access;
       Executable :    out kv.avm.Executables.Executable_Access;
       Reference  :    out kv.avm.Actor_References.Actor_Reference_Type);

private

   type Instance_Type is new kv.avm.Executables.Executable_Interface with
      record
         Actor      : kv.avm.Actors.Actor_Access;
         Myself     : kv.avm.Actor_References.Actor_Reference_Type;
         Pc         : Interfaces.Unsigned_32;
         Memory     : kv.avm.Memories.Memory_Type;
         Attributes : kv.avm.Memories.Register_Array_Type;
         Constants  : kv.avm.Memories.Register_Array_Type;
         Frame      : kv.avm.Frames.Frame_Access;
         Alive      : Boolean;
      end record;

end kv.avm.Instances;
