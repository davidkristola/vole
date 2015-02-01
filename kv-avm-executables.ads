with Interfaces;

with kv.avm.Actors;
with kv.avm.Control;
with kv.avm.Tuples;
with kv.avm.Messages;
with kv.avm.Registers;
with kv.avm.Memories;
with kv.avm.Actor_References;
limited with kv.avm.Processors;
with kv.avm.Actor_References.Sets;

package kv.avm.Executables is

   Corrupt_Executable_Error : exception;
   Unimplemented_Error : exception;

   type Executable_Interface is interface;
   type Executable_Access is access all Executable_Interface'CLASS;

   function Is_Running(Self : Executable_Interface) return Boolean is abstract;

   function Program_Counter
      (Self : in    Executable_Interface) return Interfaces.Unsigned_32 is abstract;

   procedure Step
      (Self      : access Executable_Interface;
       Processor : access kv.avm.Processors.Processor_Type;
       Status    :    out kv.avm.Control.Status_Type) is abstract;

   function Can_Accept_Message_Now(Self : Executable_Interface; Message : kv.avm.Messages.Message_Type) return Boolean is abstract;

   procedure Process_Message
      (Self    : in out Executable_Interface;
       Message : in     kv.avm.Messages.Message_Type) is abstract;

   procedure Process_Gosub
      (Self      : access Executable_Interface;
       Tailcall  : in     Boolean;
       Supercall : in     Boolean;
       Reply_To  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Method    : in     kv.avm.Registers.String_Type;
       Data      : access constant kv.avm.Memories.Register_Set_Type;
       Future    : in     Interfaces.Unsigned_32) is abstract;

   procedure Process_Internal_Response
      (Self   : in out Executable_Interface;
       Answer : in     kv.avm.Tuples.Tuple_Type) is abstract;

   procedure Resolve_Future
      (Self   : in out Executable_Interface;
       Answer : in     kv.avm.Tuples.Tuple_Type;
       Future : in     Interfaces.Unsigned_32) is abstract;

   procedure Halt_Actor
      (Self : in out Executable_Interface) is abstract;

   function Reachable(Self : Executable_Interface) return kv.avm.Actor_References.Sets.Set is abstract;

   function Image(Self : Executable_Interface) return String is abstract;

   function Debug_Info(Self : Executable_Interface) return String is abstract;



   type Executable_Factory is interface;
   type Factory_Access is access all Executable_Factory'CLASS;
   procedure New_Executable
      (Self       : in out Executable_Factory;
       Actor      : in     kv.avm.Actors.Actor_Access;
       Machine    : in     kv.avm.Control.Control_Access;
       Executable :    out Executable_Access;
       Reference  :    out kv.avm.Actor_References.Actor_Reference_Type) is abstract;

end kv.avm.Executables;
