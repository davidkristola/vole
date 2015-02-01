with Ada.Finalization;
with Interfaces;

with kv.avm.references; use kv.avm.references;
with kv.avm.Instructions;
with kv.avm.Registers;
with kv.avm.Memories;
with kv.avm.Actor_References;
with kv.avm.Actor_References.Sets;
with kv.avm.Tuples;

package kv.avm.Frames is

   Program_Counter_Error : exception;
   Invalid_Source_Error : exception;
   Operand_Mismatch_Error : exception;
   Fixed_Target_Error : exception;

   type Frame_Type is tagged private;
   type Frame_Access is access all Frame_Type;

   procedure Initialize
      (Self     : in out Frame_Type;
       Instance : in     kv.avm.Actor_References.Actor_Reference_Type;
       Name     : in     kv.avm.Registers.String_Type;
       Invoker  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Future   : in     Interfaces.Unsigned_32;
       Code     : in     kv.avm.Instructions.Code_Access;
       Memory   : in     kv.avm.Memories.Memory_Type;
       Next     : in     Frame_Access := null);

   function Program_Counter
      (Self : in    Frame_Type) return Interfaces.Unsigned_32;

   procedure Set_Program_Counter
      (Self : in out Frame_Type;
       Pc   : in     Interfaces.Unsigned_32);

   procedure Increment_Program_Counter
      (Self : in out Frame_Type);

   function Is_Done(Self : Frame_Type) return Boolean;

   function Get_Running
      (Self : in    Frame_Type) return Boolean;

   procedure Set_Running
      (Self    : in out Frame_Type;
       Running : in     Boolean);

   procedure Set_Blocked
      (Self    : in out Frame_Type;
       Blocked : in     Boolean);

   function Fetch_Instruction
      (Self : in     Frame_Type) return kv.avm.Instructions.Instruction_Type;

   procedure Fetch_And_Increment -- Fetch an instruction and increment the program counter by one
      (Self        : in out Frame_Type;
       Instruction :    out kv.avm.Instructions.Instruction_Type);

   procedure Vet_Operands
      (Self   : in out Frame_Type;
       Target : in     Reference_Type;
       Source : in     Reference_Type);

   procedure Vet_Operands
      (Self     : in out Frame_Type;
       Target   : in     Reference_Type;
       Source_1 : in     Reference_Type;
       Source_2 : in     Reference_Type);

   function Get
      (Self : in     Frame_Type;
       Ref  : in     Reference_Type) return kv.avm.Registers.Register_Type;

   procedure Set
      (Self  : in out Frame_Type;
       Ref   : in     Reference_Type;
       Value : in     kv.avm.Registers.Register_Type);

   function Get_Invoker(Self : Frame_Type) return kv.avm.Actor_References.Actor_Reference_Type;
   function Get_Instance(Self : Frame_Type) return kv.avm.Actor_References.Actor_Reference_Type;
   function Is_Self_Replying(Self : Frame_Type) return Boolean;
   function Get_Name(Self : Frame_Type) return String;

   procedure Set_Invoker -- Test routine
      (Self    : in out Frame_Type;
       Invoker : in     kv.avm.Actor_References.Actor_Reference_Type);

   procedure Set_Future -- Test routine
      (Self   : in out Frame_Type;
       Future : in     Interfaces.Unsigned_32);

   function Fold
      (Self : in     Frame_Type;
       Ref  : in     Reference_Type) return kv.avm.Tuples.Tuple_Type;


   procedure Process_Gosub_Response
      (Self   : in out Frame_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type);

   procedure Set_Reply_Information
      (Self     : in out Frame_Type;
       Reply_To : in     Reference_Type;
       Future   : in     Interfaces.Unsigned_32);

   function Get_Invoker_Future(self : Frame_Type) return Interfaces.Unsigned_32;

   procedure Process_Gosub
      (Self         : in out Frame_Type;
       Tail_Call    : in     Boolean;
       Supercall    : in     Boolean;
       Message_Name : in     kv.avm.Registers.String_Type;
       Data         : in     kv.avm.Tuples.Tuple_Type;
       Future       : in     Interfaces.Unsigned_32);

   procedure Resolve_Future
      (Self   : in out Frame_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type;
       Future : in     Interfaces.Unsigned_32);

   function Reachable(Self : Frame_Type) return kv.avm.Actor_References.Sets.Set;

   function Get_Next(Self : Frame_Type) return Frame_Access;

   procedure Halt_Actor
      (Self : in out Frame_Type);

   function Image(Self : Frame_Type) return String;

   function Debug_Info(Self : Frame_Type) return String;

   procedure Prepare_For_Deletion(Self : in out Frame_Type);

private

   type Frame_Type is tagged
      record
         Instance : kv.avm.Actor_References.Actor_Reference_Type;
         Invoker  : kv.avm.Actor_References.Actor_Reference_Type;
         Future   : Interfaces.Unsigned_32; -- Future of the Invoker
         Future_R : Reference_Type;
         Name     : kv.avm.Registers.String_Type;
         Code     : kv.avm.Instructions.Code_Access;
         Memory   : kv.avm.Memories.Memory_Type;
         Pc       : Interfaces.Unsigned_32;
         RunningF : Boolean;
         Blocked  : Boolean;
         Depth    : Positive;
         Next     : Frame_Access;
      end record;

end kv.avm.Frames;
