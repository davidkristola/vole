with Interfaces;

with kv.avm.references; use kv.avm.references;
with kv.avm.Instructions;
with kv.avm.Registers;
with kv.avm.Frames;
with kv.avm.Control;

package kv.avm.Processors is

   Unimplemented_Error : exception;
   Frame_Stopped_Error : exception;

   type Processor_Type is tagged
      record
         Machine : kv.avm.Control.Control_Access;
         Failed_Assertion_Count : Natural;
      end record;
   type Processor_Access is access all Processor_Type;

   procedure Initialize
      (Self    : in out Processor_Type;
       Machine : in     kv.avm.control.Control_Access);

   procedure Step
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Status :    out kv.avm.Control.Status_Type);

   function Get_Failed_Assertion_Count(Self : Processor_Type) return Natural;
   function Get_Machine(Self : Processor_Type) return kv.avm.Control.Control_Access;

private
   procedure No_Op
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type);
   procedure Stop_Frame
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type);
   procedure Reply
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Jump
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Jump_Immediate -- jump_abs, jump_rel
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Set
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Branch
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Fold
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Peek
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type);
   procedure New_Actor
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type);
   procedure Compute
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Emit
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Self_Tail_X
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Halt_Actor
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Trap
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type);
   procedure Self_Call
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Super_Call
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Actor_Call
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type);
   procedure Format_5A2
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type);
   procedure Peek_Immediate
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type);
   procedure Assert
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);
   procedure Self_Send
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type);

end kv.avm.Processors;
