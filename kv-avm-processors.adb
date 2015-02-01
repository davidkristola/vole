with Interfaces; use Interfaces;
with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with kv.avm.Log; use kv.avm.Log;
with kv.avm.Memories;
with kv.avm.Tuples;
with kv.avm.Actor_References;
with kv.avm.Messages;
with kv.avm.Control; use kv.avm.Control;
with kv.avm.Executables;
with kv.avm.actor_pool;

package body kv.avm.Processors is

   use kv.avm.Instructions;
   use kv.avm.Registers;

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self    : in out Processor_Type;
       Machine : in     kv.avm.control.Control_Access) is
   begin
      Self.Machine := Machine;
      Self.Failed_Assertion_Count := 0;
   end Initialize;

   -----------------------------------------------------------------------------
   procedure Step
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Status :    out kv.avm.Control.Status_Type) is

      Instruction : kv.avm.Instructions.Instruction_Type;
      Program_Counter : Interfaces.Unsigned_32;

      function Log_Entry return String is
      begin
         return "kv.avm.processor.Step "&Frame.Debug_Info;
      end Log_Entry;

      use kv.avm.Control;

   begin
      Status := kv.avm.Control.Active;
      Log_If(Log_Entry'Access);
      Program_Counter := Frame.Program_Counter;
      Frame.Fetch_And_Increment(Instruction);
      case Instruction.Op_Code is
         when No_Op =>
            Self.No_Op(Frame);
         when Stop_Frame =>
            Self.Stop_Frame(Frame);
         when Reply =>
            Self.Reply(Frame, Instruction);
         when Jump =>
            Self.Jump(Frame, Instruction);
         when Jump_Abs | Jump_Rel =>
            Self.Jump_Immediate(Frame, Instruction);
         when Set =>
            Self.Set(Frame, Instruction);
         when Branch_Abs .. Branch_Neq =>
            Self.Branch(Frame, Instruction);
         when Fold =>
            Self.Fold(Frame, Instruction);
         when Peek =>
            Self.Peek(Frame, Instruction, Status);
         when New_Actor =>
            Self.New_Actor(Frame, Instruction, Status);
         when Compute =>
            Self.Compute(Frame, Instruction);
         when Emit =>
            Self.Emit(Frame, Instruction);
         when Self_Tail_Send | Self_Tail_Call =>
            Self.Self_Tail_X(Frame, Instruction);
         when Halt_Actor =>
            Self.Halt_Actor(Frame, Instruction);
         when Trap =>
            Self.Trap(Frame, Instruction, Status);
         when Actor_Call =>
            Self.Actor_Call(Frame, Instruction, Status);
         when Self_Call =>
            Self.Self_Call(Frame, Instruction);
         when Super_Call =>
            Self.Super_Call(Frame, Instruction);
         when Self_Send =>
            Self.Self_Send(Frame, Instruction);
         when format_5a2_type =>
            Self.Format_5A2(Frame, Instruction, Status);
         when Peek_Immediate =>
            Self.Peek_Immediate(Frame, Instruction, Status);
         when Assert =>
            Self.Assert(Frame, Instruction);
         when others =>
            raise Unimplemented_Error;
      end case;
      if Status = kv.avm.Control.Blocked then
         Put_Line("Frame "&Frame.Image&" returned Blocked status.");
         Frame.Set_Program_Counter(Program_Counter);
         Frame.Set_Blocked(True);
      elsif Status = kv.avm.Control.Deferred then
         Put_Line("Frame "&Frame.Image&" returned Deferred status.");
         Frame.Set_Program_Counter(Program_Counter);
      end if;
   exception
      when Error: others =>
         Put_Line("EXCEPTION: "&Exception_Information(Error));
         Frame.Set_Program_Counter(Program_Counter); -- Return the progam counter to the value it had on entry
         raise;
   end Step;


   -----------------------------------------------------------------------------
   function Get_Failed_Assertion_Count(Self : Processor_Type) return Natural is
   begin
      return Self.Failed_Assertion_Count;
   end Get_Failed_Assertion_Count;


   -----------------------------------------------------------------------------
   function Get_Machine(Self : Processor_Type) return kv.avm.Control.Control_Access is
   begin
      return Self.Machine;
   end Get_Machine;


   -----------------------------------------------------------------------------
   procedure No_Op
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type) is
   begin
      null;
   end No_Op;

   -----------------------------------------------------------------------------
   procedure Stop_Frame
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type) is
   begin
      Frame.Set_Program_Counter(Frame.Program_Counter - 1);
      Frame.Set_Running(False);
   end Stop_Frame;





   -----------------------------------------------------------------------------
   function Get_Or_Make_Tuple
      (Frame : in     kv.avm.Frames.Frame_Type;
       Index : in     kv.avm.references.reference_type) return kv.avm.Tuples.Tuple_Type is

      Data : kv.avm.Tuples.Tuple_Type;

   begin
      if Frame.Get(Index).Format = kv.avm.Registers.Tuple then
         return Frame.Get(Index).folded_tuple;
      elsif Frame.Get(Index).Format = kv.avm.Registers.Tuple_Map then
         return Frame.Fold(Index);
      else
         declare
            Register : aliased kv.avm.Registers.Register_Type := Frame.Get(Index);
         begin
            Data.Fold_One(Register'ACCESS);
         end;
         return Data;
      end if;
   end Get_Or_Make_Tuple;






   -----------------------------------------------------------------------------
   procedure Reply
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Data : kv.avm.Tuples.Tuple_Type;
      My_Instance : kv.avm.Executables.Executable_Access;
      use kv.avm.control;

   begin
      Frame.Set_Running(False);
      -- If the return value is a tuple, send it...
      Data := Get_Or_Make_Tuple(Frame.all, Instr.value);
--      if Frame.Get(Instr.value).Format = kv.avm.Registers.Tuple then
--         Data := Frame.Get(Instr.value).folded_tuple;
--      else
--         --TODO: check to see of this is a tuple map
--         -- Otherwise build a singleton tuple from the answer and send that.
--         -- Technically, this is a convenience function to avoid a FOLD instruction
--         -- before each reply, but since it is expected that most returns are
--         -- a single value, this should be worthwhile.
--         declare
--            Register : aliased kv.avm.Registers.Register_Type := Frame.Get(Instr.value);
--         begin
--            Data.Fold_One(Register'ACCESS);
--         end;
--      end if;
      if Frame.Is_Self_Replying then
         Put_Line("Reply to self!");
         My_Instance := kv.avm.actor_pool.Resolve(Frame.Get_Instance);
         My_Instance.Process_Internal_Response(Data);
      else
         if Self.Machine = null then
            Put_Line("ERROR: Machine is not set!");
         end if;
         Self.Machine.Post_Response
            (Reply_To => Frame.Get_Invoker,
             Answer   => Data,
             Future   => Frame.Get_Invoker_Future);
      end if;
   end Reply;

   -----------------------------------------------------------------------------
   procedure Set
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is
   begin
      --!@#$ This is where we would put in type checking, etc.
      Frame.Vet_Operands(Instr.Lhs, Instr.Rhs);
      Frame.Set(Instr.Lhs, Frame.Get(Instr.Rhs));
   end Set;

   -----------------------------------------------------------------------------
   procedure Jump
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Destination : kv.avm.Registers.Register_Type;

   begin
      Destination := Frame.Get(Instr.Value);
      Frame.Set_Program_Counter(Interfaces.Unsigned_32(Destination.unsigned_value));
   end Jump;

   -----------------------------------------------------------------------------
   procedure Jump_Immediate
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Jump_Target : Interfaces.Unsigned_32;

   begin
      if Instr.op_code = jump_abs then
         Jump_Target := Instr.jump;
      else -- Instr.op_code = branch_rel
         -- Target is a signed integer stored in an unsigned field
         -- Simply adding target to the program
         -- counter produces the correct result.
         Jump_Target := Frame.Program_Counter + Instr.jump;
      end if;
      Frame.Set_Program_Counter(Jump_Target);
   end Jump_Immediate;

   -----------------------------------------------------------------------------
   generic
      with function Op(Lhs : Interfaces.Unsigned_64; Rhs : Interfaces.Unsigned_64) return Interfaces.Unsigned_64;
   function U_Op(Lhs : Register_Type; Rhs : Register_Type) return Register_Type;
   function U_Op(Lhs : Register_Type; Rhs : Register_Type) return Register_Type is
      Intermediate : Interfaces.Unsigned_64;
   begin
      Intermediate := Op(Lhs.unsigned_value, Rhs.unsigned_value);
      return (format => Unsigned_Integer, unsigned_value => Intermediate);
   end U_Op;
   function U_Add is new U_Op(Interfaces."+");
   function U_Sub is new U_Op(Interfaces."-");
   function U_Mul is new U_Op(Interfaces."*");
   function U_Div is new U_Op(Interfaces."/");

   -----------------------------------------------------------------------------
   generic
      with function Op(Lhs : Interfaces.Integer_64; Rhs : Interfaces.Integer_64) return Interfaces.Integer_64;
   function S_Op(Lhs : Register_Type; Rhs : Register_Type) return Register_Type;
   function S_Op(Lhs : Register_Type; Rhs : Register_Type) return Register_Type is
      Intermediate : Interfaces.Integer_64;
   begin
      Intermediate := Op(Lhs.signed_value, Rhs.signed_value);
      return (format => Signed_Integer, signed_value => Intermediate);
   end S_Op;
   function S_Add is new S_Op(Interfaces."+");
   function S_Sub is new S_Op(Interfaces."-");
   function S_Mul is new S_Op(Interfaces."*");
   function S_Div is new S_Op(Interfaces."/");

   function Unimplemented_Op(Lhs : Register_Type; Rhs : Register_Type) return Register_Type is
   begin
      Put_Line("Unimplemented_Op raising Unimplemented_Error!");
      raise Unimplemented_Error;
      return Lhs; -- Unreachable
   end Unimplemented_Op;

   type Operator_Access is access function(Lhs : Register_Type; Rhs : Register_Type) return Register_Type;
   type Operation_Lookup_Table_Type is array (Data_Kind) of Operator_Access;

   Add_Operations : constant Operation_Lookup_Table_Type :=
      (Signed_Integer   => S_Add'ACCESS,
       Unsigned_Integer => U_Add'ACCESS,
       others           => Unimplemented_Op'ACCESS);
   Sub_Operations : constant Operation_Lookup_Table_Type :=
      (Signed_Integer   => S_Sub'ACCESS,
       Unsigned_Integer => U_Sub'ACCESS,
       others           => Unimplemented_Op'ACCESS);
   Mul_Operations : constant Operation_Lookup_Table_Type :=
      (Signed_Integer   => S_Mul'ACCESS,
       Unsigned_Integer => U_Mul'ACCESS,
       others           => Unimplemented_Op'ACCESS);
   Div_Operations : constant Operation_Lookup_Table_Type :=
      (Signed_Integer   => S_Div'ACCESS,
       Unsigned_Integer => U_Div'ACCESS,
       others           => Unimplemented_Op'ACCESS);


   -----------------------------------------------------------------------------
   generic
      with function Op(Lhs : Interfaces.Unsigned_64; Rhs : Interfaces.Unsigned_64) return Boolean;
   function U_Compare(Lhs : Register_Type; Rhs : Register_Type) return Boolean;
   function U_Compare(Lhs : Register_Type; Rhs : Register_Type) return Boolean is
   begin
      return Op(Lhs.unsigned_value, Rhs.unsigned_value);
   end U_Compare;
   function U_Eq  is new U_Compare(Interfaces."=");
   function U_Neq is new U_Compare(Interfaces."/=");
   function U_Lt  is new U_Compare(Interfaces."<");
   function U_Lte is new U_Compare(Interfaces."<=");
   function U_Gt  is new U_Compare(Interfaces.">");
   function U_Gte is new U_Compare(Interfaces.">=");

   -----------------------------------------------------------------------------
   generic
      with function Op(Lhs : Interfaces.Integer_64; Rhs : Interfaces.Integer_64) return Boolean;
   function S_Compare(Lhs : Register_Type; Rhs : Register_Type) return Boolean;
   function S_Compare(Lhs : Register_Type; Rhs : Register_Type) return Boolean is
   begin
      return Op(Lhs.signed_value, Rhs.signed_value);
   end S_Compare;
   function S_Eq  is new S_Compare(Interfaces."=");
   function S_Neq is new S_Compare(Interfaces."/=");
   function S_Lt  is new S_Compare(Interfaces."<");
   function S_Lte is new S_Compare(Interfaces."<=");
   function S_Gt  is new S_Compare(Interfaces.">");
   function S_Gte is new S_Compare(Interfaces.">=");

   function Unimplemented_Compare(Lhs : Register_Type; Rhs : Register_Type) return Boolean is
   begin
      Put_Line("Unimplemented_Compare raising Unimplemented_Error!");
      raise Unimplemented_Error;
      return False; -- Unreachable
   end Unimplemented_Compare;

   type Compare_Access is access function(Lhs : Register_Type; Rhs : Register_Type) return Boolean;
   type Compare_Lookup_Table_Type is array (Data_Kind) of Compare_Access;
   type Compare_Lookup_Table_Access is access constant Compare_Lookup_Table_Type;

   Compare_Eq : aliased constant Compare_Lookup_Table_Type :=
      (Signed_Integer   => S_Eq'ACCESS,
       Unsigned_Integer => U_Eq'ACCESS,
       others           => Unimplemented_Compare'ACCESS);

   Compare_Neq : aliased constant Compare_Lookup_Table_Type :=
      (Signed_Integer   => S_Neq'ACCESS,
       Unsigned_Integer => U_Neq'ACCESS,
       others           => Unimplemented_Compare'ACCESS);

   Compare_Lt : aliased constant Compare_Lookup_Table_Type :=
      (Signed_Integer   => S_Lt'ACCESS,
       Unsigned_Integer => U_Lt'ACCESS,
       others           => Unimplemented_Compare'ACCESS);

   Compare_Lte : aliased constant Compare_Lookup_Table_Type :=
      (Signed_Integer   => S_Lte'ACCESS,
       Unsigned_Integer => U_Lte'ACCESS,
       others           => Unimplemented_Compare'ACCESS);

   Compare_Gt : aliased constant Compare_Lookup_Table_Type :=
      (Signed_Integer   => S_Gt'ACCESS,
       Unsigned_Integer => U_Gt'ACCESS,
       others           => Unimplemented_Compare'ACCESS);

   Compare_Gte : aliased constant Compare_Lookup_Table_Type :=
      (Signed_Integer   => S_Gte'ACCESS,
       Unsigned_Integer => U_Gte'ACCESS,
       others           => Unimplemented_Compare'ACCESS);

   Compare_Lookup : constant array (Compare_Type) of Compare_Lookup_Table_Access :=
      (Eq  => Compare_Eq'ACCESS,
       Neq => Compare_Neq'ACCESS,
       L_t => Compare_Lt'ACCESS,
       Lte => Compare_Lte'ACCESS,
       G_t => Compare_Gt'ACCESS,
       Gte => Compare_Gte'ACCESS);

   -----------------------------------------------------------------------------
   generic
      Lookup_Table : in Operation_Lookup_Table_Type;
   procedure Generic_Operation
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       A     : in     Reference_Type;
       X     : in     Reference_Type;
       Y     : in     Reference_Type);
   procedure Generic_Operation
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       A     : in     Reference_Type;
       X     : in     Reference_Type;
       Y     : in     Reference_Type) is

      Data_Format : Data_Kind;
      Operator    : Operator_Access;

   begin
      --Put_Line("kv.avm.processor.Generic_Operation "&Ref_Img(A)&" <= "&Ref_Img(X)&" op "&Ref_Img(Y));
      Frame.Vet_Operands(A, X, Y);

      Data_Format := Frame.Get(X).format; -- A might be unassigned
      Operator := Lookup_Table(Data_Format);
      Frame.Set(A, Operator(Frame.Get(X), Frame.Get(Y)));
   end Generic_Operation;


   -----------------------------------------------------------------------------
   procedure Add_Instance is new Generic_Operation(Add_Operations);

   -----------------------------------------------------------------------------
   procedure Subtract_Instance is new Generic_Operation(Sub_Operations);

   -----------------------------------------------------------------------------
   procedure Multiply_Instance is new Generic_Operation(Mul_Operations);

   -----------------------------------------------------------------------------
   procedure Divide_Instance is new Generic_Operation(Div_Operations);


   -----------------------------------------------------------------------------
   procedure Branch
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Branch_Target : Interfaces.Unsigned_32;
      Condition     : kv.avm.Registers.Register_Type;

   begin
      Condition := Frame.Get(Instr.condition);
      if Condition.bit then
         if Instr.op_code = branch_abs then
            Branch_Target := Instr.target;
         elsif Instr.op_code = branch_rel then
            -- Target is a signed integer stored in an unsigned field
            -- Simply adding target to the program
            -- counter produces the correct result.
            Branch_Target := Frame.Program_Counter + Instr.target;
         else
            return;
         end if;
      else
         if Instr.op_code = branch_neq then
            Branch_Target := Frame.Program_Counter + Instr.target;
         else
            return;
         end if;
      end if;
      Frame.Set_Program_Counter(Branch_Target);
   end Branch;

   -----------------------------------------------------------------------------
   procedure Fold
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is
   begin
      Frame.Set(Instr.lhs, (format => kv.avm.Registers.Tuple, folded_tuple => Frame.Fold(Instr.rhs)));
   end Fold;

   -----------------------------------------------------------------------------
   procedure Peek
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type) is

      Register : kv.avm.Registers.Register_Type;
      The_Tuple : kv.avm.Tuples.Tuple_Type;

   begin
      --Put_Line("kv.avm.processor.Peek "&Ref_Img(Instr.A)&" <= peek from tuple "&Ref_Img(Instr.X)&", index "&Ref_Img(Instr.Y));
      -- Return values are sent back as tuples, so a PEEK is the normal means of accessing the data.
      -- This data might be an unfufilled future.
      Register := Frame.Get(Instr.X);
      if Register.Format = kv.avm.Registers.Future then
         --Put_Line("Frame="&Frame.Image&" blocking on Future="&Interfaces.Unsigned_32'IMAGE(Register.ID));
         Status := kv.avm.Control.Blocked;
         return;
      end if;
      The_Tuple := Register.folded_tuple;
      Frame.Set(Instr.A, The_Tuple.Peek(Interfaces.Unsigned_32(Frame.Get(Instr.Y).unsigned_value)).all);
      Status := kv.avm.Control.Active;
   end Peek;

   Constructor_Name : aliased constant String := "CONSTRUCTOR";

   -----------------------------------------------------------------------------
   procedure New_Actor
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type) is

      Fresh_Actor : kv.avm.Actor_References.Actor_Reference_Type;
      Folded_Tuple : aliased kv.avm.Tuples.Tuple_Type;
      Register : aliased kv.avm.Registers.Register_Type;
      Message : kv.avm.Messages.Message_Type;

      use kv.avm.Registers;

   begin
      --Put_Line("kv.avm.processor.New_Actor called with name = '"&Frame.Get(Instr.Y).Actor_Kind.all&"'.");
      Self.Machine.New_Actor(+Frame.Get(Instr.Y).Actor_Kind, Fresh_Actor);
      Frame.Set(Instr.A, (format => kv.avm.Registers.Actor_Reference, Instance => Fresh_Actor));
      Folded_Tuple := Get_Or_Make_Tuple(Frame.all, Instr.X);
--      if Frame.Get(Instr.X).format = kv.avm.Registers.Tuple then
--         Folded_Tuple := Frame.Get(Instr.X).folded_tuple;
--      else
--         --Put_Line("kv.avm.processor.New_Actor folding a tuple around "&Ref_Img(Instr.X)&" = "&Reg_Img(Frame.Get(Instr.X)));
--         Register := Frame.Get(Instr.X);
--         Folded_Tuple.Fold_One(Register'ACCESS);
--      end if;
      Message.Initialize
         (Source       => Frame.Get_Instance,
          Reply_To     => Frame.Get_Instance,
          Destination  => Fresh_Actor,
          Message_Name => Constructor_Name,
          Data         => Folded_Tuple,
          Future       => kv.avm.control.NO_FUTURE);
      Self.Machine.Post_Message
         (Message => Message,
          Status  => Status);
   end New_Actor;


   -----------------------------------------------------------------------------
   procedure Compute
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Compare_Op : Compare_Access;
      X : kv.avm.Registers.Register_Type;
      Y : kv.avm.Registers.Register_Type;

      use kv.avm.Registers;

      True_Value : constant kv.avm.Registers.Register_Type := (format => Bit_Or_Boolean, bit => True);
      False_Value : constant kv.avm.Registers.Register_Type := (format => Bit_Or_Boolean, bit => False);

   begin
      case Instr.Action is
         when Add =>
            Add_Instance(Self, Frame, Instr.Result, Instr.Left, Instr.Right);
         when Sub =>
            Subtract_Instance(Self, Frame, Instr.Result, Instr.Left, Instr.Right);
         when Mul =>
            Multiply_Instance(Self, Frame, Instr.Result, Instr.Left, Instr.Right);
         when Div =>
            Divide_Instance(Self, Frame, Instr.Result, Instr.Left, Instr.Right);
         when Compare_Type =>
            X := Frame.Get(Instr.Left);
            Y := Frame.Get(Instr.Right);
            Compare_Op := Compare_Lookup(Instr.Action)(X.format);
            if Compare_Op(X, Y) then
               Frame.Set(Instr.Result, True_Value);
            else
               Frame.Set(Instr.Result, False_Value);
            end if;
         when others =>
            raise Unimplemented_Error;
      end case;
   end Compute;


   -----------------------------------------------------------------------------
   procedure Emit
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Value : kv.avm.Registers.Register_Type;

      use kv.avm.Registers;

   begin
      Value := Frame.Get(Instr.value);
      Ada.Text_IO.Put("Emit:");
      case Value.format is
         when Signed_Integer =>
            Ada.Text_IO.Put_Line(Interfaces.Integer_64'IMAGE(Value.signed_value));
         when Unsigned_Integer =>
            Ada.Text_IO.Put_Line(Interfaces.Unsigned_64'IMAGE(Value.unsigned_value));
         when Bit_Or_Boolean =>
            Ada.Text_IO.Put_Line(Boolean'IMAGE(Value.bit));
         when Floatingpoint =>
            Ada.Text_IO.Put_Line(Interfaces.IEEE_Float_64'IMAGE(Value.value));
         when Immutable_String =>
            Ada.Text_IO.Put_Line(+Value.The_String);
         when Future =>
            Ada.Text_IO.Put_Line(Interfaces.Unsigned_32'IMAGE(Value.ID));
         when Message_Definition =>
            Ada.Text_IO.Put_Line(+Value.Message_Name);
         when others =>
            Ada.Text_IO.Put_Line("EMIT not yet implemented for this register type.");
      end case;
   end Emit;


   -----------------------------------------------------------------------------
   procedure Self_Tail_X
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Future : Interfaces.Unsigned_32 := kv.avm.control.NO_FUTURE;
      Folded_Tuple : aliased kv.avm.Tuples.Tuple_Type;

   begin
      Put_Line("kv.avm.processor.Self_Tail_X message args="&Ref_Img(Instr.args_5b2)&", mdef="&Ref_Img(Instr.mdef_5b2));

      if Instr.Op_Code = Self_Tail_Call then
         -- This is a tail call, need the future that the active frame was invoked with
         Future := Frame.Get_Invoker_Future;
      else
         -- This is a tail send, if there was a future, it is being lost.
         Future := kv.avm.control.NO_FUTURE;
      end if;
      Folded_Tuple := Get_Or_Make_Tuple(Frame.all, Instr.args_5b2);
      Frame.Process_Gosub
         (Tail_Call    => True,
          Supercall    => False,
          Message_Name => Frame.Get(Instr.mdef_5b2).Message_Name,
          Data         => Folded_Tuple, --Frame.Get(Instr.args_5b2).folded_tuple,
          Future       => Future);
   end Self_Tail_X;

   -----------------------------------------------------------------------------
   procedure Halt_Actor
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is
   begin
      Frame.Set_Program_Counter(Frame.Program_Counter - 1);
      Frame.Halt_Actor;
   end Halt_Actor;

   -----------------------------------------------------------------------------
   procedure Trap
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type) is

      Answer : kv.avm.Registers.Register_Type;

      use kv.avm.Registers;
   begin
      -- Unsigned integer traps are handled by the processor, string traps go to the machine.
      --
      if Frame.Get(Instr.X).Format = Unsigned_Integer then
         case Frame.Get(Instr.X).Unsigned_Value is
            when 1 =>
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line("The answser is: "&Reg_Img(Frame.Get(Instr.Y)));
               Ada.Text_IO.New_Line;
            when others =>
               raise Unimplemented_Error;
         end case;
      elsif Frame.Get(Instr.X).Format = Immutable_String then
         Self.Machine.Trap_To_The_Machine(+Frame.Get(Instr.X).The_String, Frame.Get(Instr.Y), Answer, Status);
         Frame.Set(Instr.A, Answer);
      else
         raise Unimplemented_Error;
      end if;
   end Trap;

   -----------------------------------------------------------------------------
   procedure Self_Call
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Future : Interfaces.Unsigned_32 := kv.avm.control.NO_FUTURE;
      Folded_Tuple : aliased kv.avm.Tuples.Tuple_Type;

   begin
      Put_Line("kv.avm.processor.Self_Call reply into=" & Ref_Img(Instr.rply_5b1) &  ", message arguments="&Ref_Img(Instr.args_5b1)&", message profile="&Ref_Img(Instr.mdef_5b1));

      Self.Machine.Generate_Next_Future(Future);
      Frame.Set_Reply_Information(Instr.rply_5b1, Future);

      Folded_Tuple := Get_Or_Make_Tuple(Frame.all, Instr.args_5b1);
      Frame.Process_Gosub
         (Tail_Call    => False,
          Supercall    => False,
          Message_Name => Frame.Get(Instr.mdef_5b1).Message_Name,
          Data         => Folded_Tuple, --Frame.Get(Instr.args_5b1).folded_tuple,
          Future       => Future);
   end Self_Call;


   -----------------------------------------------------------------------------
   procedure Super_Call
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Future : Interfaces.Unsigned_32 := kv.avm.control.NO_FUTURE;
      Folded_Tuple : aliased kv.avm.Tuples.Tuple_Type;

   begin
      Put_Line("kv.avm.processor.Super_Call reply into=" & Ref_Img(Instr.rply_5b1) &  ", message arguments="&Ref_Img(Instr.args_5b1)&", message profile="&Ref_Img(Instr.mdef_5b1));

      Self.Machine.Generate_Next_Future(Future);
      Frame.Set_Reply_Information(Instr.rply_5b1, Future);

      --!@#$
      Folded_Tuple := Get_Or_Make_Tuple(Frame.all, Instr.args_5b1);
      Frame.Process_Gosub
         (Tail_Call    => False,
          Supercall    => True,
          Message_Name => Frame.Get(Instr.mdef_5b1).Message_Name,
          Data         => Folded_Tuple, --Frame.Get(Instr.args_5b1).folded_tuple,
          Future       => Future);
   end Super_Call;


   -----------------------------------------------------------------------------
   procedure Actor_Call
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type) is

      Future : Interfaces.Unsigned_32 := kv.avm.control.NO_FUTURE;
      Message : kv.avm.Messages.Message_Type;
      Folded_Tuple : aliased kv.avm.Tuples.Tuple_Type;

   begin
      Put_Line("kv.avm.processor.Actor_Call reply into=" & Ref_Img(Instr.rply_5a1) &
         ", message arguments=" & Ref_Img(Instr.args_5a1) &
         ", actor=" & Ref_Img(Instr.actr_5a1) &
         ", message profile=" & Ref_Img(Instr.mdef_5a1));

      Self.Machine.Generate_Next_Future(Future);
      Frame.Set_Reply_Information(Instr.rply_5a1, Future);

      Folded_Tuple := Get_Or_Make_Tuple(Frame.all, Instr.args_5a1);
      Message.Initialize
         (Source       => Frame.Get_Instance,
          Reply_To     => Frame.Get_Instance,
          Destination  => Frame.Get(Instr.actr_5a1).Instance,
          Message_Name => +Frame.Get(Instr.mdef_5a1).Message_Name,
          Data         => Folded_Tuple, --Frame.Get(Instr.args_5a1).folded_tuple,
          Future       => Future);
      Self.Machine.Post_Message
         (Message => Message,
          Status  => Status);
   end Actor_Call;


   -----------------------------------------------------------------------------
   procedure Format_5A2
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type) is

      Future   : Interfaces.Unsigned_32 := kv.avm.control.NO_FUTURE;
      Reply_To : kv.avm.Actor_References.Actor_Reference_Type;
      --Status   : kv.avm.Control.Status_Type;
      Message : kv.avm.Messages.Message_Type;
      Folded_Tuple : aliased kv.avm.Tuples.Tuple_Type;

   begin
      Put_Line("kv.avm.processor.Format_5A2 message arguments=" & Ref_Img(Instr.args_5a2) &
         ", actor=" & Ref_Img(Instr.actr_5a2) &
         ", message profile=" & Ref_Img(Instr.mdef_5a2));

      if Instr.op_code = Actor_Tail_Call then
         -- This is a tail call, need the future that the active frame was invoked with
         Future := Frame.Get_Invoker_Future;
      end if;
      if Instr.op_code = Actor_Send then
         Reply_To := Frame.Get_Instance;
      else
         Reply_To := Frame.Get_Invoker;
      end if;

      Folded_Tuple := Get_Or_Make_Tuple(Frame.all, Instr.args_5a2);
      Message.Initialize
         (Source       => Frame.Get_Instance,
          Reply_To     => Reply_To,
          Destination  => Frame.Get(Instr.actr_5a2).Instance,
          Message_Name => +Frame.Get(Instr.mdef_5a2).Message_Name,
          Data         => Folded_Tuple, --Frame.Get(Instr.args_5a2).folded_tuple,
          Future       => Future);
      Self.Machine.Post_Message
         (Message => Message,
          Status  => Status);
   end Format_5A2;


   -----------------------------------------------------------------------------
   procedure Peek_Immediate
      (Self   : in out Processor_Type;
       Frame  : access kv.avm.Frames.Frame_Type;
       Instr  : in     kv.avm.Instructions.Instruction_Type;
       Status :    out kv.avm.Control.Status_Type) is

      Register : kv.avm.Registers.Register_Type;
      The_Tuple : kv.avm.Tuples.Tuple_Type;

   begin
      -- Return values are sent back as tuples, so a PEEK is the normal means of accessing the data.
      -- This data might be an unfufilled future.
      Register := Frame.Get(Instr.tuple);
      if Register.Format = kv.avm.Registers.Future then
         --Put_Line("Frame="&Frame.Image&" blocking on Future="&Interfaces.Unsigned_32'IMAGE(Register.ID));
         Status := kv.avm.Control.Blocked;
         return;
      end if;
      The_Tuple := Register.folded_tuple;
      Frame.Set(Instr.lvalue, The_Tuple.Peek(Interfaces.Unsigned_32(Instr.index)).all);
      Status := kv.avm.Control.Active;
   end Peek_Immediate;


   -----------------------------------------------------------------------------
   procedure Assert
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Condition : kv.avm.Registers.Register_Type;

   begin
      Condition := Frame.Get(Instr.value);
      if not Condition.bit then
         -- The assertion failed.
         Frame.Set_Program_Counter(Frame.Program_Counter - 1);
         Frame.Halt_Actor;
         Self.Failed_Assertion_Count := Self.Failed_Assertion_Count + 1;
         Put_Error("Machine Assertion Failed at " & Frame.Debug_Info);
      end if;
   end Assert;


   -----------------------------------------------------------------------------
   procedure Self_Send
      (Self  : in out Processor_Type;
       Frame : access kv.avm.Frames.Frame_Type;
       Instr : in     kv.avm.Instructions.Instruction_Type) is

      Status : kv.avm.Control.Status_Type;
      Message : kv.avm.Messages.Message_Type;
      Folded_Tuple : aliased kv.avm.Tuples.Tuple_Type;

   begin
      Put_Line("kv.avm.processor.Self_Send message arguments="&Ref_Img(Instr.args_5b2)&", message profile="&Ref_Img(Instr.mdef_5b2));

      Folded_Tuple := Get_Or_Make_Tuple(Frame.all, Instr.args_5b2);
      Message.Initialize
         (Source       => Frame.Get_Instance,
          Reply_To     => Frame.Get_Instance,
          Destination  => Frame.Get_Instance,
          Message_Name => +Frame.Get(Instr.mdef_5b2).Message_Name,
          Data         => Folded_Tuple, --Frame.Get(Instr.args_5b2).folded_tuple,
          Future       => kv.avm.control.NO_FUTURE);
      Self.Machine.Post_Message
         (Message => Message,
          Status  => Status);
   end Self_Send;

end kv.avm.Processors;
