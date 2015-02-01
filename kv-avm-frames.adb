with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;

with kv.avm.memories;
with kv.avm.Log; use kv.avm.Log;
with kv.avm.Instances;
with kv.avm.Messages;
with kv.avm.Actor_Pool;
with kv.avm.Executables;

package body kv.avm.Frames is

   use Interfaces;
   use kv.avm.Instructions;
   use kv.avm.Registers;

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self     : in out Frame_Type;
       Instance : in     kv.avm.Actor_References.Actor_Reference_Type;
       Name     : in     kv.avm.Registers.String_Type;
       Invoker  : in     kv.avm.Actor_References.Actor_Reference_Type;
       Future   : in     Interfaces.Unsigned_32;
       Code     : in     kv.avm.Instructions.Code_Access;
       Memory   : in     kv.avm.Memories.Memory_Type;
       Next     : in     Frame_Access := null) is
   begin
      Self.Instance := Instance;
      Self.Name := Name;
      Self.Invoker := Invoker;
      Self.Future := Future;
      Self.Code := Code;
      Self.Memory := Memory;
      Self.Pc := Code'FIRST;
      Self.RunningF := True;
      Self.Blocked := False;
      Self.Next := Next;
      if Next = null then
         Self.Depth := 1;
      else
         Self.Depth := Next.Depth + 1;
      end if;
   end Initialize;

   -----------------------------------------------------------------------------
   function Program_Counter
      (Self : in    Frame_Type) return Interfaces.Unsigned_32 is
   begin
      return Self.Pc;
   end Program_Counter;

   -----------------------------------------------------------------------------
   procedure Set_Program_Counter
      (Self : in out Frame_Type;
       Pc   : in     Interfaces.Unsigned_32) is
   begin
      if Pc in Self.Code'RANGE then
         Self.Pc := Pc;
      else
         Self.RunningF := False;
         raise Program_Counter_Error;
      end if;
   end Set_Program_Counter;

   -----------------------------------------------------------------------------
   procedure Increment_Program_Counter
      (Self : in out Frame_Type) is
   begin
      Self.Set_Program_Counter(Self.Pc + 1);
   end Increment_Program_Counter;

   -----------------------------------------------------------------------------
   function Is_Done(Self : Frame_Type) return Boolean is
   begin
      return not Self.RunningF;
      --return not (Self.RunningF or Self.Blocked);
   end Is_Done;

   -----------------------------------------------------------------------------
   function Get_Running
      (Self : in    Frame_Type) return Boolean is
   begin
      return Self.RunningF;
      --return Self.RunningF and not Self.Blocked;
   end Get_Running;

   -----------------------------------------------------------------------------
   procedure Set_Running
      (Self    : in out Frame_Type;
       Running : in     Boolean) is
   begin
      Self.RunningF := Running;
   end Set_Running;

   -----------------------------------------------------------------------------
   procedure Set_Blocked
      (Self    : in out Frame_Type;
       Blocked : in     Boolean) is
    begin
       Self.Blocked := Blocked;
    end Set_Blocked;

   -----------------------------------------------------------------------------
   function Fetch_Instruction
      (Self : in     Frame_Type) return kv.avm.Instructions.Instruction_Type is
   begin
      return Self.Code(Self.Pc);
   end Fetch_Instruction;

   -----------------------------------------------------------------------------
   procedure Fetch_And_Increment -- Fetch an instruction and increment the program counter by one
      (Self        : in out Frame_Type;
       Instruction :    out kv.avm.Instructions.Instruction_Type) is
   begin
      Instruction := Self.Fetch_Instruction;
      Self.Increment_Program_Counter;
   end Fetch_And_Increment;

   -----------------------------------------------------------------------------
   procedure Vet_Operands
      (Self   : in out Frame_Type;
       Target : in     Reference_Type;
       Source : in     Reference_Type) is

      Target_Format : Data_Kind := Self.Get(Target).Format;
      Source_Format : Data_Kind := Self.Get(Source).Format;

   begin
      if Target.Memory = Fixed then
         raise Fixed_Target_Error;
      end if;
      if Source_Format = Unset then
         raise Invalid_Source_Error;
      elsif Target_Format = Unset then
         null; -- This is not only fine, but often expected
      elsif Source_Format /= Target_Format then
         raise Operand_Mismatch_Error;
      end if;
   end Vet_Operands;

   -----------------------------------------------------------------------------
   procedure Vet_Operands
      (Self     : in out Frame_Type;
       Target   : in     Reference_Type;
       Source_1 : in     Reference_Type;
       Source_2 : in     Reference_Type) is

      Source_1_Format : Data_Kind := Self.Get(Source_1).Format;
      Source_2_Format : Data_Kind := Self.Get(Source_2).Format;

   begin
      Self.Vet_Operands(Target, Source_1);
      Self.Vet_Operands(Target, Source_2);
      if Source_1_Format /= Source_2_Format then
         raise Operand_Mismatch_Error;
      end if;
   end Vet_Operands;

   -----------------------------------------------------------------------------
   function Get
      (Self : in     Frame_Type;
       Ref  : in     Reference_Type) return kv.avm.Registers.Register_Type is
      use kv.avm.Memories;
   begin
      if not Self.Memory.Get(Ref.Memory).Is_Set then
         Put_Line("ERROR: kv.avm.frame.Get, Self.Memory(" & Register_Bank_Type'IMAGE(Ref.Memory) & ") is null!");
      end if;
      return Self.Memory.Read(Ref);
   end Get;

   -----------------------------------------------------------------------------
   procedure Set
      (Self  : in out Frame_Type;
       Ref   : in     Reference_Type;
       Value : in     kv.avm.Registers.Register_Type) is
   begin
      Self.Memory.Write(Ref, Value);
   end Set;

   -----------------------------------------------------------------------------
   function Get_Invoker(Self : Frame_Type) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Invoker;
   end Get_Invoker;

   -----------------------------------------------------------------------------
   function Get_Instance(Self : Frame_Type) return kv.avm.Actor_References.Actor_Reference_Type is
   begin
      return Self.Instance;
   end Get_Instance;

   -----------------------------------------------------------------------------
   function Is_Self_Replying(Self : Frame_Type) return Boolean is
      use kv.avm.Actor_References;
   begin
      return Self.Instance = Self.Invoker;
   end Is_Self_Replying;

   -----------------------------------------------------------------------------
   function Get_Name(Self : Frame_Type) return String is
      use kv.avm.Registers;
   begin
      return +Self.Name;
   end Get_Name;

   -----------------------------------------------------------------------------
   procedure Set_Invoker -- Test routine
      (Self    : in out Frame_Type;
       Invoker : in     kv.avm.Actor_References.Actor_Reference_Type) is
   begin
      Self.Invoker := Invoker;
   end Set_Invoker;

   -----------------------------------------------------------------------------
   procedure Set_Future -- Test routine
      (Self   : in out Frame_Type;
       Future : in     Interfaces.Unsigned_32) is
   begin
      Self.Future := Future;
   end Set_Future;

   -----------------------------------------------------------------------------
   function Fold
      (Self : in     Frame_Type;
       Ref  : in     Reference_Type) return kv.avm.Tuples.Tuple_Type is

      Map : kv.avm.Tuples.Map_Type := Self.Get(Ref).Map;
      Answer : kv.avm.Tuples.Tuple_Type;

   begin
      Answer.Fold(Self.Memory, Map);
      return Answer;
   end Fold;


   -----------------------------------------------------------------------------
   procedure Process_Gosub_Response
      (Self   : in out Frame_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type) is
   begin
      Self.Set(Self.Future_R, (format => kv.avm.Registers.Tuple, folded_tuple => Answer));
   end Process_Gosub_Response;


   -----------------------------------------------------------------------------
   procedure Resolve_Future
      (Self   : in out Frame_Type;
       Answer : in     kv.avm.Tuples.Tuple_Type;
       Future : in     Interfaces.Unsigned_32) is

      Found : Boolean;
      Location : kv.avm.References.Reference_Type;

   begin
      Self.Memory.Find_Future(Local, Future, Found, Location);
      if Found then
         Self.Memory.Write(Location, (Format => kv.avm.Registers.Tuple, folded_tuple => Answer));
         Put_Line("Future"&Interfaces.Unsigned_32'IMAGE(Future)&" resolved!");
         Self.Blocked := False;
         return;
      end if;
      if Self.Next = null then
         -- The future wasn't here and there is nowhere else to look.
         raise kv.avm.Executables.Corrupt_Executable_Error;
      else
         Self.Next.Resolve_Future(Answer, Future);
      end if;
   end Resolve_Future;


   -----------------------------------------------------------------------------
   function Reachable(Self : Frame_Type) return kv.avm.Actor_References.Sets.Set is
      Can_Reach : kv.avm.Actor_References.Sets.Set := kv.avm.Actor_References.Sets.Empty_Set;
      use kv.avm.Actor_References;
   begin
      if Self.Invoker /= Null_Reference then
         Can_Reach.Include(Self.Invoker);
      end if;
      if Self.Next /= null then
         Can_Reach.Union(Self.Next.Reachable);
      end if;
      Can_Reach.Union(Self.Memory.Reachable(kv.avm.References.Input));
      Can_Reach.Union(Self.Memory.Reachable(kv.avm.References.Local));
      return Can_Reach;
   end Reachable;


   -----------------------------------------------------------------------------
   procedure Set_Reply_Information
      (Self     : in out Frame_Type;
       Reply_To : in     Reference_Type;
       Future   : in     Interfaces.Unsigned_32) is
   begin
      Self.Set(Reply_To, (Format => kv.avm.Registers.Future, ID => Future));
      Self.Future_R := Reply_To;
   end Set_Reply_Information;


   -----------------------------------------------------------------------------
   function Get_Invoker_Future(self : Frame_Type) return Interfaces.Unsigned_32 is
   begin
      return Self.Future;
   end Get_Invoker_Future;

   -----------------------------------------------------------------------------
   function True_Instance(Instance_Ref : kv.avm.Actor_References.Actor_Reference_Type) return kv.avm.Executables.Executable_Access is
   begin
      return kv.avm.Actor_Pool.Resolve(Instance_Ref);
   end True_Instance;

   -----------------------------------------------------------------------------
   procedure Process_Gosub
      (Self         : in out Frame_Type;
       Tail_Call    : in     Boolean;
       Supercall    : in     Boolean;
       Message_Name : in     kv.avm.Registers.String_Type;
       Data         : in     kv.avm.Tuples.Tuple_Type;
       Future       : in     Interfaces.Unsigned_32) is

      Reply_To : kv.avm.Actor_References.Actor_Reference_Type;
      Frame_Instance : kv.avm.Executables.Executable_Access;

      use kv.avm.Registers;

   begin
      if Tail_Call then
         Reply_To := Self.Invoker;
      else
         Reply_To := Self.Instance;
      end if;
      Put_Line("kv.avm.frame.Process_Gosub"&
         ", Name=" & (+Message_Name) &
         ", tail_call="&Boolean'IMAGE(Tail_Call)&
         ", Future="&Interfaces.Unsigned_32'IMAGE(Future) & "@" & Reply_To.Image);
      Frame_Instance := kv.avm.Actor_Pool.Resolve(Self.Instance);
      Frame_Instance.Process_Gosub
         (Tailcall  => Tail_Call,
          Supercall => Supercall,
          Reply_To  => Reply_To,
          Method    => Message_Name,
          Data      => Data.Unfolded,
          Future    => Future);
   end Process_Gosub;

   -----------------------------------------------------------------------------
   function Get_Next(Self : Frame_Type) return Frame_Access is
   begin
      return Self.Next;
   end Get_Next;

   -----------------------------------------------------------------------------
   procedure Halt_Actor
      (Self : in out Frame_Type) is

      Frame_Instance : kv.avm.Executables.Executable_Access;

   begin
      Frame_Instance := kv.avm.Actor_Pool.Resolve(Self.Instance);
      Frame_Instance.Halt_Actor;
   end Halt_Actor;

   -----------------------------------------------------------------------------
   function Image(Self : Frame_Type) return String is
      use kv.avm.Registers;
   begin
      return True_Instance(Self.Instance).Image &
             "." & (+Self.Name) & "(depth"&Positive'IMAGE(Self.Depth)&")" &
             "@" & Interfaces.Unsigned_32'IMAGE(Self.Pc);
   end Image;


   -----------------------------------------------------------------------------
   function Debug_Info(Self : Frame_Type) return String is
      Inst : kv.avm.Instructions.Instruction_Type := Self.Fetch_Instruction;
      use kv.avm.Registers;
   begin
      return True_Instance(Self.Instance).Image &
             "." & (+Self.Name) & "(depth"&Positive'IMAGE(Self.Depth)&")" &
             "@" & Interfaces.Unsigned_32'IMAGE(Self.Pc) &
             ":" & kv.avm.memories.Instruction_Image(Inst, Self.Memory);
--             ":" & kv.avm.memories.Instruction_Image(Inst, Convert(Self.Memory'ADDRESS));
   end Debug_Info;

   -----------------------------------------------------------------------------
   procedure Prepare_For_Deletion(Self : in out Frame_Type) is
   begin
      if Self.RunningF then
         Put_Error("Running frame is being deleted!");
      end if;
   end Prepare_For_Deletion;

end kv.avm.Frames;
