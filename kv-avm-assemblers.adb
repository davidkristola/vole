with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces;

with String_Ops; use String_Ops;

with kv.avm.references; use kv.avm.references;
with kv.avm.Log; use kv.avm.Log;
with kv.avm.Methods; use kv.avm.Methods;
with kv.avm.File_Reader;

package body kv.avm.Assemblers is

   use kv.avm.Registers;

   function Convert is new Ada.Unchecked_Conversion
      (Source => kv.avm.Instructions.Instruction_Type,
       Target => Interfaces.Unsigned_64);

   function Convert is new Ada.Unchecked_Conversion
      (Source => Interfaces.Unsigned_64,
       Target => kv.avm.Instructions.Instruction_Type);

   function Convert is new Ada.Unchecked_Conversion
      (Source => Interfaces.Integer_32,
       Target => Interfaces.Unsigned_32);

   procedure Free is new Ada.Unchecked_Deallocation(String, String_Ops.String_Pointer_Type);

   ----------------------------------------------------------------------------
   function Make_Word_Code_Name(Asm_Name : String) return String is
      New_Name : String := Asm_Name;
   begin
      New_Name(New_Name'LAST) := 'c';
      return New_Name;
   end Make_Word_Code_Name;


   ----------------------------------------------------------------------------
   function No_Quotes(Str : String) return String is
      Index_First : Positive := Str'FIRST;
      Index_Last : Positive := Str'LAST;
   begin
      if Str(Index_First) = '"' then
         Index_First := Index_First + 1;
      end if;
      if Str(Index_Last) = '"' then
         Index_Last := Index_Last - 1;
      end if;
      return Str(Index_First..Index_Last);
   end No_Quotes;


   ----------------------------------------------------------------------------
   function Build_Constant(Type_Img : String; Value : String) return kv.avm.Registers.Register_Type is
      Kind   : constant String := Down_Case(Type_Img);
      use kv.avm.Registers;
   begin
      if Kind = "uint" or Kind = "unsigned_integer" then
         return (format => Unsigned_Integer, unsigned_value => Interfaces.Unsigned_64'VALUE(Value));
      elsif Kind = "adef" or Kind = "actor_definition" then
         return (format => Actor_Definition, Actor_Kind => +No_Quotes(Value));
      elsif Kind = "mdef" or Kind = "message_definition" then
         return (format => Message_Definition, Message_Name => +No_Quotes(Value), Send_Count => 0, Reply_Count => 0);
      elsif Kind = "str" or Kind = "immutable_string" then
         return (format => Immutable_String, The_String => +No_Quotes(Value));
      elsif Kind = "int64_t" or Kind = "signed_integer" then
         return (format => Signed_Integer, signed_value => Interfaces.Integer_64'VALUE(Value));
      elsif Kind = "bit_or_boolean" then
         return (format => Bit_Or_Boolean, bit => Boolean'VALUE(Value));
      elsif Kind = "tuple_map" then
         return String_To_Tuple_Map(Value);
      end if;
      Put_Error("ERROR: Could not parse actor constant <"&Type_Img&"> := <"&Value&">.");
      raise Unrecognized_Type_Error;
   end Build_Constant;


   -----------------------------------------------------------------------------
   function Nth_Rest(Str : String; N : Natural) return String is
   begin
      if N = 1 then
         return Rest(Str);
      else
         return Nth_Rest(Rest(Str), N-1);
      end if;
   end Nth_Rest;


   -----------------------------------------------------------------------------
   function Select_Value
      (Str : in     String) return String is
      In_Q : Boolean := False;
      In_B : Boolean := False;
   begin
      for I in Str'RANGE loop
         if Str(I) = '"' then
            In_Q := not In_Q;
         elsif Str(I) = '[' then
            In_B := True;
         elsif Str(I) = ']' then
            In_B := False;
         end if;
         if not (In_Q or In_B) then
            if Is_White_Space(Str(I)) then
               return Str(Str'FIRST..I-1);
            end if;
         end if;
      end loop;
      return Str; -- no blanks, return the whole string.
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Select_Value('" & Str & "').");
         raise;
   end Select_Value;


   ----------------------------------------------------------------------------
   function Bool(Img : String) return Boolean is
      First_Letter : Character;
   begin
      First_Letter := Img(Img'FIRST);
      if First_Letter = 'T' or First_Letter = 't' then
         return True;
      elsif First_Letter = 'F' or First_Letter = 'f' then
         return False;
      end if;
      return Boolean'VALUE(Img);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Bool('" & Img & "').");
         raise;
   end Bool;


   ----------------------------------------------------------------------------
   procedure Advance_And_Skip_If(Offset : in out Positive; Rest : in String; Expected : in String) is
   begin
      Offset := Offset + 1;
      if Nth(Rest, Offset) = Expected then
         Offset := Offset + 1;
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Advance_And_Skip_If");
         raise;
   end Advance_And_Skip_If;


   ----------------------------------------------------------------------------
   procedure Initialize
      (Self  : access Message_Type;
       Name  : in     String;
       Next  : in     Message_Access) is
   begin
      Self.Name := new String'(Name);
      Self.Count := 0;
      Self.Next := Next;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Initialize(Message_Type)");
         raise;
   end Initialize;


   ----------------------------------------------------------------------------
   function Build_Format_0(Op_Code : kv.avm.Instructions.format_0_type) return kv.avm.Instructions.Instruction_Type is
      use kv.avm.Instructions;
   begin
      case Op_Code is
         when TERMINATE_PROGRAM =>
            return (Op_Code => TERMINATE_PROGRAM);
         when HALT_ACTOR =>
            return (Op_Code => HALT_ACTOR);
         when STOP_FRAME =>
            return (Op_Code => STOP_FRAME);
         when NO_OP =>
            return (Op_Code => NO_OP);
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_0");
         raise;
   end Build_Format_0;


   ----------------------------------------------------------------------------
   function Build_Format_1(Op_Code : kv.avm.Instructions.format_1_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      Value : reference_type;
      use kv.avm.Instructions;
   begin
      Value := Make_Reference(Drop_Vole_Comments(Rest));
      case Op_Code is -- EMIT, ASSERT, REPLY, JUMP
         when EMIT =>
            return (Op_Code => EMIT, value => Value);
         when ASSERT =>
            return (Op_Code => ASSERT, value => Value);
         when REPLY =>
            return (Op_Code => REPLY, value => Value);
         when JUMP =>
            return (Op_Code => JUMP, value => Value);
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_1");
         raise;
   end Build_Format_1;


   ----------------------------------------------------------------------------
   function Build_Format_1b(Op_Code : kv.avm.Instructions.format_1b_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      Offset : Interfaces.Integer_32;
      use kv.avm.Instructions;
   begin
      case Op_Code is -- JUMP_ABS, JUMP_REL
         when JUMP_ABS =>
            return (Op_Code => JUMP_ABS, jump => Interfaces.Unsigned_32'VALUE(Rest));
         when JUMP_REL =>
            Offset := Interfaces.Integer_32'VALUE(Rest);
            return (Op_Code => JUMP_REL, jump => Convert(Offset));
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_1b");
         raise;
   end Build_Format_1b;


   ----------------------------------------------------------------------------
   function Build_Format_2(Op_Code : kv.avm.Instructions.format_2_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      Lhs : reference_type;
      Rhs : reference_type;
      Rhs_Pos : Positive;
      use kv.avm.Instructions;
   begin
      Lhs := Make_Reference(First(Rest));
      -- Nth 2 should be ":="
      if Nth(Rest, 2) = ":=" then
         Rhs_Pos := 3;
      else
         Rhs_Pos := 2;
      end if;
      Rhs := Make_Reference(Nth(Rest, Rhs_Pos));
      case Op_Code is -- SET, SHIFT_LEFT, SHIFT_RIGHT, UNFOLD, OFFSET
         when SET =>
            return (Op_Code => SET, lhs => Lhs, rhs => Rhs);
         when FOLD =>
            return (Op_Code => FOLD, lhs => Lhs, rhs => Rhs);
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_2");
         raise;
   end Build_Format_2;


   ----------------------------------------------------------------------------
   function Build_Format_2b(Op_Code : kv.avm.Instructions.format_2b_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      Condition : reference_type;
      Offset : Interfaces.Integer_32;
      use kv.avm.Instructions;
   begin
      Condition := Make_Reference(First(Rest));
      case Op_Code is -- BRANCH_ABS, BRANCH_REL, BRANCH_NEQ
         when BRANCH_ABS =>
            return (Op_Code => BRANCH_ABS, condition => Condition, target => Interfaces.Unsigned_32'VALUE(Second(Rest)));
         when BRANCH_REL =>
            Offset := Interfaces.Integer_32'VALUE(Second(Rest));
            return (Op_Code => BRANCH_REL, condition => Condition, target => Convert(Offset));
         when BRANCH_NEQ =>
            Offset := Interfaces.Integer_32'VALUE(Second(Rest));
            return (Op_Code => BRANCH_NEQ, condition => Condition, target => Convert(Offset));
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_2b");
         raise;
   end Build_Format_2b;


   ----------------------------------------------------------------------------
   function Build_Format_3(Op_Code : kv.avm.Instructions.format_3_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      a : reference_type;
      x : reference_type;
      y : reference_type;
      Offset : Positive := 1;
      use kv.avm.Instructions;
   begin
      a := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, ":=");
      x := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, "=>");
      y := Make_Reference(Nth(Rest, Offset));
      case Op_Code is -- PEEK, TRAP, NEW_ACTOR
         when PEEK =>
            return (Op_Code => PEEK, a => a, x => x, y => y);
         when TRAP =>
            return (Op_Code => TRAP, a => a, x => x, y => y);
         when NEW_ACTOR =>
            return (Op_Code => NEW_ACTOR, a => a, x => x, y => y);
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_3");
         raise;
   end Build_Format_3;


   ----------------------------------------------------------------------------
   function Build_Format_3b(Op_Code : kv.avm.Instructions.format_3b_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      use kv.avm.Instructions;
      target       : Reference_Type;
      source_tuple : Reference_Type;
      index        : Offset_Type;
      Offset : Positive := 1;
   begin
      target := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, ":=");
      source_tuple := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, "[");
      index := offset_type'VALUE(Nth(Rest, Offset));
      return (Op_Code => PEEK_IMMEDIATE, lvalue => target, tuple => source_tuple, index => index);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_3b");
         raise;
   end;

   ----------------------------------------------------------------------------
   function Build_Format_4(Op_Code : kv.avm.Instructions.format_4_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      a : reference_type;
      x : reference_type;
      y : reference_type;
      op : kv.avm.Instructions.operation_type;
      Offset : Positive;
      use kv.avm.Instructions;
   begin
      a := Make_Reference(Nth(Rest, 1));
      -- Nth 2 should be ":="
      if Nth(Rest, 2) = ":=" then
         Offset := 3;
      else
         Offset := 2;
      end if;
      x := Make_Reference(Nth(Rest, Offset));
      op := Decode_Operation(Nth(Rest, Offset+1));
      y := Make_Reference(Nth(Rest, Offset+2));
      return (Op_Code => COMPUTE, action => op, result => a, left => x, right => y);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_4");
         raise;
   end Build_Format_4;


   ----------------------------------------------------------------------------
   function Build_Format_5a1(Op_Code : kv.avm.Instructions.format_5a1_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      use kv.avm.Instructions;
      reply_register     : Reference_Type;
      argument_tuple     : Reference_Type;
      actor_reference    : Reference_Type;
      message_definition : Reference_Type;
      Offset : Positive := 1;
   begin
      reply_register := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, ":=");
      argument_tuple := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, "=>");
      actor_reference := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, ".");
      message_definition := Make_Reference(Nth(Rest, Offset));
      return (Op_Code => ACTOR_CALL, rply_5a1 => reply_register, args_5a1 => argument_tuple, actr_5a1 => actor_reference, mdef_5a1 => message_definition);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_5a1");
         raise;
   end;


   ----------------------------------------------------------------------------
   function Build_Format_5a2(Op_Code : kv.avm.Instructions.format_5a2_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      use kv.avm.Instructions;
      argument_tuple     : Reference_Type;
      actor_reference    : Reference_Type;
      message_definition : Reference_Type;
      Offset : Positive := 1;
   begin
      argument_tuple := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, "=>");
      actor_reference := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, ".");
      message_definition := Make_Reference(Nth(Rest, Offset));
      case Op_Code is -- ACTOR_SEND, ACTOR_TAIL_SEND, ACTOR_TAIL_CALL
         when ACTOR_SEND =>
            return (Op_Code => ACTOR_SEND     , args_5a2 => argument_tuple, actr_5a2 => actor_reference, mdef_5a2 => message_definition);
         when ACTOR_TAIL_SEND =>
            return (Op_Code => ACTOR_TAIL_SEND, args_5a2 => argument_tuple, actr_5a2 => actor_reference, mdef_5a2 => message_definition);
         when ACTOR_TAIL_CALL =>
            return (Op_Code => ACTOR_TAIL_CALL, args_5a2 => argument_tuple, actr_5a2 => actor_reference, mdef_5a2 => message_definition);
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_5a2");
         raise;
   end;


   ----------------------------------------------------------------------------
   function Build_Format_5b1(Op_Code : kv.avm.Instructions.format_5b1_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      rpl : reference_type;
      msg : reference_type;
      arg : reference_type;
      Offset : Positive := 1;
      use kv.avm.Instructions;
   begin
      rpl := Make_Reference(Nth(Rest, 1));
      Advance_And_Skip_If(Offset, Rest, ":=");
      arg := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, "=>");
      msg := Make_Reference(Nth(Rest, Offset));
      case Op_Code is -- SELF_CALL, SUPER_CALL
         when SELF_CALL =>
            return (Op_Code => SELF_CALL, rply_5b1 => rpl, mdef_5b1 => msg, args_5b1 => arg);
         when SUPER_CALL =>
            return (Op_Code => SUPER_CALL, rply_5b1 => rpl, mdef_5b1 => msg, args_5b1 => arg);
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_5b1");
         raise;
   end;


   ----------------------------------------------------------------------------
   function Build_Format_5b2(Op_Code : kv.avm.Instructions.format_5b2_type; Rest : String) return kv.avm.Instructions.Instruction_Type is
      msg : reference_type;
      arg : reference_type;
      Offset : Positive := 1;
      use kv.avm.Instructions;
   begin
      arg := Make_Reference(Nth(Rest, Offset));
      Advance_And_Skip_If(Offset, Rest, "=>");
      msg := Make_Reference(Nth(Rest, Offset));
      case Op_Code is -- SELF_SEND, SELF_TAIL_SEND, SELF_TAIL_CALL, SUPER_TAIL_CALL
         when SELF_SEND =>
            return (Op_Code => SELF_SEND, mdef_5b2 => msg, args_5b2 => arg);
         when SELF_TAIL_SEND =>
            return (Op_Code => SELF_TAIL_SEND, mdef_5b2 => msg, args_5b2 => arg);
         when SELF_TAIL_CALL =>
            return (Op_Code => SELF_TAIL_CALL, mdef_5b2 => msg, args_5b2 => arg);
         when SUPER_TAIL_CALL =>
            return (Op_Code => SUPER_TAIL_CALL, mdef_5b2 => msg, args_5b2 => arg);
      end case;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Build_Format_5b2");
         raise;
   end;


   ----------------------------------------------------------------------------
   function Decode_Op_Code(Line : String) return kv.avm.Instructions.Instruction_Type is
      Name        : constant String := First(Line);
      The_Rest    : constant String := Rest(Drop_Vole_Comments(Line));
      Op_Code     : kv.avm.Instructions.op_code_type;
      Instruction : kv.avm.Instructions.Instruction_Type;

   begin
      begin
         Op_Code := kv.avm.Instructions.op_code_type'VALUE(Name);
      exception
         when others =>
            raise Invalid_Op_Code;
      end;
      --Put_Line("Op_Code is "&kv.avm.instruction.op_code_type'IMAGE(Op_Code));

      case Op_Code is
         when kv.avm.Instructions.format_0_type =>
            Instruction := Build_Format_0(Op_Code);

         when kv.avm.Instructions.format_1_type =>
            Instruction := Build_Format_1(Op_Code, The_Rest);

         when kv.avm.Instructions.format_1b_type =>
            Instruction := Build_Format_1b(Op_Code, The_Rest);

         when kv.avm.Instructions.format_2_type =>
            Instruction := Build_Format_2(Op_Code, The_Rest);

         when kv.avm.Instructions.format_2b_type =>
            Instruction := Build_Format_2b(Op_Code, The_Rest);

         when kv.avm.Instructions.format_3_type =>
            Instruction := Build_Format_3(Op_Code, The_Rest);

         when kv.avm.Instructions.format_3b_type =>
            Instruction := Build_Format_3b(Op_Code, The_Rest);

         when kv.avm.Instructions.format_4_type =>
            Instruction := Build_Format_4(Op_Code, The_Rest);

         when kv.avm.Instructions.format_5a1_type =>
            Instruction := Build_Format_5a1(Op_Code, The_Rest);

         when kv.avm.Instructions.format_5a2_type =>
            Instruction := Build_Format_5a2(Op_Code, The_Rest);

         when kv.avm.Instructions.format_5b1_type =>
            Instruction := Build_Format_5b1(Op_Code, The_Rest);

         when kv.avm.Instructions.format_5b2_type =>
            Instruction := Build_Format_5b2(Op_Code, The_Rest);

         -- Don't include a "when others" because that could lead to erroneous behavior if instructions are added.
      end case;
      return Instruction;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Decode_Op_Code");
         raise;
   end Decode_Op_Code;


   ----------------------------------------------------------------------------
   procedure Parse_Line
      (Self : in out Message_Type;
       Line : in     String) is

   begin
      if First(Line) = "...predicate" then
         Self.Has_P := True;
         Self.Pred := kv.avm.References.Offset_Type'VALUE(Second(Line));
      else
         Self.Code(Interfaces.Unsigned_32(Self.Count)) := Decode_Op_Code(Line);
         Self.Count := Self.Count + 1;
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Parse_Line(Message_Type)");
         raise;
   end Parse_Line;


   ----------------------------------------------------------------------------
   function Constructor_Code(Self : Message_Type) return kv.avm.Instructions.Code_Access is
   begin
      if Self.Name.all = "CONSTRUCTOR" then
         return new kv.avm.Instructions.Code_Type'(Self.Code(0 .. Interfaces.Unsigned_32(Self.Count)));
      else
         return Self.Next.Constructor_Code;
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Constructor_Code(Message_Type)");
         raise;
   end Constructor_Code;


   ----------------------------------------------------------------------------
   procedure Add_Non_Constructors
      (Self  : in out Message_Type;
       Actor : in     kv.avm.Actors.Actor_Access) is

      Method : kv.avm.Methods.Method_Access;

   begin
      if Self.Next /= null then
         Self.Next.Add_Non_Constructors(Actor);
      end if;
      if Self.Name.all /= "CONSTRUCTOR" then
         Method := New_Method(Self.Name.all, new kv.avm.Instructions.Code_Type'(Self.Code(0 .. Interfaces.Unsigned_32(Self.Count))));
         if Self.Has_P then
            Method.Add_Predicate(Self.Pred);
         end if;
         Actor.Add_Method(Method);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Add_Non_Constructors(Message_Type)");
         raise;
   end Add_Non_Constructors;


   ----------------------------------------------------------------------------
   procedure Write_Word_Code
      (Self : in out Message_Type;
       File : in     Ada.Streams.Stream_IO.Stream_Access) is
   begin
      --Put_Line("Write_Word_Code:Message="&Self.Name.all);
      String'OUTPUT(File, Self.Name.all);
      Boolean'OUTPUT(File, Self.Has_P);
      if Self.Has_P then
         kv.avm.References.Offset_Type'OUTPUT(File, Self.Pred);
      end if;
      --Put_Line("Write_Word_Code:instruction count="&Natural'IMAGE(Self.Count));
      Natural'WRITE(File, Self.Count);
      for I in Self.Code'FIRST .. Interfaces.Unsigned_32(Self.Count-1) loop
         --Put_Line(Interfaces.Unsigned_32'IMAGE(I));
         Interfaces.Unsigned_64'WRITE(File, Convert(Self.Code(I)));
      end loop;
      if Self.Next /= null then
         Self.Next.Write_Word_Code(File);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Write_Word_Code(Message_Type)");
         raise;
   end Write_Word_Code;


   ----------------------------------------------------------------------------
   function New_From_Stream(Count : Natural; Stream : Ada.Streams.Stream_IO.Stream_Access) return Message_Access is
      Self : Message_Access;
      Uint : Interfaces.Unsigned_64;
   begin
      --Put_Line("New_From_Stream return Message_Access, count="&Natural'IMAGE(Count));
      Self := new Message_Type;
      Self.Name := new String'(String'INPUT(Stream));
      Boolean'READ(Stream, Self.Has_P);
      if Self.Has_P then
         kv.avm.References.Offset_Type'READ(Stream, Self.Pred);
      end if;
      --Put_Line("New_From_Stream Name="&Self.Name.all);
      Natural'READ(Stream, Self.Count);
      --Put_Line("New_From_Stream instruction count="&Natural'IMAGE(Self.Count));
      for I in Self.Code'FIRST .. Interfaces.Unsigned_32(Self.Count-1) loop
         --Put_Line(Interfaces.Unsigned_32'IMAGE(I));
         Interfaces.Unsigned_64'READ(Stream, Uint);
         Self.Code(I) := Convert(Uint);
      end loop;
      if Count > 1 then
         Self.Next := New_From_Stream(Count-1, Stream);
      end if;
      return Self;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.New_From_Stream(Message_Type)");
         raise;
   end New_From_Stream;





   ----------------------------------------------------------------------------
   procedure Initialize
      (Self  : access Actor_Type;
       Name  : in     String;
       Next  : in     Actor_Pointer) is
   begin
      Self.Name    := new String'(Name);
      Self.Section := Constants_Section;
      Self.Next    := Next;
      Self.F_Max   := 0;
      Self.M_Count := 0;
      Self.Message := null;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Initialize(Actor_Type)");
         raise;
   end Initialize;


   Message_Directive : constant String := "..message";
   Constants_Directive : constant String := "..constants";
   Subclassof_Directive : constant String := "..subclassof";

   ----------------------------------------------------------------------------
   function Is_Message_Directive(Line : String) return Boolean is
   begin
      return First(Line) = Message_Directive;
   end Is_Message_Directive;

   ----------------------------------------------------------------------------
   function Is_Constants_Directive(Line : String) return Boolean is
   begin
      return First(Line) = Constants_Directive;
   end Is_Constants_Directive;

   ----------------------------------------------------------------------------
   function Is_Subclassof_Directive(Line : String) return Boolean is
   begin
      return First(Line) = Subclassof_Directive;
   end Is_Subclassof_Directive;

   ----------------------------------------------------------------------------
   procedure Parse_Message_Directive
      (Self : in out Actor_Type;
       Line : in     String) is

      Next_Message : Message_Access;
      Message_Name : constant String := Second(Line);

   begin
      -- Push a message
      Self.M_Count := Self.M_Count + 1;
      Next_Message := Self.Message;
      Self.Message := new Message_Type;
      Self.Message.Initialize(Message_Name, Next_Message);
      Self.Section := Message_Section;
   end Parse_Message_Directive;

   ----------------------------------------------------------------------------
   procedure Parse_Constants_Directive
      (Self : in out Actor_Type;
       Line : in     String) is
   begin
      Self.Section := Constants_Section;
   end Parse_Constants_Directive;

   ----------------------------------------------------------------------------
   procedure Parse_Subclassof_Directive
      (Self : in out Actor_Type;
       Line : in     String) is
      Parent_Name : constant String := Second(Line);
   begin
      Self.Parent := new String'(Parent_Name);
   end Parse_Subclassof_Directive;

   ----------------------------------------------------------------------------
   procedure Parse_Actor_Constants -- pre-condition: line must be clean
      (Self : in out Actor_Type;
       Line : in     String) is

      -- Accepted formats:
      --  register type value
      --  register : type value
      --  register : type := value (prefered)
      --  register type := value

      Constant_Ref : reference_type;
      Type_Offset  : Positive := 1;
      Value_Offset : Positive;
      Index        : Interfaces.Unsigned_32;

   begin
      -- Collect the constant
      Constant_Ref := Make_Reference(First(Line));
      Index := Interfaces.Unsigned_32(Constant_Ref.index);

      Advance_And_Skip_If(Type_Offset, Line, ":");
      Value_Offset := Type_Offset;
      Advance_And_Skip_If(Value_Offset, Line, ":=");

      declare
         Type_Image : constant String := Nth(Line, Type_Offset);
         Value_Image : constant String := Select_Value(Nth_Rest(Line, Value_Offset-1));
      begin
         Self.Fixed(Index) := Build_Constant(Type_Image, Value_Image);
      end;

      -- And finally, keep track of the last used constant
      if Natural(Constant_Ref.index) > Self.F_Max then
         Self.F_Max := Natural(Constant_Ref.index);
      end if;
   end Parse_Actor_Constants;

   ----------------------------------------------------------------------------
   procedure Parse_Actor_Section
      (Self : in out Actor_Type;
       Line : in     String) is
   begin
      case Self.Section is
         when Constants_Section =>
            Parse_Actor_Constants(Self, Line);
         when Message_Section =>
            Self.Message.Parse_Line(Line);
      end case;
   end Parse_Actor_Section;

   ----------------------------------------------------------------------------
   procedure Parse_Line
      (Self : in out Actor_Type;
       Line : in     String) is

      Clean : constant String := Drop_Vole_Comments(Line);

   begin
      if Is_Message_Directive(Clean) then
         Parse_Message_Directive(Self, Clean);
      elsif Is_Constants_Directive(Clean) then
         Parse_Constants_Directive(Self, Clean);
      elsif Is_Subclassof_Directive(Clean) then
         Parse_Subclassof_Directive(Self, Clean);
      else
         Parse_Actor_Section(Self, Clean);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Parse_Line(Actor_Type)");
         raise;
   end Parse_Line;


   ----------------------------------------------------------------------------
   procedure Load
      (Self : in out Actor_Type) is
      Actor_Constants : kv.avm.Memories.Register_Array_Type;
      Parent : kv.avm.Actors.Actor_Access;
      use type kv.avm.Actors.Actor_Access;
   begin
      if Self.Next /= null then
         Self.Next.Load;
      end if;
      Actor_Constants.Initialize(Self.Fixed(0 .. Interfaces.Unsigned_32(Self.F_Max)));
      if Self.Parent /= null then
         Parent := kv.avm.Actors.Get_Actor_By_Name(self.Parent.all);
      end if;
      Self.Loaded := kv.avm.Actors.New_Actor
         (Name            => Self.Name.all,
          Constructor     => Self.Message.Constructor_Code,
          Attribute_Count => 64,
          Fixed_Registers => Actor_Constants,
          Parent          => Parent);
      Self.Message.Add_Non_Constructors(Self.Loaded);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Load(Actor_Type), actor = " & Self.Name.all);
         raise;
   end Load;

   ----------------------------------------------------------------------------
   procedure Link
      (Self : in out Actor_Type) is

      Parent : kv.avm.Actors.Actor_Access;
      use type kv.avm.Actors.Actor_Access;

   begin
      if Self.Next /= null then
         Self.Next.Link;
      end if;
      if Self.Parent /= null then
         Parent := kv.avm.Actors.Get_Actor_By_Name(Self.Parent.all);
         Self.Loaded.Add_Parent(Parent);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Link(Actor_Type), actor = " & Self.Name.all);
         raise;
   end Link;


   ----------------------------------------------------------------------------
   procedure Write_Word_Code
      (Self : in out Actor_Type;
       File : in     Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Put_Line("Write_Word_Code:Actor=" & Self.Name.all);
      String'OUTPUT(File, Self.Name.all);
      if Self.Parent /= null then
         Put_Line("Write_Word_Code:Parent=" & Self.Parent.all);
         String'OUTPUT(File, Self.Parent.all);
      else
         Put_Line("Write_Word_Code:Parent=<none>");
         String'OUTPUT(File, "<none>");
      end if;
      Put_Line("Write_Word_Code:fixed max=" & Natural'IMAGE(Self.F_Max));
      Natural'WRITE(File, Self.F_Max);
      for I in Self.Fixed'FIRST .. Interfaces.Unsigned_32(Self.F_Max) loop
         Put_Line(Interfaces.Unsigned_32'IMAGE(I) & ": " & kv.avm.Registers.Reg_Img(Self.Fixed(I)));
         kv.avm.Registers.Register_Type'WRITE(File, Self.Fixed(I));
      end loop;
      Put_Line("Write_Word_Code:message count="&Natural'IMAGE(Self.M_Count));
      Natural'WRITE(File, Self.M_Count);
      Self.Message.Write_Word_Code(File);
      if Self.Next /= null then
         Self.Next.Write_Word_Code(File);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Write_Word_Code(Actor_Type)");
         raise;
   end Write_Word_Code;


   ----------------------------------------------------------------------------
   function New_From_Stream(Count : Natural; Stream : Ada.Streams.Stream_IO.Stream_Access) return Actor_Pointer is
      Self : Actor_Pointer;
   begin
      Put_Line("New_From_Stream return Actor_Pointer, count=" & Natural'IMAGE(Count));
      Self := new Actor_Type;
      Self.Name := new String'(String'INPUT(Stream));
      Put_Line("New_From_Stream Name=" & Self.Name.all);
      Self.Parent := new String'(String'INPUT(Stream));
      Put_Line("New_From_Stream Parent=" & Self.Parent.all);
      if Self.Parent.all = "<none>" then
         Free(Self.Parent);
      end if;
      Natural'READ(Stream, Self.F_Max);
      Put_Line("New_From_Stream F_Max=" & Natural'IMAGE(Self.F_Max));
      for I in Self.Fixed'FIRST .. Interfaces.Unsigned_32(Self.F_Max) loop
         Put_Line(Interfaces.Unsigned_32'IMAGE(I));
         kv.avm.Registers.Register_Type'READ(Stream, Self.Fixed(I));
      end loop;
      Natural'READ(Stream, Self.M_Count);
      Put_Line("New_From_Stream M_Count=" & Natural'IMAGE(Self.M_Count));
      if Self.M_Count > 0 then
         Put_Line("New_From_Stream return Actor_Pointer: reading actor " & Self.Name.all & ", M_Count=" & Positive'IMAGE(Positive(Self.M_Count)));
         Self.Message := New_From_Stream(Self.M_Count, Stream);
      end if;
      if Count > 1 then
         Self.Next := New_From_Stream(Count-1, Stream);
      end if;
      return Self;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.New_From_Stream(Actor_Type)");
         raise;
   end New_From_Stream;





   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : access Assembler_Type) is
   begin
      Self.Lines_Parsed := 0;
      Self.Actor_Count := 0;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Initialize(Assembler_Type)");
         raise;
   end Initialize;


   Comment_Directive : constant Character := '#';
   Control_Directive : constant Character := '.';
   Actor_Directive : constant String := ".actor";

   ----------------------------------------------------------------------------
   function Is_Comment_Line(Stripped : String) return Boolean is
   begin
      return Stripped(Stripped'FIRST) = Comment_Directive;
   end Is_Comment_Line;

   ----------------------------------------------------------------------------
   function Is_Control_Line(Stripped : String) return Boolean is
   begin
      return (Stripped(Stripped'FIRST) = Control_Directive) and
             ((Stripped'LENGTH >= 2) and then (Stripped(Stripped'FIRST+1) /= Control_Directive));
   end Is_Control_Line;

   ----------------------------------------------------------------------------
   function Is_Actor_Directive(Line : String) return Boolean is
   begin
      return First(Line) = Actor_Directive;
   end Is_Actor_Directive;

   ----------------------------------------------------------------------------
   procedure Parse_Actor_Directive
      (Self : in out Assembler_Type;
       Line : in     String) is

      Next_Actor : Actor_Pointer;
      Actor_Name : constant String := Second(Line);

   begin
      Self.Actor_Count := Self.Actor_Count + 1;
      Next_Actor := Self.Actor;
      Self.Actor := new Actor_Type;
      Self.Actor.Initialize(Actor_Name, Next_Actor);
   end Parse_Actor_Directive;

   ----------------------------------------------------------------------------
   procedure Parse_Line
      (Self : in out Assembler_Type;
       Line : in     String) is

      Stripped : constant String := Trim_Blanks(Line);

   begin
      Put_Line("<"&Stripped&">");
      Self.Lines_Parsed := Self.Lines_Parsed + 1;
      if Stripped = "" then
         null; -- blank
      elsif Is_Comment_Line(Stripped) then
         null; -- comment
      elsif Is_Control_Line(Stripped) then
         if Is_Actor_Directive(Stripped) then
            Parse_Actor_Directive(Self, Stripped);
         else
            Put_Error("Unrecognized directive on line " & Img(Self.Lines_Parsed));
         end if;
      else
         Self.Actor.Parse_Line(Stripped);
      end if;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Parse_Line(Assembler_Type)");
         raise;
   end Parse_Line;


   ----------------------------------------------------------------------------
   package Assembler_Reader is new kv.avm.File_Reader(Assembler_Type);

   ----------------------------------------------------------------------------
   procedure Parse_Input_File
      (Self    : in out Assembler_Type;
       File_In : in     String) renames Assembler_Reader.Parse_Input_File;

   ----------------------------------------------------------------------------
   procedure Transfer_Actors_To_Store
      (Self : in out Assembler_Type) is
   begin
      Self.Actor.Load;
      Self.Actor.Link;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Transfer_Actors_To_Store(Assembler_Type)");
         raise;
   end Transfer_Actors_To_Store;


   ----------------------------------------------------------------------------
   procedure Write_Word_Code
      (Self : in out Assembler_Type;
       File : in     Ada.Streams.Stream_IO.Stream_Access) is
   begin
      Integer'WRITE(File, Word_Code_Identifier_Number);
      String'WRITE(File, Word_Code_Human_Id); -- Fixed length, no dope data saved
      --Put_Line("Write_Word_Code:assember, Actor_Count="&Natural'IMAGE(Self.Actor_Count));
      Natural'WRITE(File, Self.Actor_Count);
      Self.Actor.Write_Word_Code(File);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Write_Word_Code(Assembler_Type)");
         raise;
   end Write_Word_Code;


   ----------------------------------------------------------------------------
   procedure Read_Word_Code
      (Self : in out Assembler_Type;
       File : in     Ada.Streams.Stream_IO.Stream_Access) is

      Id        : Integer;
      Code_Word : String(1..8);
      Count     : Natural;

   begin
      Integer'READ(File, Id);
      String'READ(File, Code_Word); -- Fixed length, no dope data saved
      if Id /= Word_Code_Identifier_Number or Code_Word /= Word_Code_Human_Id then
         raise Invalid_Word_Code_Header;
      end if;
      Natural'READ(File, Count);
      --Put_Line("Read_Word_Code:assember, Actor_Count="&Natural'IMAGE(Count));
      Self.Actor_Count := Count;
      Self.Actor := New_From_Stream(Count, File);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Assemblers.Read_Word_Code(Assembler_Type)");
         raise;
   end Read_Word_Code;

end kv.avm.Assemblers;
