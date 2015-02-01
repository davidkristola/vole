with Ada.Exceptions; use Ada.Exceptions;

with kv.avm.Symbol_Tables;
with kv.avm.References;
with kv.avm.Instructions;
with kv.avm.Registers; use kv.avm.Registers;
with kv.avm.Log; use kv.avm.Log;

package body kv.avm.Code_Generator is

   ----------------------------------------------------------------------------
   procedure Init
      (Self : in out Code_Generator_Class) is
   begin
      null;
   end Init;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self      : in out Code_Generator_Class) is
   begin
      null;
   end Finalize;



   ----------------------------------------------------------------------------
   procedure Explore_Next
      (Self   : in out Code_Generator_Class;
       Target : in     Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      Next : Node_Pointer;

   begin
      Next := Target.Get_Association(My_Next);
      if Next /= null then
         Next.Visit(Self'ACCESS, Depth);
      end if;
   end Explore_Next;

   ----------------------------------------------------------------------------
   procedure Explore
      (Self    : in out Code_Generator_Class;
       Target  : in     Node_Base_Class'CLASS;
       Depth   : in     Natural;
       Go_Next : in     Boolean := True) is

      Child : Node_Pointer;

   begin
      for Association in Child_Association_Type loop
         Child := Target.Get_Association(Association);
         if Child /= null then
            Child.Visit(Self'ACCESS, Depth+1);
         end if;
      end loop;
      if Go_Next then
         Explore_Next(Self, Target, Depth);
      end if;
   end Explore;





   ----------------------------------------------------------------------------
   procedure Visit_Id
      (Self   : in out Code_Generator_Class;
       Target : in out Id_Node_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Id, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Actor_Definition
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      Actor_Node : Actor_Definition_Class;
      Table : access kv.avm.Symbol_Tables.Symbol_Table;
      Super_Name_Node : Node_Pointer;

      ----------------------------------------------------------------------------
      procedure Print_Symbol
         (Name : in String;
          Kind : in kv.avm.Registers.Data_Kind;
          Indx : in Natural;
          Init : in String) is

         function Default return String is
         begin
            if Init = "" then
               return "";
            end if;
            return " := " & Init;
         end Default;
         function Constant_Name(Index : Natural) return String is
            Img : String := Natural'IMAGE(Index);
         begin
            Img(1) := 'C';
            return Img;
         end Constant_Name;

      begin
         Self.Buffer.Put_Meta("  " & Constant_Name(Indx) & " : " & kv.avm.Registers.Data_Kind'IMAGE(Kind) & Default & " # " & Name);
      end Print_Symbol;

   begin
      Actor_Node := Actor_Definition_Class(Target);
      Super_Name_Node := Actor_Node.Get_Super_Class;
      Self.Buffer.Put_Meta(".actor " & Target.Get_Name);
      if Super_Name_Node /= null then
         Self.Buffer.Put_Meta("..subclassof " & Super_Name_Node.Get_Name);
      end if;
      Self.Buffer.Put_Meta("..constants");
      Table := Actor_Definition_Class(Target).Get_Symbol_Table(CONSTANT_SYMBOLS);
      Table.For_Each(Print_Symbol'ACCESS);

      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Actor_Definition, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Attribute_Definition
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Attribute_Definition, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Message_Definition
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      Predicate : Node_Pointer := Target.Get_Association(My_Condition);

      function Offset_For(P : Node_Pointer) return String is
         Image : constant String := Resolve_Register(P.all);
      begin
         return Image(2 .. Image'LAST);
      end Offset_For;

   begin
      Self.Buffer.Put_Meta("..message " & Target.Get_Name);
      if Predicate /= null then
         Self.Buffer.Put_Meta("...predicate " & Offset_For(Predicate));
      end if;
      Explore(Self, Target, Depth, False);
      Self.Buffer.Put_Instruction("  STOP_FRAME");
      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Message_Definition, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Kind_Node
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Kind_Node, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Argument
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Argument, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Constructor_Send_Node
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      Father : Node_Pointer := Target.Get_Association(My_Parent);
      Grandpa : Node_Pointer := Father.Get_Association(My_Parent);

      Arguments : Node_Pointer := Target.Get_Association(My_Arguments);
      New_Actor : Node_Pointer := Target.Get_Association(My_Destination);

      Out_Count : Positive;

      procedure Put_New_Actor(Arguments : in     String) is
      begin
         Self.Buffer.Put_Instruction("  NEW_ACTOR " & Resolve_Register(Grandpa.all) & " := " & Arguments & " => " & Resolve_Register(New_Actor.all));
      end Put_New_Actor;

   begin
      Explore(Self, Target, Depth);
      Out_Count := Arguments.Get_Length;
      if Out_Count > 1 then
         Self.Buffer.Put_Comment("  # fold " & Positive'IMAGE(Out_Count) & " arguments.");
         Put_New_Actor("the_new_fold");
      else
         Put_New_Actor(Resolve_Register(Arguments.all));
      end if;
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Constructor_Send_Node, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_List
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Expression_List, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Op
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      Rhs : Node_Pointer := Target.Get_Association(My_Right);
      Lhs : Node_Pointer := Target.Get_Association(My_Left);

   begin
      Explore(Self, Target, Depth);
      if Lhs = null then
         -- emit a unary set
         --!@#$ what about transformative unary operators like '-' or abs?
         --!@#$ maybe emit nothing! Self.Buffer.Put_Instruction("  SET " & Resolve_Register(Target) & " := " & Resolve_Register(Rhs.all));
         null;
      else
         Self.Buffer.Put_Instruction("  COMPUTE " & Resolve_Register(Target) & " := " & Resolve_Register(Lhs.all) & " " & kv.avm.Instructions.Encode_Operation(Expression_Op_Class(Target).Get_Op) & " " & Resolve_Register(Rhs.all));
      end if;
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Expression_Op, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Var
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Expression_Var, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Literal
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Expression_Literal, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Send
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      E : access Expression_Call_Class := Expression_Call_Class(Target)'ACCESS;

      procedure Format_5a2(Instruction : String) is
      begin
         Self.Buffer.Put_Instruction("  " & Instruction & " " &
            Resolve_Register(Target.Get_Association(My_Arguments).all) & " => " &
            Resolve_Register(Target.Get_Association(My_Destination).all) & " . " &
            Resolve_Register(Target.Get_Association(My_Name).all)); -- 5a2
      end Format_5a2;

      procedure Format_5b2(Instruction : String) is
      begin
         Self.Buffer.Put_Instruction("  " & Instruction & " " &
            Resolve_Register(Target.Get_Association(My_Arguments).all) & " => " &
            Resolve_Register(Target.Get_Association(My_Name).all)); -- 5b2
      end Format_5b2;

   begin
      Explore(Self, Target, Depth);
      if E.Is_Gosub then
         if E.Is_Call then
            if E.Is_Tail then
               Format_5b2("SELF_TAIL_CALL");
            else
               Self.Buffer.Put_Instruction("  SELF_CALL " & Resolve_Register(Target) & " := " &
                  Resolve_Register(Target.Get_Association(My_Arguments).all) & " => " &
                  Resolve_Register(Target.Get_Association(My_Name).all)
                  ); -- 5b1
            end if;
         else
            if E.Is_Tail then
               Format_5b2("SELF_TAIL_SEND");
            else
               Format_5b2("SELF_SEND");
            end if;
         end if;
      else
         if E.Is_Call then
            if E.Is_Tail then
               Format_5a2("ACTOR_TAIL_CALL");
            else
               Self.Buffer.Put_Instruction("  ACTOR_CALL " & Resolve_Register(Target) & " := " &
                  Resolve_Register(Target.Get_Association(My_Arguments).all) & " => " &
                  Resolve_Register(Target.Get_Association(My_Destination).all) & " . " &
                  Resolve_Register(Target.Get_Association(My_Name).all)); -- 5a1
            end if;
         else
            if E.Is_Tail then
               Format_5a2("ACTOR_TAIL_SEND");
            else
               Format_5a2("ACTOR_SEND");
            end if;
         end if;
      end if;
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Expression_Send, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Fold
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      E : access Expression_Fold_Class := Expression_Fold_Class(Target)'ACCESS;
   begin
      Explore(Self, Target, Depth);
      Self.Buffer.Put_Instruction("  FOLD " & Resolve_Register(Target) & " := " & Resolve_Ref_Name(Target, E.Get_Tuple_Map_Name) &
         " # [" & E.Get_Tuple_Map_Init & "]");
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Expression_Fold, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_List
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth, False);
      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_List, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Assign
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      Dst : Node_Pointer := Target.Get_Association(My_Destination);
      Src : Node_Pointer := Target.Get_Association(My_Value);

      Destination : constant String := Resolve_Name(Dst.all);
      Source : constant String := Resolve_Name(Src.all);

   begin
      declare
         Dst_Reg : constant String := Resolve_Register(Dst.all, True);
         Src_Reg : constant String := Resolve_Register(Src.all, True);
         Dst_Kind : kv.avm.Registers.Data_Kind := Dst.Get_Kind;
         Src_Kind : kv.avm.Registers.Data_Kind := Src.Get_Kind;

      begin
         Explore(Self, Target, Depth, False);
         -- If the source is a 1-tuple and the dest is not, then we are unfolding, not setting
         if Src_Kind = Tuple and Dst_Kind /= Tuple then
            Self.Buffer.Put_Instruction("  PEEK_IMMEDIATE " & Dst_Reg & " := " & Src_Reg & " [ 0 ]");
         else
            Self.Buffer.Put_Instruction("  SET " & Dst_Reg & " := " & Src_Reg & " # " & Destination & " := " & Source);
         end if;
         Explore_Next(Self, Target, Depth);
      end;
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when kv.avm.vole_tree.Variable_Undefined =>
         Put_Error("Error: assignment statement (line" & Positive'IMAGE(Target.Get_Line) & ") to undefined varaiable '" & Destination & "'.");
         raise Terminate_Parsing;
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_Assign, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Var_Def
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      Kind_Node : Node_Pointer := Target.Get_Association(My_Kind);
      Init_Node : Node_Pointer;

      procedure Initialize_Var(Src : Node_Pointer; Dst : Node_Pointer) is
      begin
         declare
            Destination : constant String := Resolve_Name(Dst.all);
            Source : constant String := Resolve_Name(Src.all);
            Dst_Reg : constant String := Resolve_Register(Dst.all, True);
            Src_Reg : constant String := Resolve_Register(Src.all, True);
            Dst_Kind : kv.avm.Registers.Data_Kind := Dst.Get_Kind;
            Src_Kind : kv.avm.Registers.Data_Kind := Src.Get_Kind;
         begin
            --!@#$ new_actor makes this redundant (and not work)
            if Source /= "Could not Resolve_Name" then
               Self.Buffer.Put_Instruction("  SET " & Dst_Reg & " := " & Src_Reg & " # " & Destination & " := " & Source);
            end if;
         end;
      exception
         when Unresolveable_Name =>
            null; -- It is okay to skip this if the name can't be resolved.
         when Variable_Undefined =>
            null; -- It is okay to skip this if the name can't be resolved.
      end Initialize_Var;

   begin
      --Self.Buffer.Put_Comment("  # local variable " & Target.Get_Name & " defined.");
      Explore(Self, Target, Depth, False);
      if Kind_Node /= null then
         Init_Node := Kind_Node.Get_Association(My_Value);
         if Init_Node /= null then
            Initialize_Var(Init_Node, Target.Get_Association(My_Name));
         end if;
      else
         null; --!@#$ This should be an internal error because the grammar should not let us get here.
      end if;
      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_Var_Def, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Emit
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
       Src_Reg : constant String := Resolve_Register(Target.Get_Association(My_Value).all);
   begin
      Explore(Self, Target, Depth, False);
      Self.Buffer.Put_Instruction("  EMIT " & Src_Reg);
      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_Emit, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Return
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
       Value : Node_Pointer := Target.Get_Association(My_Value);
       Method : Node_Pointer := Message_Of(Target);
       Out_Count : Positive;
   begin
      Explore(Self, Target, Depth, False);
      if Value = null then
         -- Return the specified value(s)
         Out_Count := Method.Get_Association(My_Outputs).Get_Length;
         if Out_Count = 1 then
            -- default single output (always in local 0)
            Self.Buffer.Put_Instruction("  REPLY " & kv.avm.References.Make_Register_Name(0, kv.avm.References.Local));
         else
            Self.Buffer.Put_Instruction("  fold " & Positive'IMAGE(Out_Count) & " parameter(s).");
            Self.Buffer.Put_Instruction("  REPLY x # default out of" & Positive'IMAGE(Out_Count) & " parameter(s).");
         end if;
      else
         -- Return some non-default value
         if Value.all in Expression_Call_Class'CLASS then
            -- Tail call
            null; -- Nothing to return since a tail call was made.
         else
            Self.Buffer.Put_Instruction("  REPLY " & Resolve_Register(Target.Get_Association(My_Value).all)); -- Something else
         end if;
      end if;
      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_Return, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_If
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      use kv.avm.Code_Buffers;

      Condition_Node : Node_Pointer := Target.Get_Association(My_Condition);
      Then_Part_Node : Node_Pointer := Target.Get_Association(My_Then_Part);
      Else_Part_Node : Node_Pointer := Target.Get_Association(My_Else_Part);

      Src_Reg : constant String := Resolve_Register(Condition_Node.all);
      Skip_Then_Count : Natural := 0;
      Skip_Else_Count : Natural := 0;

      Main_Buffer : kv.avm.Code_Buffers.Buffer_Type;
      Then_Buffer : kv.avm.Code_Buffers.Buffer_Type;
      Else_Buffer : kv.avm.Code_Buffers.Buffer_Type;

      procedure Visit_Part(Part_Node : in Node_Pointer; Buffer : in out Buffer_Type; Count : out Natural) is
      begin
         Self.Buffer := Buffer;
         if Part_Node /= null then
            Part_Node.Visit(Self'ACCESS, Depth + 1);
         end if;
         Buffer := Self.Buffer;
         Count := Buffer.Code_Count;
      end Visit_Part;

   begin
      if Condition_Node /= null then
         Condition_Node.Visit(Self'ACCESS, Depth+1);
      end if;

      Main_Buffer := Self.Buffer;

      Visit_Part(Then_Part_Node, Then_Buffer, Skip_Then_Count);
      Visit_Part(Else_Part_Node, Else_Buffer, Skip_Else_Count);

      Self.Buffer := Main_Buffer;

      -- BRANCH_REL branches when Src_Reg is True so we fall through to the else part.
      -- Note the "+ 1"; skip the added JUMP_REL instruction to jump over the then part.
      --
      Self.Buffer.Put_Instruction("  BRANCH_REL " & Src_Reg & Natural'IMAGE(Skip_Else_Count + 1));
      Self.Buffer.Append(Else_Buffer);
      Self.Buffer.Put_Instruction("  JUMP_REL " & Natural'IMAGE(Skip_Then_Count)); -- Jump over then part
      Self.Buffer.Append(Then_Buffer);

      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_If, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;


   ----------------------------------------------------------------------------
   procedure Visit_Statement_Assert
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

       Condition : Node_Pointer := Target.Get_Association(My_Condition);
       Condition_Name : constant String := Resolve_Name(Condition.all);
       Condition_Reg : constant String := Resolve_Register(Condition.all);
       Condition_Kind : kv.avm.Registers.Data_Kind := Condition.Get_Kind;

   begin
      Explore(Self, Target, Depth, False);
      if Condition_Kind = Bit_Or_Boolean then
         Self.Buffer.Put_Instruction("  ASSERT " & Condition_Reg & " # " & Condition_Name);
      else
         --!@#$ warn the user
         Self.Buffer.Put_Instruction("  TERMINATE_PROGRAM # ill-formed assertion");
      end if;
      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_Assert, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Send
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      S : access Statement_Send_Class := Statement_Send_Class(Target)'ACCESS;

   begin
      Explore(Self, Target, Depth, False);
      case S.Get_Destination is
         when Actor =>
            Self.Buffer.Put_Instruction("  ACTOR_SEND " &
               Resolve_Register(Target.Get_Association(My_Arguments).all) & " => " &
               Resolve_Register(Target.Get_Association(My_Destination).all) & " . " &
               Resolve_Register(Target.Get_Association(My_Name).all)); -- 5a2
         when kv.avm.vole_tree.Self =>
            Self.Buffer.Put_Instruction("  SELF_SEND " &
               Resolve_Register(Target.Get_Association(My_Arguments).all) & " => " &
               Resolve_Register(Target.Get_Association(My_Name).all)); -- 5b2
         when Super =>
            --!@#$ warn the user
            Self.Buffer.Put_Instruction("  TERMINATE_PROGRAM # unimplemented super send");
      end case;
      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_Send, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_While
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      use kv.avm.Code_Buffers;

      Condition_Node : Node_Pointer := Target.Get_Association(My_Condition);
      Loop_Part_Node : Node_Pointer := Target.Get_Association(My_Loop_Part);

--      Src_Reg : constant String := Resolve_Register(Condition_Node.all);
--      Condition_Kind : kv.avm.Registers.Data_Kind := Condition_Node.Get_Kind;
      Skip_Loop_Count : Natural := 0;
      Skip_Cond_Count : Natural := 0;
      Jump_Back_Count : Natural;

      Main_Buffer : kv.avm.Code_Buffers.Buffer_Type;
      Cond_Buffer : kv.avm.Code_Buffers.Buffer_Type;
      Loop_Buffer : kv.avm.Code_Buffers.Buffer_Type;

      procedure Visit_Part(Part_Node : in Node_Pointer; Buffer : in out Buffer_Type; Count : out Natural) is
      begin
         Self.Buffer := Buffer;
         if Part_Node /= null then
            Part_Node.Visit(Self'ACCESS, Depth + 1);
         end if;
         Buffer := Self.Buffer;
         Count := Buffer.Code_Count;
      end Visit_Part;

   begin
--!@#$ move this kind of checking to rewrite
--      if Condition_Kind /= Bit_Or_Boolean then
--         raise Non_Boolean_Condition_Error;
--      end if;

      Main_Buffer := Self.Buffer;

      if Condition_Node /= null then
         Visit_Part(Condition_Node, Cond_Buffer, Skip_Cond_Count);
      end if;
      Visit_Part(Loop_Part_Node, Loop_Buffer, Skip_Loop_Count);

      Jump_Back_Count := Skip_Loop_Count + 1;

      Self.Buffer := Main_Buffer;

      if Condition_Node /= null then
         Self.Buffer.Append(Cond_Buffer);
         Self.Buffer.Put_Instruction("  BRANCH_NEQ " & Resolve_Register(Condition_Node.all) & Natural'IMAGE(Skip_Loop_Count + 1));
         Jump_Back_Count := Jump_Back_Count + Skip_Cond_Count + 1;
      end if;
      Self.Buffer.Append(Loop_Buffer);
      Self.Buffer.Put_Instruction("  JUMP_REL " & Natural'IMAGE(-Jump_Back_Count));

      Explore_Next(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Statement_While, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;



   ----------------------------------------------------------------------------
   procedure Visit_Program
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Self.Buffer.Put_Comment("################################################################################");
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Program, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Unimp
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Terminate_Parsing =>
         raise; -- Quietly leave
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Code_Gen.Visit_Unimp, line " & Positive'IMAGE(Target.Get_Line));
         raise Terminate_Parsing;
   end;

   ----------------------------------------------------------------------------
   procedure Print
      (Self : in     Code_Generator_Class) is
   begin
      Self.Buffer.Print;
   end Print;

   ----------------------------------------------------------------------------
   procedure Process_Lines
      (Self      : in     Code_Generator_Class;
       Processor : access procedure(Line : String)) is
   begin
--      Self.Buffer.Process_Lines(Processor.all'ACCESS);
      Self.Buffer.Process_Lines(Processor);
   end Process_Lines;

end kv.avm.Code_Generator;
