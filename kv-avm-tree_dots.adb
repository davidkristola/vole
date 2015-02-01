with Ada.Text_IO; use Ada.Text_IO;
with kv.avm.Instructions;
with kv.avm.Registers; use kv.avm.Registers;

package body kv.avm.Tree_Dots is

   ----------------------------------------------------------------------------
   procedure Init
      (Self      : in out Grapher_Class;
       File_Name : in     String) is
   begin
      Create(Self.File, Out_File, File_Name);
   end Init;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self      : in out Grapher_Class) is
   begin
      Close(Self.File);
   end Finalize;


   ----------------------------------------------------------------------------
   function Indent(Depth : Natural) return String is
      Pad : constant String(1..200) := (others => ' ');
   begin
      return Pad(1..((Depth+1)*2));
   end Indent;

   ----------------------------------------------------------------------------
   procedure Explore
      (Self   : in out Grapher_Class;
       Target : in     Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      Child : Node_Pointer;
      Next : Node_Pointer;
      PID  : constant Positive := Target.Get_ID;
      PIDI : constant String := Positive'IMAGE(PID);

   begin
      for Association in Child_Association_Type loop
         Child := Target.Get_Association(Association);
         if Child /= null then
            Put_Line(Self.File, "  " & PIDI & " -> " & Positive'IMAGE(Child.Get_ID) & " [label=""" & Child_Association_Type'IMAGE(Association) & """];");
            Child.Visit(Self'ACCESS, Depth+1);
         end if;
      end loop;
      Next := Target.Get_Association(My_Next);
      if Next /= null then
         Put_Line(Self.File, "  {rank=same;" & PIDI & ";" & Positive'IMAGE(Next.Get_ID) & "}");
         Put_Line(Self.File, "  " & PIDI & " -> " & Positive'IMAGE(Next.Get_ID) & " [label=""next"",style=bold];");
         Next.Visit(Self'ACCESS, Depth);
      end if;
   end Explore;





   ----------------------------------------------------------------------------
   procedure Visit_Id
      (Self   : in out Grapher_Class;
       Target : in out Id_Node_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""" & Target.Get_Name & """,style=dotted];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Actor_Definition
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      Super : Node_Pointer;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""Actor " & Target.Get_Name & """];");
      Super := Actor_Definition_Class(Target).Get_Super_Class;
      if Super /= null then
         Put_Line(Self.File, "  " & Positive'IMAGE(Super.Get_ID) & " [label=""" & Super.Get_Name & """,shape=egg];");
         Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " -> " & Positive'IMAGE(Super.Get_ID) & " [label=""superclass"",style=dashed];");
      end if;
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Attribute_Definition
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""attribute " & Target.Get_Name & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Message_Definition
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""message " & Target.Get_Name & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Kind_Node
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      N : constant Kind_Node_Class := Kind_Node_Class(Target);
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""" & kv.avm.Registers.Data_Kind'IMAGE(N.Get_Kind) & """,shape=box];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Argument
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""arg " & Target.Get_Name & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Constructor_Send_Node
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""c-send""];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_List
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""exp""];");
      Explore(Self, Target, Depth);
   end;


   type Op_Lookup_Type is array (kv.avm.Instructions.Operation_Type) of access constant String;

   Unknown : aliased constant String := "Unk";
   Plus : aliased constant String := "+";
   Minus : aliased constant String := "-";
   Star : aliased constant String := "*";
   Slash : aliased constant String := "/";
   Mod_Word : aliased constant String := "mod";
   Not_Word : aliased constant String := "not";
   Equals : aliased constant String := "=";
   Not_Eq : aliased constant String := "!=";
   Less : aliased constant String := "<";
   Less_Eq : aliased constant String := "<=";
   More : aliased constant String := ">";
   More_Eq : aliased constant String := ">=";
   Shift_Up : aliased constant String := "<<";
   Shift_Down : aliased constant String := ">>";
   Or_Word : aliased constant String := "or";
   Xor_Word : aliased constant String := "xor";
   And_Word : aliased constant String := "and";
   Exp_Op : aliased constant String := "**";

   Op_Lookup : constant Op_Lookup_Type :=
      (kv.avm.Instructions.Add => Plus'ACCESS,
       kv.avm.Instructions.Op_Pos => Plus'ACCESS,
       kv.avm.Instructions.Mul => Star'ACCESS,
       kv.avm.Instructions.Sub => Minus'ACCESS,
       kv.avm.Instructions.Negate => Not_Word'ACCESS,
       kv.avm.Instructions.Op_Neg => Minus'ACCESS,
       kv.avm.Instructions.Div => Slash'ACCESS,
       kv.avm.Instructions.Modulo => Mod_Word'ACCESS,
       kv.avm.Instructions.Exp => Exp_Op'ACCESS,
       kv.avm.Instructions.Eq => Equals'ACCESS,
       kv.avm.Instructions.Neq => Not_Eq'ACCESS,
       kv.avm.Instructions.L_t => Less'ACCESS,
       kv.avm.Instructions.Lte => Less_Eq'ACCESS,
       kv.avm.Instructions.G_t => More'ACCESS,
       kv.avm.Instructions.Gte => More_Eq'ACCESS,
       kv.avm.Instructions.Shift_Left => Shift_Up'ACCESS,
       kv.avm.Instructions.Shift_Right => Shift_Down'ACCESS,
       kv.avm.Instructions.B_And => And_Word'ACCESS,
       kv.avm.Instructions.B_Or => Or_Word'ACCESS,
       kv.avm.Instructions.B_Xor => Xor_Word'ACCESS,
       others => Unknown'ACCESS);

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Op
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      N : constant Expression_Op_Class := Expression_Op_Class(Target);
   begin
      --Put_Line(N.Get_Temp_Name);
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""" & Op_Lookup(N.Get_Op).all & " " & N.Get_Temp_Name & """];");
      Explore(Self, Target, Depth);
   end;

   Is_Local : aliased constant String := "var";
   Is_Attr  : aliased constant String := "attribute";
   Var_Lookup : constant array (Boolean) of access constant String := (True => Is_Attr'ACCESS, False => Is_Local'ACCESS);

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Var
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      N : constant Expression_Var_Class := Expression_Var_Class(Target);
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""" & Var_Lookup(N.Get_Is_Self).all & " " & Target.Get_Name & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Literal
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      N : constant Expression_Literal_Class := Expression_Literal_Class(Target);
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""literal = " & N.Get_Value & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Send
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      N : access Expression_Call_Class := Expression_Call_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""" & N.Get_Desc & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Fold
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""FOLD""];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_List
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""list " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Assign
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""assign " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Var_Def
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""def " & Target.Get_Name & " " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Emit
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""emit " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Return
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""return " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_If
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""if " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Assert
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""assert " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Send
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""send " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_While
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""while " & S.Get_Block_Name  & """];");
      Explore(Self, Target, Depth);
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Program
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, "digraph program {");
      Put_Line(Self.File, "  " & Positive'IMAGE(Target.Get_ID) & " [label=""Program""];");
      Explore(Self, Target, Depth);
      Put_Line(Self.File, "}");
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Unimp
      (Self   : in out Grapher_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Put_Line(Self.File, Indent(Depth)&"Unimp"&Positive'IMAGE(Target.Get_ID));
      Explore(Self, Target, Depth);
   end;

end kv.avm.Tree_Dots;
