with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with kv.avm.Log; use kv.avm.Log;
with kv.avm.Symbol_Tables;
with kv.avm.Instructions;
with kv.avm.Registers; use kv.avm.Registers;

package body kv.avm.Tree_Rewrite is

   ----------------------------------------------------------------------------
   procedure Init
      (Self : in out Rewriter_Class) is
   begin
      null;
   end Init;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self      : in out Rewriter_Class) is
   begin
      null;
   end Finalize;

   ----------------------------------------------------------------------------
   function Next_Temp(Self : access Rewriter_Class; Src_Line : Positive) return String is
   begin
      Self.Temp := Self.Temp + 1;
      declare
         Img : String := Natural'IMAGE(Self.Temp);
         Num : String := Positive'IMAGE(Src_Line);
      begin
         Img(1) := '#'; -- This replaces the leading space and creates a unique name that is not "legal" in Vole.
         Num(1) := '-';
         return Img&"-line"&Num;
      end;
   end Next_Temp;

   ----------------------------------------------------------------------------
   function Next_Block(Self : access Rewriter_Class) return String is
   begin
      Self.Block := Self.Block + 1;
      declare
         Img : String := Natural'IMAGE(Self.Block);
      begin
         Img(1) := ':'; -- This replaces the leading space and creates a unique name that is not "legal" in Vole.
         return Img;
      end;
   end Next_Block;


   ----------------------------------------------------------------------------
   procedure Explore
      (Self   : in out Rewriter_Class;
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
            Child.Set(My_Parent, Target.Get_Association(My_Self)); -- Link parents
            Child.Visit(Self'ACCESS, Depth+1);
         end if;
      end loop;
      Next := Target.Get_Association(My_Next);
      if Next /= null then
         Next.Set(My_Parent, Target.Get_Association(My_Parent)); -- Link parents
         Next.Visit(Self'ACCESS, Depth);
      end if;
   end Explore;


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

   begin
      Put_Line("   Symbol: " & Name & ", index " & Natural'IMAGE(Indx) & " of " & kv.avm.Registers.Data_Kind'IMAGE(Kind) & Default);
   end Print_Symbol;

   ----------------------------------------------------------------------------
   procedure Add_To_Message_Symbol_Table
      (Target : in out Node_Base_Class'CLASS;
       Name   : in     String;
       Local  : in     Boolean) is
      My_Message : Node_Pointer;
      Table : access kv.avm.Symbol_Tables.Symbol_Table;
      Table_Flavor : constant array (Boolean) of String(1..6) := (false => "inputs", true => "locals");
      Kind : kv.avm.Registers.Data_Kind := Target.Get_Kind;
      Kind_Node : Node_Pointer;
   begin
      My_Message := Message_Of(Target);
      Table := Message_Definition_Class(My_Message.all).Get_Symbol_Table(Local);
      if Kind = Unset then -- Try to figre it out
         Kind_Node := Target.Get_Association(My_Kind);
         if Kind_Node /= null then
            Kind := Kind_Node_Class(Kind_Node.all).Get_Kind;
         end if;
      end if;
      Table.Add(Name, Kind);
      --Put_Line("Just added '" & Name & "' to the message symbol table " & Table_Flavor(Local));
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Add_To_Message_Symbol_Table('" & Name & "', " & Boolean'IMAGE(Local) & "), line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end Add_To_Message_Symbol_Table;


   ----------------------------------------------------------------------------
   function Actor_Symbol_Already_Defined
      (Target  : in     Node_Base_Class'CLASS;
       Name    : in     String;
       Attr    : in     Boolean) return Boolean is

      My_Actor : Node_Pointer;
      Table : access kv.avm.Symbol_Tables.Symbol_Table;

   begin
      My_Actor := Actor_Of(Target);
      Table := Actor_Definition_Class(My_Actor.all).Get_Symbol_Table(Attr);
      return Table.Has(Name);
   end Actor_Symbol_Already_Defined;


   ----------------------------------------------------------------------------
   procedure Add_To_Actor_Symbol_Table
      (Target  : in out Node_Base_Class'CLASS;
       Name    : in     String;
       Attr    : in     Boolean;
       Init    : in     String := "";
       As_Kind : in     kv.avm.Registers.Data_Kind := kv.avm.Registers.Unset) is

      My_Actor : Node_Pointer;
      Table : access kv.avm.Symbol_Tables.Symbol_Table;
      Table_Flavor : constant array (Boolean) of String(1..9) := (false => "constant ", true => "attribute");
      Kind : kv.avm.Registers.Data_Kind := Target.Get_Kind;
      Kind_Node : Node_Pointer;

   begin
      if As_Kind /= Unset then
         Kind := As_Kind;
      end if;
      My_Actor := Actor_Of(Target);
      Table := Actor_Definition_Class(My_Actor.all).Get_Symbol_Table(Attr);
      if Kind = Unset then -- Try to figre it out
         Kind_Node := Target.Get_Association(My_Kind);
         if Kind_Node /= null then
            Kind := Kind_Node_Class(Kind_Node.all).Get_Kind;
         end if;
      end if;
      if not Table.Has(Name) then
         Table.Add(Name, Kind, Init);
      end if;
      --Put_Line("Just added '" & Name & "' to the actor symbol table " & Table_Flavor(Attr));
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Add_To_Actor_Symbol_Table('" & Name & "', " & Boolean'IMAGE(Attr) & ", '" & Init & "', " &
            kv.avm.Registers.Data_Kind'IMAGE(As_Kind) & "), line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end Add_To_Actor_Symbol_Table;





   ----------------------------------------------------------------------------
   procedure Visit_Id
      (Self   : in out Rewriter_Class;
       Target : in out Id_Node_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Id, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Actor_Definition
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      Table : access kv.avm.Symbol_Tables.Symbol_Table;
      Actor_Node : Actor_Definition_Class;
      Super_Name_Node : Node_Pointer;
      Super_Node : Node_Pointer;
      Superclass : Actor_Definition_Pointer;
      Super_Symbols : kv.avm.Symbol_Tables.Symbol_Table_Access;
      Extend_Symbols : kv.avm.Symbol_Tables.Symbol_Table_Access;
   begin
      Explore(Self, Target, Depth);
      Actor_Node := Actor_Definition_Class(Target);
      Super_Name_Node := Actor_Node.Get_Super_Class;
      -- See if we are a subclass
      if Super_Name_Node /= null then
         Put_Line("Actor " & Actor_Node.Get_Name & " is a subclass of " & Super_Name_Node.Get_Name);
         -- Find the superclass name
         Super_Node := Find_Actor(Super_Name_Node.Get_Name);
         Superclass := Actor_Definition_Pointer(Super_Node);
         for Is_Const in Boolean loop
            Super_Symbols := kv.avm.Symbol_Tables.Symbol_Table_Access(Superclass.Get_Symbol_Table(Is_Const));
            Extend_Symbols := kv.avm.Symbol_Tables.Symbol_Table_Access(Actor_Node.Get_Symbol_Table(Is_Const));
            Extend_Symbols.Link_Superclass_Table(Super_Symbols);
            Extend_Symbols.Set_All_Indexes(Super_Symbols.Count);
         end loop;
       end if;



      Put_Line("Actor " & Actor_Node.Get_Name & " attributes:");
      Table := Actor_Node.Get_Symbol_Table(VARIABLE_SYMBOLS);
      Table.For_Each(Print_Symbol'ACCESS);
      Put_Line("Actor " & Actor_Node.Get_Name & " constant:");
      Table := Actor_Node.Get_Symbol_Table(CONSTANT_SYMBOLS);
      Table.For_Each(Print_Symbol'ACCESS);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Actor_Definition, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Attribute_Definition
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      --Put_Line("Just added attribute '" & Target.Get_Name & "' to the actor symbol table.");
      Add_To_Actor_Symbol_Table(Target, Target.Get_Name, Attr => True);

      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Attribute_Definition, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Message_Definition
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      Table : access kv.avm.Symbol_Tables.Symbol_Table;
   begin
      Explore(Self, Target, Depth);
      Put_Line("Message " & Target.Get_Name & " inputs:");
      Table := Message_Definition_Class(Target).Get_Symbol_Table(False);
      Table.For_Each(Print_Symbol'ACCESS);
      Put_Line("Message " & Target.Get_Name & " locals:");
      Table := Message_Definition_Class(Target).Get_Symbol_Table(True);
      Table.For_Each(Print_Symbol'ACCESS);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Message_Definition, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Kind_Node
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Kind_Node, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Argument
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      Association : constant Association_Type := Association_Of(Target);
   begin
      Add_To_Message_Symbol_Table(Target, Target.Get_Name, Local => (Association = My_Outputs));
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Argument, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   function Get_Argument_List(Node : Node_Pointer) return String is
   begin
      if Node = null then
         return "";
      end if;
      return " " & Resolve_Register(Node.all) & Get_Argument_List(Node.Get_Association(My_Next));
   end Get_Argument_List;

   ----------------------------------------------------------------------------
   procedure Add_Folded_Arguments_Node
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS) is

      Arguments : Node_Pointer := Target.Get_Association(My_Arguments);
      Argument_Init : constant String := Get_Argument_List(Arguments);
      Fold : Node_Pointer;
      Temp_Tuple_Name : constant String := Self.Next_Temp(Target.Get_Line);
      Tuple_Map_Name : constant String := Self.Next_Temp(Target.Get_Line);

   begin
      Build_Fold(Fold, Target.Get_Line, Arguments);
      declare
         E : access Expression_Fold_Class := Expression_Fold_Class(Fold.all)'ACCESS;
      begin
         E.Set_Temp_Name(Temp_Tuple_Name);
         E.Set_Tuple_Map_Name(Tuple_Map_Name);
         E.Set_Tuple_Map_Init(Argument_Init);
      end;
      Add_To_Message_Symbol_Table(Target, Temp_Tuple_Name, Local => True);
      Add_To_Actor_Symbol_Table(Target, Tuple_Map_Name, Attr => False, Init => "[" & Argument_Init & "]", As_Kind => kv.avm.Registers.Tuple_Map);
      Fold.Set(My_Parent, Target.Get_Association(My_Self));
      if Arguments /= null then
         Arguments.Set(My_Parent, Fold);
      end if;
      Target.Set(My_Arguments, Fold);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Add_Folded_Arguments_Node, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end Add_Folded_Arguments_Node;

   ----------------------------------------------------------------------------
   procedure Visit_Constructor_Send_Node
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      My_Actor : Node_Pointer;
      Table : access kv.avm.Symbol_Tables.Symbol_Table;

   begin
      Explore(Self, Target, Depth);

      My_Actor := Actor_Of(Target);
      Table := Actor_Definition_Class(My_Actor.all).Get_Symbol_Table(False);

      declare
         Name : constant String := Target.Get_Association(My_Destination).Get_Name;
      begin
         Add_To_Actor_Symbol_Table(Target, Name, False, '"' & Name & '"');
         Table.Set_Kind(Name, Actor_Definition);
      end;
      Add_Folded_Arguments_Node(Self, Target);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Constructor_Send_Node, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_List
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      E : access Expression_List_Class := Expression_List_Class(Target)'ACCESS;
      Name : constant String := Self.Next_Temp(Target.Get_Line);
   begin
      E.Set_Temp_Name(Name);
      E.Set_Kind(Tuple);
      Add_To_Message_Symbol_Table(Target, Name, Local => True);
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Expression_List, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;


   type Operator_Processing_Type is (Int_Float_Bool_To_Same, Bool_To_Same, Bool_To_Bool, Int_Float_Bool_To_Bool, Inf_Float_Bool_Actor_To_Bool, Float_To_Same, Int_To_Same);
   Operator_Processing : constant array (kv.avm.Instructions.Operation_Type) of Operator_Processing_Type :=
      (kv.avm.Instructions.Add => Int_Float_Bool_To_Same,
       kv.avm.Instructions.Sub => Int_Float_Bool_To_Same,
       kv.avm.Instructions.Mul => Int_Float_Bool_To_Same,
       kv.avm.Instructions.Div => Int_Float_Bool_To_Same,
       kv.avm.Instructions.Modulo => Int_Float_Bool_To_Same,
       kv.avm.Instructions.Exp    => Float_To_Same,
       kv.avm.Instructions.Negate => Bool_To_Bool,
       kv.avm.Instructions.Op_Pos => Int_Float_Bool_To_Same,
       kv.avm.Instructions.Op_Neg => Int_Float_Bool_To_Same,
       kv.avm.Instructions.Eq  => Inf_Float_Bool_Actor_To_Bool,
       kv.avm.Instructions.Neq => Inf_Float_Bool_Actor_To_Bool,
       kv.avm.Instructions.L_t  => Int_Float_Bool_To_Bool,
       kv.avm.Instructions.Lte => Int_Float_Bool_To_Bool,
       kv.avm.Instructions.G_t  => Int_Float_Bool_To_Bool,
       kv.avm.Instructions.Gte => Int_Float_Bool_To_Bool,
       kv.avm.Instructions.Shift_Left  => Int_To_Same,
       kv.avm.Instructions.Shift_Right => Int_To_Same,
       kv.avm.Instructions.B_And => Bool_To_Bool,
       kv.avm.Instructions.B_Or  => Bool_To_Bool,
       kv.avm.Instructions.B_Xor => Bool_To_Bool);

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Op
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      E : access Expression_Op_Class := Expression_Op_Class(Target)'ACCESS;
      Name : constant String := Self.Next_Temp(Target.Get_Line);
      Operator : kv.avm.Instructions.Operation_Type;
      Resulting_Kind : kv.avm.Registers.Data_Kind;
   begin
      E.Set_Temp_Name(Name);
      Explore(Self, Target, Depth);

      Operator := E.Get_Op;
      case Operator_Processing(Operator) is
         when Bool_To_Bool | Int_Float_Bool_To_Bool | Inf_Float_Bool_Actor_To_Bool =>
            Resulting_Kind := Bit_Or_Boolean;
         when others =>
            Resulting_Kind := Target.Get_Association(My_Right).Get_Kind;
      end case;

      --Put_Line(">> Setting " & Name & "'s kind to " & kv.avm.Registers.Data_Kind'IMAGE(Resulting_Kind));
      Target.Set_Kind(Resulting_Kind);

      Add_To_Message_Symbol_Table(Target, Name, Local => True);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Expression_Op, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;



   ----------------------------------------------------------------------------
   procedure Visit_Expression_Var
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      E : access Expression_Var_Class := Expression_Var_Class(Target)'ACCESS;
      Table : access kv.avm.Symbol_Tables.Symbol_Table;
      Parent : Node_Pointer;
   begin
      -- Look up the variable and copy its kind.
      --
      if E.Get_Is_Self then
         -- look up in the actor's symbol tables
         Parent := Actor_Of(Target);
         Table := Actor_Definition_Class(Parent.all).Get_Symbol_Table(True);
         if Table.Has(Target.Get_Name) then
            Target.Set_Kind(Table.Get_Kind(Target.Get_Name));
         else
            Table := Actor_Definition_Class(Parent.all).Get_Symbol_Table(False);
            if Table.Has(Target.Get_Name) then
               Target.Set_Kind(Table.Get_Kind(Target.Get_Name));
            else
               Put_Line("*** Visit_Expression_Var could not set kind for attribute " & Target.Get_Name);
            end if;
         end if;
      else
         -- look up in the message's symbol tables
         Parent := Message_Of(Target);
         Table := Message_Definition_Class(Parent.all).Get_Symbol_Table(True);
         if Table.Has(Target.Get_Name) then
            Target.Set_Kind(Table.Get_Kind(Target.Get_Name));
         else
            Table := Message_Definition_Class(Parent.all).Get_Symbol_Table(False);
            if Table.Has(Target.Get_Name) then
               Target.Set_Kind(Table.Get_Kind(Target.Get_Name));
            else
               Put_Line("*** Visit_Expression_Var could not set kind for local " & Target.Get_Name);
            end if;
         end if;
      end if;
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Expression_Var, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Literal
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      E : access Expression_Literal_Class := Expression_Literal_Class(Target)'ACCESS;
      Name : constant String := Self.Next_Temp(Target.Get_Line);
   begin
      E.Set_Temp_Name(Name);
      Explore(Self, Target, Depth);
      Add_To_Actor_Symbol_Table(Target, Name, False, E.Get_Value);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Expression_Literal, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Send
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is

      E : access Expression_Call_Class := Expression_Call_Class(Target)'ACCESS;
      Name : constant String := Self.Next_Temp(Target.Get_Line);

   begin
      E.Set_Temp_Name(Name);
      E.Set_Kind(Tuple);
      Add_To_Message_Symbol_Table(Target, Name, Local => True);
      Explore(Self, Target, Depth);

      declare
         Name : constant String := Target.Get_Association(My_Name).Get_Name;
      begin
         if not Actor_Symbol_Already_Defined(Target, Name, False) then
            Add_To_Actor_Symbol_Table(Target, Name, False, '"' & Name & '"', Message_Definition);
         end if;
      end;
      Add_Folded_Arguments_Node(Self, Target);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Expression_Send, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Expression_Fold
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Expression_Fold, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_List
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      S.Set_Block_Name(Self.Next_Block);
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_List, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Assign
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      S.Set_Block_Name(Self.Next_Block);
      Explore(Self, Target, Depth);
      --!@#$ check kind
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_Assign, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Var_Def
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      S.Set_Block_Name(Self.Next_Block);
      Add_To_Message_Symbol_Table(Target, Target.Get_Name, Local => True);
      --!@#$ if there is an init, strip it off and make a set node
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_Var_Def, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Emit
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      S.Set_Block_Name(Self.Next_Block);
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_Emit, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_Return
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      Child : Node_Pointer;
      E : access Expression_Call_Class;
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      --!@#$ this is a place to insert a FOLD statement if the number of arguments being retunr > 1
      S.Set_Block_Name(Self.Next_Block);
      -- Check if this is a tail call.
      Child := Target.Get_Association(MY_VALUE);
      if Child /= null and then Child.all in Expression_Call_Class'CLASS then
         E := Expression_Call_Class(Child.all)'ACCESS;
         E.Set_Tail(True);
      end if;
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_Return, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Statement_If
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      S.Set_Block_Name(Self.Next_Block);
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_If, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;


   ----------------------------------------------------------------------------
   procedure Visit_Statement_Assert
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      S.Set_Block_Name(Self.Next_Block);
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_Assert, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;


   ----------------------------------------------------------------------------
   procedure Visit_Statement_Send
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      S.Set_Block_Name(Self.Next_Block);
      Explore(Self, Target, Depth);

      declare
         Name : constant String := Target.Get_Association(My_Name).Get_Name;
      begin
         if not Actor_Symbol_Already_Defined(Target, Name, False) then
            Add_To_Actor_Symbol_Table(Target, Name, False, '"' & Name & '"', Message_Definition);
         end if;
      end;
      Add_Folded_Arguments_Node(Self, Target);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_Send, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;


   ----------------------------------------------------------------------------
   procedure Visit_Statement_While
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
      S : access Statement_List_Class := Statement_List_Class(Target)'ACCESS;
   begin
      S.Set_Block_Name(Self.Next_Block);
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Statement_While, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;



   ----------------------------------------------------------------------------
   procedure Visit_Program
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Program, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

   ----------------------------------------------------------------------------
   procedure Visit_Unimp
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural) is
   begin
      Explore(Self, Target, Depth);
   exception
      when Error: others =>
         Put_Error("EXCEPTION: " & Exception_Information(Error));
         Put_Error("Rewrite.Visit_Unimp, line " & Positive'IMAGE(Target.Get_Line));
         raise;
   end;

end kv.avm.Tree_Rewrite;
