with Ada.Text_IO;
with kv.avm.Log; use kv.avm.Log;
with Ada.Exceptions; use Ada.Exceptions;

with kv.avm.references; use kv.avm.references;
with kv.avm.Tree_Visitors;
with kv.avm.Symbol_Tables;

package body kv.avm.vole_tree is

   function "+"(S : String) return String_Type renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+"(U : String_Type) return String renames Ada.Strings.Unbounded.To_String;

   ----------------------------------------------------------------------------
   protected type Unique_Id_Type is
      procedure Next(ID :    out Positive);
   private 
      Count : Natural := 0;
   end Unique_Id_Type;
   protected body Unique_Id_Type is
      procedure Next(ID :    out Positive) is
      begin
         Count := Count + 1;
         ID := Count;
      end Next;
   end Unique_Id_Type;

   UID : Unique_Id_Type;


   ----------------------------------------------------------------------------
   function Actor_Of(Target : Node_Base_Class'CLASS) return Node_Pointer is
      Parent : Node_Pointer;
   begin
      Parent := Target.Get_Association(My_Parent);
      if Parent /= null then
         if Parent.all in Actor_Definition_Class'CLASS then
            return Parent;
         else
            return Actor_Of(Parent.all);
         end if;
      else
         raise Missing_Parent_Error;
      end if;
   end Actor_Of;


   ----------------------------------------------------------------------------
   function Message_Of(Target : Node_Base_Class'CLASS) return Node_Pointer is
      Parent : Node_Pointer;
   begin
      Parent := Target.Get_Association(My_Parent);
      if Parent /= null then
         if Parent.all in Message_Definition_Class'CLASS then
            return Parent;
         else
            return Message_Of(Parent.all);
         end if;
      else
         raise Missing_Parent_Error;
      end if;
   end Message_Of;


   ----------------------------------------------------------------------------
   function Association_Of(Target : Node_Base_Class'CLASS) return Association_Type is
      Parent : Node_Pointer;
      Me : constant Node_Pointer := Target.Get_Association(My_Self);
      Previous : Node_Pointer;
   begin
      --Put_Line("Looking for the association to "&Me.Get_Name);
      Parent := Target.Get_Association(My_Parent);
      if Parent /= null then
         for Association in Child_Association_Type loop
            if Parent.Get_Association(Association) = Me then
               return Association;
            end if;
         end loop;
         -- We have to follow the chain back because a parent can only point to one child in a list
         Previous := Target.Get_Association(My_Previous);
         if Previous /= Null then
            return Association_Of(Previous.all);
         else
            raise Association_Error;
         end if;
      else
         raise Missing_Parent_Error;
      end if;
   end Association_Of;


   ----------------------------------------------------------------------------
   function Find_Actor(Target : String) return Node_Pointer is
      Program : Program_Pointer;
      Current : Node_Pointer;
   begin
      Program := Get_Program;
      Current := Program.Get_Association(My_Actors);
      while Current /= null and then Resolve_Name(Current.all) /= Target loop
         Current := Current.Get_Association(My_Next);
      end loop;
      return Current;
   end Find_Actor;



   ----------------------------------------------------------------------------
   function Resolve_Name(Target : Node_Base_Class'CLASS; Raise_Errors : Boolean := False) return String is
      First_Attempt : constant String := Target.Get_Name;
   begin
      if First_Attempt = "Not applicable" then
         if Target in Expression_List_Class'CLASS then
            return Expression_List_Class(Target).Get_Temp_Name;
         else
            if Raise_Errors then
               raise Unresolveable_Name;
            end if;
            return "Could not Resolve_Name";
         end if;
      else
         return First_Attempt;
      end if;
   end Resolve_Name;


   ----------------------------------------------------------------------------
   function Resolve_Ref_Name(Target : Node_Base_Class'CLASS; Ref_Name : String; Raise_Errors : Boolean := False) return String is

      Table : access kv.avm.Symbol_Tables.Symbol_Table;
      My_Message : Node_Pointer;
      My_Actor : Node_Pointer;

      use kv.avm.Instructions;
      use kv.avm.References;

   begin
      -- Check to see if this is a "self.x" variable and skip the message symbol table checks
      if not (Target in Expression_Var_Class'CLASS and then Expression_Var_Class(Target).Get_Is_Self) then
         My_Message := Message_Of(Target);
         Table := Message_Definition_Class(My_Message.all).Get_Symbol_Table(VARIABLE_SYMBOLS);
         if Table.Has(Ref_Name) then
            return Make_Register_Name(Table.Get_Index(Ref_Name), Local);
         end if;
         Table := Message_Definition_Class(My_Message.all).Get_Symbol_Table(CONSTANT_SYMBOLS);
         if Table.Has(Ref_Name) then
            return Make_Register_Name(Table.Get_Index(Ref_Name), Input);
         end if;
      end if;

      My_Actor := Actor_Of(Target);
      Table := Actor_Definition_Class(My_Actor.all).Get_Symbol_Table(VARIABLE_SYMBOLS);
      if Table.Has(Ref_Name) then
         return Make_Register_Name(Table.Get_Index(Ref_Name), Attribute);
      end if;
      Table := Actor_Definition_Class(My_Actor.all).Get_Symbol_Table(CONSTANT_SYMBOLS);
      if Table.Has(Ref_Name) then
         return Make_Register_Name(Table.Get_Index(Ref_Name), Fixed);
      end if;
      if Raise_Errors then
         raise Variable_Undefined;
      end if;
      return "ERROR-variable-not-found:" & Ref_Name;
   end Resolve_Ref_Name;

   ----------------------------------------------------------------------------
   function Resolve_Register(Target : Node_Base_Class'CLASS; Raise_Errors : Boolean := False) return String is

      Ref_Name : constant String := Resolve_Name(Target);

   begin
      if Ref_Name = "Not applicable" then
         if Raise_Errors then
            raise Variable_Unspecified;
         end if;
         return "ERROR-no-variable-to-resolve";
      else
         return Resolve_Ref_Name(Target, Ref_Name, Raise_Errors);
      end if;
   end Resolve_Register;




   ----------------------------------------------------------------------------
   function Get_ID(Self : Node_Base_Class) return Positive is
   begin
      return Self.ID;
   end Get_ID;

   ----------------------------------------------------------------------------
   function Get_Line(Self : Node_Base_Class) return Positive is
   begin
      return Self.Line;
   end Get_Line;

   ----------------------------------------------------------------------------
   function Get_Name(Self : Node_Base_Class) return String is
   begin
      return "Not applicable";
   end Get_Name;
   function Get_Name(Self : Id_Node_Class) return String is
   begin
      return +Self.Name;
   end Get_Name;
   function Get_Name(Self : Actor_Definition_Class) return String is
   begin
      return Self.Associations(My_Name).Get_Name;
   end Get_Name;
   function Get_Name(Self : Attribute_Definition_Class) return String is
   begin
      return Self.Associations(My_Name).Get_Name;
   end Get_Name;
   function Get_Name(Self : Message_Definition_Class) return String is
   begin
      return Self.Associations(My_Name).Get_Name;
   end Get_Name;
   function Get_Name(Self : Argument_Class) return String is
   begin
      return Self.Associations(My_Name).Get_Name;
   end Get_Name;
   function Get_Name(Self : Expression_Var_Class) return String is
   begin
      return Self.Associations(My_Name).Get_Name;
   end Get_Name;
   function Get_Name(Self : Statement_Var_Def_Class) return String is
   begin
      return Self.Associations(My_Name).Get_Name;
   end Get_Name;



   ----------------------------------------------------------------------------
   function Get_Association(Self : Node_Base_Class; Association : Association_Type) return Node_Pointer is
   begin
      return Self.Associations(Association);
   end Get_Association;



   ----------------------------------------------------------------------------
   procedure Visit(Self : in out Id_Node_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Id(Self, D);
   end Visit;
   procedure Visit(Self : in out Actor_Definition_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Actor_Definition(Self, D);
   end Visit;
   procedure Visit(Self : in out Attribute_Definition_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Attribute_Definition(Self, D);
   end Visit;
   procedure Visit(Self : in out Message_Definition_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Message_Definition(Self, D);
   end Visit;
   procedure Visit(Self : in out Kind_Node_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Kind_Node(Self, D);
   end Visit;
   procedure Visit(Self : in out Argument_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Argument(Self, D);
   end Visit;
   procedure Visit(Self : in out Constructor_Send_Node_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Constructor_Send_Node(Self, D);
   end Visit;
   procedure Visit(Self : in out Expression_List_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Expression_List(Self, D);
   end Visit;
   procedure Visit(Self : in out Expression_Op_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Expression_Op(Self, D);
   end Visit;
   procedure Visit(Self : in out Expression_Var_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Expression_Var(Self, D);
   end Visit;
   procedure Visit(Self : in out Expression_Literal_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Expression_Literal(Self, D);
   end Visit;
   procedure Visit(Self : in out Expression_Call_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Expression_Send(Self, D);
   end Visit;
   procedure Visit(Self : in out Expression_Fold_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Expression_Fold(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_List_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_List(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_Assign_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_Assign(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_Var_Def_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_Var_Def(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_Emit_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_Emit(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_Return_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_Return(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_If_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_If(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_Assert_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_Assert(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_Send_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_Send(Self, D);
   end Visit;
   procedure Visit(Self : in out Statement_While_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Statement_While(Self, D);
   end Visit;
   procedure Visit(Self : in out Program_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is
   begin
      V.Visit_Program(Self, D);
   end Visit;


   ----------------------------------------------------------------------------
   function Get_Kind(Self : Node_Base_Class) return kv.avm.Registers.Data_Kind is
   begin
      if Self.Kind /= Unset then
         return Self.Kind;
      end if;
      if Self.Associations(My_Kind) /= null then
         return Self.Associations(My_Kind).Get_Kind;
      end if;
      return Unset;
   end Get_Kind;

   ----------------------------------------------------------------------------
   procedure Set_Kind(Self : in out Node_Base_Class; Kind : in kv.avm.Registers.Data_Kind) is
   begin
      Self.Kind := Kind;
   end Set_Kind;


   ----------------------------------------------------------------------------
   function Get_Op(Self : Expression_Op_Class) return kv.avm.Instructions.operation_type is
   begin
      return Self.Op;
   end Get_Op;

   ----------------------------------------------------------------------------
   function Get_Value(Self : Expression_Literal_Class) return String is
   begin
      return +Self.Value;
   end Get_Value;

   ----------------------------------------------------------------------------
   function Get_Is_Self(Self : Expression_Var_Class) return Boolean is
   begin
      return Self.Self;
   end Get_Is_Self;

   ----------------------------------------------------------------------------
   procedure Set_Temp_Name
      (Self : in out Expression_List_Class;
       Name : in     String) is
   begin
      Self.Temp_Name := +Name;
   end Set_Temp_Name;

   ----------------------------------------------------------------------------
   function Get_Temp_Name(Self : Expression_List_Class) return String is
   begin
      return +Self.Temp_Name;
   end Get_Temp_Name;

   ----------------------------------------------------------------------------
   procedure Set_Tail(Self : in out Expression_Call_Class; Tail : Boolean) is
   begin
      Self.Is_Tail := Tail;
   end Set_Tail;

   ----------------------------------------------------------------------------
   function Get_Desc(Self : Expression_Call_Class) return String is
   begin
      if Self.Is_Tail then
         if Self.Is_Call then
            if Self.Is_Gosub then
               return "tail-call-gosub";
            else
               return "tail-call-actor";
            end if;
         else
            if Self.Is_Gosub then
               return "tail-send-gosub";
            else
               return "tail-send-actor";
            end if;
         end if;
      else
         if Self.Is_Call then
            if Self.Is_Gosub then
               return "line-call-gosub";
            else
               return "line-call-actor";
            end if;
         else
            if Self.Is_Gosub then
               return "line-send-gosub";
            else
               return "line-send-actor";
            end if;
         end if;
      end if;
   end Get_Desc;


   ----------------------------------------------------------------------------
   function Is_Gosub(Self : Expression_Call_Class) return Boolean is
   begin
      return Self.Is_Gosub;
   end Is_Gosub;
   ----------------------------------------------------------------------------
   function Is_Call(Self : Expression_Call_Class) return Boolean is
   begin
      return Self.Is_Call;
   end Is_Call;
   ----------------------------------------------------------------------------
   function Is_Tail(Self : Expression_Call_Class) return Boolean is
   begin
      return Self.Is_Tail;
   end Is_Tail;


   ----------------------------------------------------------------------------
   procedure Set_Tuple_Map_Name
      (Self : in out Expression_Fold_Class;
       Name : in     String) is
   begin
      Self.Tuple_Map_Name := +Name;
   end Set_Tuple_Map_Name;

   ----------------------------------------------------------------------------
   function Get_Tuple_Map_Name(Self : Expression_Fold_Class) return String is
   begin
      return +Self.Tuple_Map_Name;
   end Get_Tuple_Map_Name;

   ----------------------------------------------------------------------------
   procedure Set_Tuple_Map_Init
      (Self : in out Expression_Fold_Class;
       Init : in     String) is
   begin
      Self.Tuple_Map_Init := +Init;
   end Set_Tuple_Map_Init;

   ----------------------------------------------------------------------------
   function Get_Tuple_Map_Init(Self : Expression_Fold_Class) return String is
   begin
      return +Self.Tuple_Map_Init;
   end Get_Tuple_Map_Init;

   ----------------------------------------------------------------------------
   procedure Set_Block_Name
      (Self : in out Statement_List_Class;
       Name : in     String) is
   begin
      Self.Block_Name := +Name;
   end Set_Block_Name;

   ----------------------------------------------------------------------------
   function Get_Block_Name(Self : Statement_List_Class) return String is
   begin
      return +Self.Block_Name;
   end Get_Block_Name;

   ----------------------------------------------------------------------------
   function Get_Symbol_Table(Self : Actor_Definition_Class; Attributes : Boolean) return access kv.avm.Symbol_Tables.Symbol_Table is
   begin
      if Attributes then
         return Self.Attribute_Symbols;
      else
         return Self.Constant_Symbols;
      end if;
   end Get_Symbol_Table;

   ----------------------------------------------------------------------------
   function Get_Super_Class(Self : Actor_Definition_Class) return Node_Pointer is
   begin
      return Self.Super_Class;
   end Get_Super_Class;

   ----------------------------------------------------------------------------
   function Get_Symbol_Table(Self : Message_Definition_Class; Local : Boolean) return access kv.avm.Symbol_Tables.Symbol_Table is
   begin
      if Local then
         return Self.Local_Symbols;
      else
         return Self.Input_Symbols;
      end if;
   end Get_Symbol_Table;

   ----------------------------------------------------------------------------
   function Get_Length(Self : Node_Base_Class) return Positive is
   begin
      return 1;
   end Get_Length;

   ----------------------------------------------------------------------------
   function Get_Length(Self : Node_List_Class) return Positive is
   begin
      if Self.Associations(My_Next) = null then
         return 1;
      else
         return Self.Associations(My_Next).Get_Length + 1;
      end if;
   end Get_Length;

   ----------------------------------------------------------------------------
   procedure Set_Destination
      (Self : in out Statement_Send_Class;
       Dest : in     Destination_Type) is
   begin
      Self.Destination := Dest;
   end Set_Destination;

   ----------------------------------------------------------------------------
   function Get_Destination(Self : Statement_Send_Class) return Destination_Type is
   begin
      return Self.Destination;
   end Get_Destination;








   ----------------------------------------------------------------------------
   procedure Set
      (Node        : in out Node_Base_Class;
       Association : in     Association_Type;
       Target      : in     Node_Pointer) is
   begin
      Node.Associations(Association) := Target;
   end Set;








   ----------------------------------------------------------------------------
   procedure Build_Id_Node
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     String) is
      N : Id_Node_Pointer;
   begin
      --Put_Line("Build_Id_Node, Name="&(Name));
      N := new Id_Node_Class;
      UID.Next(N.ID);
      N.Name := +Name;
      N.Line := Line;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
   end Build_Id_Node;

   ----------------------------------------------------------------------------
   procedure Build_Actor_Node
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     Node_Pointer;
       Attr : in     Node_Pointer;
       Meth : in     Node_Pointer;
       Supr : in     Node_Pointer := null) is
      N : Actor_Definition_Pointer;
   begin
      --Put_Line("Build_Actor_Node, Name="&(+Id.Name));
      N := new Actor_Definition_Class;
      UID.Next(N.ID);
      N.Line := Line;

      N.Super_Class := Supr;
      N.Attribute_Symbols := new kv.avm.Symbol_Tables.Symbol_Table;
      N.Attribute_Symbols.Initialize;
      N.Constant_Symbols := new kv.avm.Symbol_Tables.Symbol_Table;
      N.Constant_Symbols.Initialize;
      -- TODO: if Supr /= null, copy Supr's constants into our table... er... preserve indexs

      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Name, Name);
      N.Set(My_Attributes, Attr);
      N.Set(My_Methods, Meth);
   end Build_Actor_Node;


   ----------------------------------------------------------------------------
   procedure Build_Attribute
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     Node_Pointer;
       Ty_I : in     Node_Pointer) is
      N : Attribute_Definition_Pointer;
   begin
      N := new Attribute_Definition_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      --Put_Line("Build_Attribute, Name="&(+N.Name.Name));
      N.Set(My_Self, Node);
      N.Set(My_Name, Name);
      N.Set(My_Kind, Ty_I);
   end Build_Attribute;


   ----------------------------------------------------------------------------
   procedure Add_Next
      (Node : in     Node_Pointer;
       Next : in     Node_Pointer) is
   begin
      if Next /= null then
         if Node.ID = Next.ID then
            Raise_Exception(Parsing_Error'IDENTITY, "Node"&Positive'IMAGE(Node.ID)&" can't be added as its own next!");
         end if;
         Next.Associations(My_Previous) := Node;
      end if;
      Node.Associations(My_Next) := Next;
   end Add_Next;

   ----------------------------------------------------------------------------
   procedure Build_Message
      (Node : in out Node_Pointer;
       Line : in     Positive;
       pMsg : in     Boolean;
       Name : in     Node_Pointer;
       Args : in     Node_Pointer;
       Rtn  : in     Node_Pointer;
       Code : in     Node_Pointer;
       Pred : in     Node_Pointer) is
      N : Message_Definition_Pointer;
   begin
      N := new Message_Definition_Class;
      UID.Next(N.ID);
      N.Line := Line;
      N.Method := not pMsg;

      N.Input_Symbols := new kv.avm.Symbol_Tables.Symbol_Table;
      N.Input_Symbols.Initialize;
      N.Local_Symbols := new kv.avm.Symbol_Tables.Symbol_Table;
      N.Local_Symbols.Initialize;

      Node := Node_Pointer(N);
      --Put_Line("Build_Message, Name="&(+N.Name.Name));
      N.Set(My_Self, Node);
      N.Set(My_Name, Name);
      N.Set(My_Inputs, Args);
      N.Set(My_Outputs, Rtn);
      N.Set(My_Code, Code);
      N.Set(My_Condition, Pred);
   end Build_Message;

   ----------------------------------------------------------------------------
   procedure Build_Constructor
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Args : in     Node_Pointer;
       Code : in     Node_Pointer) is
      N : Message_Definition_Pointer;
      Name : Node_Pointer;
   begin
      --Put_Line("Build_Constructor");
      Build_Id_Node(Name, Line, "CONSTRUCTOR");
      N := new Message_Definition_Class;
      UID.Next(N.ID);
      N.Line := Line;
      N.Method := False;

      N.Input_Symbols := new kv.avm.Symbol_Tables.Symbol_Table;
      N.Input_Symbols.Initialize;
      N.Local_Symbols := new kv.avm.Symbol_Tables.Symbol_Table;
      N.Local_Symbols.Initialize;

      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Name, Name);
      N.Set(My_Inputs, Args);
      N.Set(My_Code, Code);
   end Build_Constructor;

   ----------------------------------------------------------------------------
   procedure Build_Arg
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     Node_Pointer;
       Kind : in     Node_Pointer) is
      N : Argument_Pointer;
      Id : Id_Node_Pointer := Id_Node_Pointer(Name);
   begin
      --Put_Line("Build_Arg, Name="&(+Id.Name));
      N := new Argument_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Name, Name);
      N.Set(My_Kind, Kind);
   end Build_Arg;


   ----------------------------------------------------------------------------
   procedure Build_Kind
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Kind : in     kv.avm.Registers.Data_Kind;
       Init : in     Node_Pointer := null) is
      N : Kind_Node_Pointer;
   begin
      --Put_Line("Build_Kind, kind="&kv.avm.Registers.Data_Kind'IMAGE(Kind));
      N := new Kind_Node_Class;
      UID.Next(N.ID);
      N.Kind := Kind;
      N.Line := Line;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Value, Init);
   end Build_Kind;

   ----------------------------------------------------------------------------
   function New_Constructor_Send
      (Actor : in     Node_Pointer;
       Args  : in     Node_Pointer) return Node_Pointer is
      N : Constructor_Send_Node_Pointer;
   begin
      N := new Constructor_Send_Node_Class;
      UID.Next(N.ID);
      N.Line := Actor.Line;
      N.Set(My_Self, Node_Pointer(N));
      N.Set(My_Destination, Actor);
      N.Set(My_Arguments, Args);
      return Node_Pointer(N);
   end New_Constructor_Send;

   ----------------------------------------------------------------------------
   procedure Build_Op_Expression
      (Node  : in out Node_Pointer;
       Line  : in     Positive;
       Op    : in     kv.avm.Instructions.operation_type;
       Left  : in     Node_Pointer;
       Right : in     Node_Pointer := null) is
      N : Expression_Op_Pointer;
   begin
      N := new Expression_Op_Class;
      UID.Next(N.ID);
      N.Line := Line;
      N.Op := Op;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Left, Left);
      N.Set(My_Right, Right);
   end Build_Op_Expression;

   ----------------------------------------------------------------------------
   procedure Build_Var_Expression
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Self : in     Boolean;
       Name : in     Node_Pointer) is
      N : Expression_Var_Pointer;
   begin
      N := new Expression_Var_Class;
      UID.Next(N.ID);
      N.Line := Line;
      N.Self := Self;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Name, Name);
   end Build_Var_Expression;

   ----------------------------------------------------------------------------
   procedure Build_Literal_Expression
      (Node  : in out Node_Pointer;
       Line  : in     Positive;
       Kind  : in     kv.avm.Registers.Data_Kind;
       Value : in     String) is
      N : Expression_Literal_Pointer;
   begin
      N := new Expression_Literal_Class;
      UID.Next(N.ID);
      N.Line := Line;
      N.Kind := Kind;
      N.Value := +Value;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
   end Build_Literal_Expression;

   ----------------------------------------------------------------------------
   procedure Build_Assignment
      (Node   : in out Node_Pointer;
       Line   : in     Positive;
       Target : in     Node_Pointer;
       Value  : in     Node_Pointer) is
      N : Statement_Assign_Pointer;
   begin
      N := new Statement_Assign_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Destination, Target);
      N.Set(My_Value, Value);
   end Build_Assignment;

   ----------------------------------------------------------------------------
   procedure Build_Var_Def
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     Node_Pointer;
       Ty_I : in     Node_Pointer) is
      N : Statement_Var_Def_Pointer;
   begin
      N := new Statement_Var_Def_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      --Put_Line("Build_Var_Def, Name="&(+N.Name.Name));
      N.Set(My_Self, Node);
      N.Set(My_Name, Name);
      N.Set(My_Kind, Ty_I);
   end Build_Var_Def;

   ----------------------------------------------------------------------------
   procedure Build_Emit
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Expr : in     Node_Pointer) is
      N : Statement_Emit_Pointer;
   begin
      N := new Statement_Emit_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      --Put_Line("Build_Emit");
      N.Set(My_Self, Node);
      N.Set(My_Value, Expr);
   end Build_Emit;

   ----------------------------------------------------------------------------
   procedure Build_Return
      (Node : in out Node_Pointer;
       Line : in     Positive;
       What : in     Node_Pointer) is
      N : Statement_Return_Pointer;
   begin
      N := new Statement_Return_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      --Put_Line("Build_Return");
      N.Set(My_Self, Node);
      N.Set(My_Value, What);
   end Build_Return;

   ----------------------------------------------------------------------------
   procedure Build_Call_Statement
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Kind : in     Destination_Type;
--       Self : in     Boolean;
--       Call : in     Boolean;
       Dest : in     Node_Pointer;
       Name : in     Node_Pointer;
       Args : in     Node_Pointer) is
      N : Expression_Send_Pointer;
   begin
      N := new Expression_Call_Class;
      UID.Next(N.ID);
      N.Line := Line;
--      N.Is_Gosub := Self;
--      N.Is_Call := Call;
      N.Destination := Kind;
      N.Is_Gosub := (Kind = Self);
      N.Is_Call := True;
      Node := Node_Pointer(N);
      --Put_Line("Build_Send to "&(+N.Message_Name.Name));
      N.Set(My_Self, Node);
      N.Set(My_Destination, Dest);
      N.Set(My_Name, Name);
      N.Set(My_Arguments, Args);
   end Build_Call_Statement;

   ----------------------------------------------------------------------------
   procedure Build_Send_Statement
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Kind : in     Destination_Type;
       Dest : in     Node_Pointer;
       Name : in     Node_Pointer;
       Args : in     Node_Pointer) is
      N : Statement_Send_Pointer;
   begin
      N := new Statement_Send_Class;
      UID.Next(N.ID);
      N.Line := Line;
      N.Destination := Kind;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Destination, Dest);
      N.Set(My_Name, Name);
      N.Set(My_Arguments, Args);
   end Build_Send_Statement;

   ----------------------------------------------------------------------------
   procedure Build_If
      (Node      : in out Node_Pointer;
       Line      : in     Positive;
       Condition : in     Node_Pointer;
       Then_Part : in     Node_Pointer;
       Else_Part : in     Node_Pointer) is
      N : Statement_If_Pointer;
   begin
      N := new Statement_If_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      --Put_Line("Build_If");
      N.Set(My_Self, Node);
      N.Set(My_Condition, Condition);
      N.Set(My_Then_Part, Then_Part);
      N.Set(My_Else_Part, Else_Part);
   end Build_If;

   ----------------------------------------------------------------------------
   procedure Build_Fold
      (Node : in out Node_Pointer;
       Line : in     Positive;
       What : in     Node_Pointer) is
      N : Expression_Fold_Pointer;
   begin
      N := new Expression_Fold_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Value, What);
   end Build_Fold;

   ----------------------------------------------------------------------------
   procedure Build_Assert
      (Node      : in out Node_Pointer;
       Line      : in     Positive;
       Condition : in     Node_Pointer) is

      N : Statement_Assert_Pointer;

   begin
      N := new Statement_Assert_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Condition, Condition);
   end Build_Assert;


   ----------------------------------------------------------------------------
   procedure Build_While
      (Node      : in out Node_Pointer;
       Line      : in     Positive;
       Condition : in     Node_Pointer;
       Loop_Part : in     Node_Pointer) is

      N : Statement_While_Pointer;

   begin
      N := new Statement_While_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      N.Set(My_Self, Node);
      N.Set(My_Condition, Condition);
      N.Set(My_Loop_Part, Loop_Part);
   end Build_While;









   ----------------------------------------------------------------------------
   procedure Build_Program
      (Node    : in out Node_Pointer;
       Line    : in     Positive;
       Imports : in     Node_Pointer;
       Actors  : in     Node_Pointer) is
      N : Program_Pointer;
   begin
      N := new Program_Class;
      UID.Next(N.ID);
      N.Line := Line;
      Node := Node_Pointer(N);
      --Put_Line("Build_Program");
      N.Set(My_Self, Node);
      N.Set(My_Imports, Imports);
      N.Set(My_Actors, Actors);
   end Build_Program;

   Last_Saved_Program : Program_Pointer;

   ----------------------------------------------------------------------------
   procedure Save_Program
      (Node : in     Node_Pointer) is
   begin
      Last_Saved_Program := Program_Pointer(Node);
   end Save_Program;

   ----------------------------------------------------------------------------
   function Get_Program return Program_Pointer is
   begin
      return Last_Saved_Program;
   end Get_Program;


end kv.avm.vole_tree;
