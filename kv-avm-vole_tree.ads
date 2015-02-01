with Ada.Strings.Unbounded;

limited with kv.avm.Tree_Visitors;
limited with kv.avm.Symbol_Tables;
with kv.avm.Instructions;
with kv.avm.Registers; use kv.avm.Registers;

package kv.avm.vole_tree is

   Parsing_Error : exception;
   Missing_Parent_Error : exception;
   Association_Error : exception;
   Unresolveable_Name : exception;
   Variable_Undefined : exception;
   Variable_Unspecified : exception;
   Terminate_Parsing : exception; -- Once an error has been dealt with, raise this to quietly terminate parsing.

   subtype String_Type is Ada.Strings.Unbounded.Unbounded_String;

   package Visitors renames kv.avm.Tree_Visitors;




   type Node_Base_Class;
   type Node_Pointer is access all Node_Base_Class'CLASS;





   type Association_Type is (My_Self, My_Parent, My_Next, My_Previous, My_Name, My_Attributes, My_Methods, My_Kind, My_Inputs, My_Outputs,
      My_Code, My_Arguments, My_Destination, My_Left, My_Right, My_Value, My_Condition, My_Then_Part, My_Else_Part, My_Loop_Part, My_Imports,
      My_Actors, My_Loop_Exit);
   subtype Child_Association_Type is Association_Type range My_Name .. Association_Type'LAST;


   -- Some class-wide navigation functions
   function Actor_Of(Target : Node_Base_Class'CLASS) return Node_Pointer;
   function Message_Of(Target : Node_Base_Class'CLASS) return Node_Pointer;
   function Association_Of(Target : Node_Base_Class'CLASS) return Association_Type;
   function Find_Actor(Target : String) return Node_Pointer;

   -- Some class-wide utilities
   function Resolve_Name(Target : Node_Base_Class'CLASS; Raise_Errors : Boolean := False) return String;
   function Resolve_Ref_Name(Target : Node_Base_Class'CLASS; Ref_Name : String; Raise_Errors : Boolean := False) return String;
   function Resolve_Register(Target : Node_Base_Class'CLASS; Raise_Errors : Boolean := False) return String;


   type Association_List_Type is array (Association_Type) of Node_Pointer;


   ----------------------------------------------------------------------------
   -- This is the abstract base class of all node classes of the vole
   -- abstract syntax tree.
   --
   -- Each node has a uniqie ID.
   -- The Line represents the source code associated with the AST node.
   -- Nodes are associataed with one or more other nodes through Associations.
   -- Nodes may have a distinct register type (kind).
   --
   type Node_Base_Class is abstract tagged
      record
         ID           : Positive;
         Line         : Positive;
         Associations : Association_List_Type;
         Kind         : kv.avm.Registers.Data_Kind := Unset;
      end record;
   procedure Visit(Self : in out Node_Base_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural) is abstract;
   function Get_ID(Self : Node_Base_Class) return Positive;
   function Get_Line(Self : Node_Base_Class) return Positive;
   function Get_Name(Self : Node_Base_Class) return String;
   function Get_Association(Self : Node_Base_Class; Association : Association_Type) return Node_Pointer;
   procedure Set
      (Node        : in out Node_Base_Class;
       Association : in     Association_Type;
       Target      : in     Node_Pointer);
   function Get_Kind(Self : Node_Base_Class) return kv.avm.Registers.Data_Kind;
   procedure Set_Kind(Self : in out Node_Base_Class; Kind : in kv.avm.Registers.Data_Kind);
   function Get_Length(Self : Node_Base_Class) return Positive; -- 1 for non-lists


   type Node_List_Class is abstract new Node_Base_Class with null record;
   type Node_List_Pointer is access all Node_List_Class;
   overriding function Get_Length(Self : Node_List_Class) return Positive;

   type Id_Node_Class is new Node_Base_Class with
      record
         Name : String_Type;
      end record;
   type Id_Node_Pointer is access all Id_Node_Class;
   overriding procedure Visit(Self : in out Id_Node_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   overriding function Get_Name(Self : Id_Node_Class) return String;

   -- Get_Symbol_Table takes a Boolean that means:
   VARIABLE_SYMBOLS : constant Boolean := True;
   CONSTANT_SYMBOLS : constant Boolean := False;

   type Actor_Definition_Class is new Node_List_Class with
      record
         Attribute_Symbols : access kv.avm.Symbol_Tables.Symbol_Table;
         Constant_Symbols  : access kv.avm.Symbol_Tables.Symbol_Table;
         Super_Class       : Node_Pointer;
      end record;
   type Actor_Definition_Pointer is access all Actor_Definition_Class;
   overriding procedure Visit(Self : in out Actor_Definition_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   overriding function Get_Name(Self : Actor_Definition_Class) return String;
   not overriding function Get_Symbol_Table(Self : Actor_Definition_Class; Attributes : Boolean) return access kv.avm.Symbol_Tables.Symbol_Table;
   not overriding
   function Get_Super_Class(Self : Actor_Definition_Class) return Node_Pointer;

   type Attribute_Definition_Class is new Node_List_Class with null record;
   type Attribute_Definition_Pointer is access all Attribute_Definition_Class;
   overriding procedure Visit(Self : in out Attribute_Definition_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   overriding function Get_Name(Self : Attribute_Definition_Class) return String;

   type Message_Definition_Class is new Node_List_Class with
      record
         Method  : Boolean; -- Methods are private (internal) messages
         Input_Symbols : access kv.avm.Symbol_Tables.Symbol_Table;
         Local_Symbols : access kv.avm.Symbol_Tables.Symbol_Table;
      end record;
   type Message_Definition_Pointer is access all Message_Definition_Class;
   overriding procedure Visit(Self : in out Message_Definition_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   overriding function Get_Name(Self : Message_Definition_Class) return String;
   not overriding function Get_Symbol_Table(Self : Message_Definition_Class; Local : Boolean) return access kv.avm.Symbol_Tables.Symbol_Table;

   type Kind_Node_Class is new Node_Base_Class with null record;
   type Kind_Node_Pointer is access all Kind_Node_Class;
   overriding procedure Visit(Self : in out Kind_Node_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);

   type Argument_Class is new Node_List_Class with null record;
   type Argument_Pointer is access all Argument_Class;
   overriding procedure Visit(Self : in out Argument_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   overriding function Get_Name(Self : Argument_Class) return String;

   type Constructor_Send_Node_Class is new Node_Base_Class with null record;
   type Constructor_Send_Node_Pointer is access all Constructor_Send_Node_Class;
   overriding procedure Visit(Self : in out Constructor_Send_Node_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);


   type Expression_List_Class is new Node_List_Class with
      record
         Temp_Name : String_Type;
      end record;
   type Expression_List_Pointer is access all Expression_List_Class;
   overriding procedure Visit(Self : in out Expression_List_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   not overriding procedure Set_Temp_Name
      (Self : in out Expression_List_Class;
       Name : in     String);
   not overriding function Get_Temp_Name(Self : Expression_List_Class) return String;

   type Expression_Op_Class is new Expression_List_Class with
      record
         Op : kv.avm.Instructions.operation_type;
      end record;
   type Expression_Op_Pointer is access all Expression_Op_Class;
   overriding procedure Visit(Self : in out Expression_Op_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   not overriding function Get_Op(Self : Expression_Op_Class) return kv.avm.Instructions.operation_type;

   type Expression_Var_Class is new Expression_List_Class with
      record
         Self : Boolean;
      end record;
   type Expression_Var_Pointer is access all Expression_Var_Class;
   overriding procedure Visit(Self : in out Expression_Var_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   overriding function Get_Name(Self : Expression_Var_Class) return String;
   not overriding function Get_Is_Self(Self : Expression_Var_Class) return Boolean;

   type Expression_Literal_Class is new Expression_List_Class with
      record
         Value : String_Type;
      end record;
   type Expression_Literal_Pointer is access all Expression_Literal_Class;
   overriding procedure Visit(Self : in out Expression_Literal_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   not overriding function Get_Value(Self : Expression_Literal_Class) return String;

   type Destination_Type is (Actor, Self, Super);
   type Expression_Call_Class is new Expression_List_Class with
      record
         Destination : Destination_Type;
         Is_Gosub    : Boolean;
         Is_Call     : Boolean;
         Is_Tail     : Boolean := False;
      end record;
   type Expression_Send_Pointer is access all Expression_Call_Class;
   overriding procedure Visit(Self : in out Expression_Call_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   not overriding procedure Set_Tail(Self : in out Expression_Call_Class; Tail : Boolean);
   not overriding function Get_Desc(Self : Expression_Call_Class) return String;
   not overriding function Is_Gosub(Self : Expression_Call_Class) return Boolean;
   not overriding function Is_Call(Self : Expression_Call_Class) return Boolean;
   not overriding function Is_Tail(Self : Expression_Call_Class) return Boolean;


   type Expression_Fold_Class is new Expression_List_Class with
      record
         Tuple_Map_Name : String_Type;
         Tuple_Map_Init : String_Type;
      end record;
   type Expression_Fold_Pointer is access all Expression_Fold_Class;
   overriding procedure Visit(Self : in out Expression_Fold_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   not overriding procedure Set_Tuple_Map_Name
      (Self : in out Expression_Fold_Class;
       Name : in     String);
   not overriding function Get_Tuple_Map_Name(Self : Expression_Fold_Class) return String;
   not overriding procedure Set_Tuple_Map_Init
      (Self : in out Expression_Fold_Class;
       Init : in     String);
   not overriding function Get_Tuple_Map_Init(Self : Expression_Fold_Class) return String;


   type Statement_List_Class is new Node_List_Class with
      record
         Block_Name : String_Type;
      end record;
   type Statement_List_Pointer is access all Statement_List_Class;
   overriding procedure Visit(Self : in out Statement_List_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   not overriding procedure Set_Block_Name
      (Self : in out Statement_List_Class;
       Name : in     String);
   not overriding function Get_Block_Name(Self : Statement_List_Class) return String;

   type Statement_Assign_Class is new Statement_List_Class with null record;
   type Statement_Assign_Pointer is access all Statement_Assign_Class;
   overriding procedure Visit(Self : in out Statement_Assign_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);

   type Statement_Var_Def_Class is new Statement_List_Class with null record;
   type Statement_Var_Def_Pointer is access all Statement_Var_Def_Class;
   overriding procedure Visit(Self : in out Statement_Var_Def_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   overriding function Get_Name(Self : Statement_Var_Def_Class) return String;

   type Statement_Emit_Class is new Statement_List_Class with null record;
   type Statement_Emit_Pointer is access all Statement_Emit_Class;
   overriding procedure Visit(Self : in out Statement_Emit_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);

   type Statement_Return_Class is new Statement_List_Class with null record;
   type Statement_Return_Pointer is access all Statement_Return_Class;
   overriding procedure Visit(Self : in out Statement_Return_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);

   type Statement_If_Class is new Statement_List_Class with null record;
   type Statement_If_Pointer is access all Statement_If_Class;
   overriding procedure Visit(Self : in out Statement_If_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);

   type Statement_Assert_Class is new Statement_List_Class with null record;
   type Statement_Assert_Pointer is access all Statement_Assert_Class;
   overriding procedure Visit(Self : in out Statement_Assert_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);

   type Statement_While_Class is new Statement_List_Class with null record;
   type Statement_While_Pointer is access all Statement_While_Class;
   overriding procedure Visit(Self : in out Statement_While_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);

   type Statement_Send_Class is new Statement_List_Class with
      record
         Destination : Destination_Type;
      end record;
   type Statement_Send_Pointer is access all Statement_Send_Class;
   overriding procedure Visit(Self : in out Statement_Send_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);
   not overriding procedure Set_Destination
      (Self : in out Statement_Send_Class;
       Dest : in     Destination_Type);
   not overriding function Get_Destination(Self : Statement_Send_Class) return Destination_Type;

   type Program_Class is new Node_Base_Class with null record;
   type Program_Pointer is access all Program_Class;
   overriding procedure Visit(Self : in out Program_Class; V : access Visitors.Visitor_Class'CLASS; D : Natural);








   procedure Build_Id_Node
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     String);

   procedure Build_Actor_Node
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     Node_Pointer;
       Attr : in     Node_Pointer;
       Meth : in     Node_Pointer;
       Supr : in     Node_Pointer := null);

   procedure Build_Attribute
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     Node_Pointer;
       Ty_I : in     Node_Pointer);

   procedure Add_Next
      (Node : in     Node_Pointer;
       Next : in     Node_Pointer);

   procedure Build_Message
      (Node : in out Node_Pointer;
       Line : in     Positive;
       pMsg : in     Boolean;
       Name : in     Node_Pointer;
       Args : in     Node_Pointer;
       Rtn  : in     Node_Pointer;
       Code : in     Node_Pointer;
       Pred : in     Node_Pointer);

   procedure Build_Constructor
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Args : in     Node_Pointer;
       Code : in     Node_Pointer);

   procedure Build_Arg
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     Node_Pointer;
       Kind : in     Node_Pointer);

   procedure Build_Kind
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Kind : in     kv.avm.Registers.Data_Kind;
       Init : in     Node_Pointer := null);

   function New_Constructor_Send
      (Actor : in     Node_Pointer;
       Args  : in     Node_Pointer) return Node_Pointer;

   procedure Build_Op_Expression
      (Node  : in out Node_Pointer;
       Line  : in     Positive;
       Op    : in     kv.avm.Instructions.operation_type;
       Left  : in     Node_Pointer;
       Right : in     Node_Pointer := null);

   procedure Build_Var_Expression
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Self : in     Boolean;
       Name : in     Node_Pointer);

   procedure Build_Literal_Expression
      (Node  : in out Node_Pointer;
       Line  : in     Positive;
       Kind  : in     kv.avm.Registers.Data_Kind;
       Value : in     String);

   procedure Build_Assignment
      (Node   : in out Node_Pointer;
       Line   : in     Positive;
       Target : in     Node_Pointer;
       Value  : in     Node_Pointer);

   procedure Build_Var_Def
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Name : in     Node_Pointer;
       Ty_I : in     Node_Pointer);

   procedure Build_Emit
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Expr : in     Node_Pointer);

   procedure Build_Return
      (Node : in out Node_Pointer;
       Line : in     Positive;
       What : in     Node_Pointer);

   procedure Build_Call_Statement
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Kind : in     Destination_Type;
--       Self : in     Boolean;
--       Call : in     Boolean;
       Dest : in     Node_Pointer;
       Name : in     Node_Pointer;
       Args : in     Node_Pointer);

   procedure Build_Send_Statement
      (Node : in out Node_Pointer;
       Line : in     Positive;
       Kind : in     Destination_Type;
       Dest : in     Node_Pointer;
       Name : in     Node_Pointer;
       Args : in     Node_Pointer);

   procedure Build_If
      (Node      : in out Node_Pointer;
       Line      : in     Positive;
       Condition : in     Node_Pointer;
       Then_Part : in     Node_Pointer;
       Else_Part : in     Node_Pointer);

   procedure Build_Fold
      (Node : in out Node_Pointer;
       Line : in     Positive;
       What : in     Node_Pointer);

   procedure Build_Assert
      (Node      : in out Node_Pointer;
       Line      : in     Positive;
       Condition : in     Node_Pointer);

   procedure Build_While
      (Node      : in out Node_Pointer;
       Line      : in     Positive;
       Condition : in     Node_Pointer;
       Loop_Part : in     Node_Pointer);



   procedure Build_Program
      (Node    : in out Node_Pointer;
       Line    : in     Positive;
       Imports : in     Node_Pointer;
       Actors  : in     Node_Pointer);

   procedure Save_Program
      (Node : in     Node_Pointer);

   function Get_Program return Program_Pointer;

end kv.avm.vole_tree;
