with Ada.Text_IO;

with kv.avm.vole_tree; use kv.avm.vole_tree;
with kv.avm.Tree_Visitors; use kv.avm.Tree_Visitors;
with kv.avm.Code_Buffers;

package kv.avm.Code_Generator is


   Non_Boolean_Condition_Error : exception;


   type Code_Generator_Class is limited new Visitor_Class with
      record
         Buffer : kv.avm.Code_Buffers.Buffer_Type;
      end record;

   not overriding
   procedure Init
      (Self : in out Code_Generator_Class);

   not overriding
   procedure Finalize
      (Self      : in out Code_Generator_Class);


   not overriding
   procedure Print
      (Self : in     Code_Generator_Class);

   not overriding
   procedure Process_Lines
      (Self      : in     Code_Generator_Class;
       Processor : access procedure(Line : String));



   overriding
   procedure Visit_Id
      (Self   : in out Code_Generator_Class;
       Target : in out Id_Node_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Actor_Definition
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Attribute_Definition
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Message_Definition
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Kind_Node
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Argument
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Constructor_Send_Node
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Expression_List
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Expression_Op
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Expression_Var
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Expression_Literal
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Expression_Send
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Expression_Fold
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_List
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_Assign
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_Var_Def
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_Emit
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_Return
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_If
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_Assert
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_Send
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Statement_While
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding
   procedure Visit_Program
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);


   overriding
   procedure Visit_Unimp
      (Self   : in out Code_Generator_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);


end kv.avm.Code_Generator;
