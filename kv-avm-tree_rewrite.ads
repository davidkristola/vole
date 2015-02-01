with Ada.Text_IO;

with kv.avm.vole_tree; use kv.avm.vole_tree;
with kv.avm.Tree_Visitors; use kv.avm.Tree_Visitors;

package kv.avm.Tree_Rewrite is


   type Rewriter_Class is limited new Visitor_Class with
      record
         Temp : Natural := 0;
         Block : Natural := 0;
      end record;

   procedure Init
      (Self : in out Rewriter_Class);

   procedure Finalize
      (Self      : in out Rewriter_Class);

   function Next_Temp(Self : access Rewriter_Class; Src_Line : Positive) return String;
   function Next_Block(Self : access Rewriter_Class) return String;



   overriding procedure Visit_Id
      (Self   : in out Rewriter_Class;
       Target : in out Id_Node_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Actor_Definition
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Attribute_Definition
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Message_Definition
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Kind_Node
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Argument
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Constructor_Send_Node
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Expression_List
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Expression_Op
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Expression_Var
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Expression_Literal
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Expression_Send
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Expression_Fold
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_List
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_Assign
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_Var_Def
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_Emit
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_Return
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_If
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_Assert
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_Send
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Statement_While
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);
   overriding procedure Visit_Program
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);


   overriding procedure Visit_Unimp
      (Self   : in out Rewriter_Class;
       Target : in out Node_Base_Class'CLASS;
       Depth  : in     Natural);


end kv.avm.Tree_Rewrite;
