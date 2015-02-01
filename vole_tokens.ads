with  Kv.avm.vole_Tree;
package Vole_Tokens is


   type YYSType is
      record
         Node : kv.avm.vole_tree.Node_Pointer;
      end record;

    YYLVal, YYVal : YYSType; 
    type Token is
        (End_Of_Input, Error, Key_Import, Id_Token,
         Eos_Token, Block_Begin, Block_End,
         Key_Attribute, Key_Predicate, Key_Message,
         Key_Returns, Key_Actor, Key_Constructor,
         Key_Extends, Key_Assert, Key_Method,
         Key_Emit, Key_If, Key_Then,
         Key_Self, Key_Super, Key_Send,
         Key_Else, Key_Elseif, Key_Endif,
         Key_When, Key_While, Key_Do,
         Key_Loop, Key_For, Key_Case,
         Key_Local, Key_New, Op_And,
         Op_Not, Op_Or, Op_Xor,
         Op_Mod, Op_Eq, Op_Not_Eq,
         Op_Gt, Op_Gt_Eq, Op_Lt_Eq,
         Op_Lt, Key_Return, Key_Tuple,
         Float_Literal, Integer_Literal, String_Literal,
         True_Literal, False_Literal, Actor_Type,
         Boolean_Type, Tuple_Type, Unsigned_Type,
         Integer_Type, Float_Type, String_Type,
         Colon_Token, Dot_Token, Paren_Begin,
         Paren_End, Comma_Token, Tuple_Begin,
         Tuple_End, Op_Shift_Left, Op_Shift_Right,
         Op_Add, Op_Sub, Op_Mul,
         Op_Div, High_Right_Precedence, Op_Exp,
         Op_Assign );

    Syntax_Error : exception;

end Vole_Tokens;
