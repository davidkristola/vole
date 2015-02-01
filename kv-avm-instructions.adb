with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

package body kv.avm.Instructions is

   ----------------------------------------------------------------------------
   function Encode_Operation(Op : Operation_Type) return String is
   begin
      case Op is
         when Add => return "+";
         when Sub => return "-";
         when Mul => return "*";
         when Div => return "/";
         when Modulo => return "mod";
         when Exp => return "^";
         when Negate => return "not";
         when Eq => return "=";
         when Neq => return "/=";
         when L_t => return "<";
         when Lte => return "<=";
         when G_t => return ">";
         when Gte => return ">=";
         when Shift_Right => return ">>";
         when Shift_Left => return "<<";
         when B_And => return "and";
         when B_Or => return "or";
         when B_Xor => return "xor";
         when Op_Pos => return "+";
         when Op_Neg => return "-";
      end case;
   end Encode_Operation;

   ----------------------------------------------------------------------------
   function Decode_Operation(Op_Img : String) return Operation_Type is
      Adjusted : constant String := Ada.Strings.Fixed.Translate(Op_Img, Ada.Strings.Maps.Constants.Lower_Case_Map);
   begin
      if Op_Img = "+" then return ADD;
      elsif Op_Img = "-" then return SUB;
      elsif Op_Img = "*" then return MUL;
      elsif Op_Img = "/" then return DIV;
      elsif (Op_Img = "%") or (Adjusted = "mod") then return Modulo;
      elsif Op_Img = "^" then return Exp;
      elsif (Op_Img = "~") or (Adjusted = "not") then return Negate;
      elsif Op_Img = "=" then return Eq;
      elsif (Op_Img = "/=") or (Op_Img = "!=") then return Neq;
      elsif Op_Img = "<" then return L_t;
      elsif Op_Img = "<=" then return Lte;
      elsif Op_Img = ">" then return G_t;
      elsif Op_Img = ">=" then return Gte;
      elsif Op_Img = "<<" then return Shift_Left;
      elsif Op_Img = ">>" then return Shift_Right;
      elsif (Op_Img = "&") or (Adjusted = "and") then return B_And;
      elsif (Op_Img = "|") or (Adjusted = "or") then return B_Or;
      elsif Adjusted = "xor" then return B_Xor;
      end if;
      raise Invalid_Compute_Operation;
   end Decode_Operation;

end kv.avm.Instructions;
