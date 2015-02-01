with Ada.Exceptions;
use Ada.Exceptions;

package body kv.avm.References is

   -----------------------------------------------------------------------------
   function Ref_Img(Ref : Reference_Type) return String is
      Index : constant String := Offset_Type'IMAGE(Ref.Index);
   begin
      return Make_Register_Name(Natural(Ref.Index), Ref.Memory);
   end Ref_Img;

   ----------------------------------------------------------------------------
   function Make_Register_Name
      (Index : Natural;
       Bank  : Register_Bank_Type) return String is

      Lookup : constant array (Register_Bank_Type) of Character := ('I', 'L', 'A', 'C'); -- used to be SLAF
      Answer : String := Natural'IMAGE(Index);

   begin
      Answer(1) := Lookup(Bank);
      return Answer;
   end Make_Register_Name;


   ----------------------------------------------------------------------------
   function Make_Reference(Token : String) return Reference_Type is
      Answer : Reference_Type;
   begin
      case Token(Token'FIRST) is
         when 'S' | 's' | 'I' | 'i' => -- Old: S, New: I (input)
            Answer.Memory := Input;
         when 'L' | 'l' =>
            Answer.Memory := Local;
         when 'A' | 'a' =>
            Answer.Memory := Attribute;
         when 'F' | 'f' | 'C' | 'c' => -- Old: F, New: C (constant)
            Answer.Memory := Fixed;
         when others =>
            Raise_Exception(Invalid_Reference_Designator_Error'IDENTITY, "Invalid reference designator, expected I, L, A, or C; got: " & Token(Token'FIRST));
      end case;
      Answer.Index := Offset_Type'VALUE(Token(Token'FIRST+1..Token'LAST));
      return Answer;
   end Make_Reference;



end kv.avm.References;
