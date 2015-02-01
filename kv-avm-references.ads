with Interfaces;

package kv.avm.References is

   pragma preelaborate;

   Invalid_Reference_Designator_Error : exception;

   -- This enumerates the four different types of register banks.
   -- Note that "Constant" can't be used because it is an Ada
   -- reserved word.  Therefore "Fixed" is used instead.
   --
   type Register_Bank_Type is (Input, Local, Attribute, Fixed);
   for Register_Bank_Type'SIZE use 2; -- 2 bits

   type Offset_Type is mod 2**10; -- 10 bits

   -- This has to be 12 bits so that four of them can be packed into
   -- an instruction.
   --
   type Reference_Type is
      record
         Memory : Register_Bank_Type;
         Index : Offset_Type;
      end record;
   for Reference_Type use
      record
         Memory at 0 range 0 .. 1;
         Index at 0 range 2 .. 11;
      end record;
   for Reference_Type'SIZE use 12;

   type Reference_Array_Type is array (Interfaces.Unsigned_32 range <>) of Reference_Type;
   type Reference_Array_Access is access all Reference_Array_Type;


   function Ref_Img(Ref : Reference_Type) return String;

   function Make_Register_Name
      (Index : Natural;
       Bank  : Register_Bank_Type) return String;

   function Make_Reference(Token : String) return Reference_Type;

end kv.avm.References;
