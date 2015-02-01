with Ada.Finalization;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Interfaces;

with kv.avm.Instructions;
with kv.avm.Tuples;
with kv.avm.Actor_References;
with kv.avm.references;

package kv.avm.Registers is

   Unimplemented_Error : exception;

   subtype String_Type is Ada.Strings.Unbounded.Unbounded_String;

   function "+"(S : String) return String_Type;
   function "+"(U : String_Type) return String;

   type Data_Kind is
      (Unset,
       Signed_Integer,
       Unsigned_Integer,
       Floatingpoint,
       Bit_Or_Boolean,
       Tuple,
       Tuple_Map,
       Immutable_String,
       Actor_Reference, -- A reference to an actor instance (can't be used to create a new actor)
       Actor_Definition, -- The type of actor being created (and constructor message definition)
       Message_Definition,
       Future,
       Tuple_Definition);
   for Data_Kind'SIZE use 4;

   type Signature_Type is array (positive range <>) of Data_Kind;
   type Signature_Access is access Signature_Type;

   function Signature(Format : in Data_Kind) return Character;
   function Format(Signature : in Character) return Data_Kind; -- Will return Unset for unrecognized signatures
   function Signature_To_String(Signature : in Signature_Type) return String;
   function String_To_Signature(Signature : in String) return Signature_Type;

   type Constant_String_Access is access constant String;

   -- This is a variant record so that it is a constrained type and can be used
   -- in an array.
   --
   type Register_Type(format : Data_Kind := Unset) is
      record
         case format is
            when Signed_Integer =>
               Signed_Value : Interfaces.Integer_64;
            when Unsigned_Integer =>
               Unsigned_Value : Interfaces.Unsigned_64;
            when Bit_Or_Boolean =>
               Bit : boolean;
            when Floatingpoint =>
               Value : Interfaces.IEEE_Float_64;
            when Tuple =>
               Folded_Tuple : kv.avm.Tuples.Tuple_Type;
            when Tuple_Map =>
               Map : kv.avm.Tuples.Map_Type;
            when Actor_Reference =>
               Instance : kv.avm.Actor_References.Actor_Reference_Type;
            when Actor_Definition =>
               Actor_Kind : String_Type;
            when Immutable_String =>
               The_String : String_Type;
            when Future =>
               ID : Interfaces.Unsigned_32;
            when Message_Definition =>
               Message_Name : String_Type;
               Send_Count : Interfaces.Unsigned_32; -- how many elements are in the message tuple
               Reply_Count : Interfaces.Unsigned_32; -- how many elements are in the reply tuple
               --TODO Add tuple profiles
            when others =>
               null;
         end case;
      end record;

   function Reg_Img(Reg : Register_Type) return String;

   procedure Register_Write(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : in Register_Type);
   for Register_Type'WRITE use Register_Write;
   procedure Register_Read(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : out Register_Type);
   for Register_Type'READ use Register_Read;


   function Bool(B : Boolean) return String;

   function Make_Tuple_Map(Value : kv.avm.references.Reference_Array_Type) return Register_Type;
   function String_To_Tuple_Map(Token : String) return Register_Type;

   function Make_S(Value : Interfaces.Integer_64) return Register_Type;
   function Make_U(Value : Interfaces.Unsigned_64) return Register_Type;
   function Make_String(Value : String) return Register_Type;
   function Make_Tuple(Value : kv.avm.Tuples.Tuple_Type) return Register_Type;
   function Make_Ref(Value : kv.avm.Actor_References.Actor_Reference_Type) return Register_Type;

end kv.avm.Registers;
