with Ada.Streams.Stream_IO;
with Ada.Strings.Maps;
with Ada.Text_IO;

with String_Ops;

with kv.avm.Tuples;

package body kv.avm.Registers is


   function "+"(S : String) return String_Type renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+"(U : String_Type) return String renames Ada.Strings.Unbounded.To_String;


   type Data_Kind_Lookup_Type is array (Data_Kind) of Character;
   Data_Kind_Signatures : constant Data_Kind_Lookup_Type := (
      Unset              => 'E',
      Signed_Integer     => 'I',
      Unsigned_Integer   => 'U',
      Floatingpoint      => 'F',
      Bit_Or_Boolean     => 'B',
      Tuple              => 'T',
      Tuple_Map          => 'm',
      Immutable_String   => 'S',
      Actor_Reference    => 'R',
      Actor_Definition   => 'A',
      Message_Definition => 'M',
      Future             => 'f',
      Tuple_Definition   => 'd');

   -----------------------------------------------------------------------------
   function Signature(Format : in Data_Kind) return Character is
   begin
      return Data_Kind_Signatures(Format);
   end Signature;

   -----------------------------------------------------------------------------
   function Format(Signature : in Character) return Data_Kind is
   begin
      for Answer in Data_Kind loop
         if Data_Kind_Signatures(Answer) = Signature then
            return Answer;
         end if;
      end loop;
      return Unset;
   end Format;

   -----------------------------------------------------------------------------
   function Signature_To_String(Signature : in Signature_Type) return String is
      Answer : String(1..Signature'LENGTH);
      Index : Integer := 1;
   begin
      for Kind in Signature'RANGE loop
         Answer(Index) := kv.avm.Registers.Signature(Signature(Kind));
         Index := Index + 1;
      end loop;
      return Answer;
   end Signature_To_String;

   -----------------------------------------------------------------------------
   function String_To_Signature(Signature : in String) return Signature_Type is
      Answer : Signature_Type(1..Signature'LENGTH);
   begin
      for Index in 1..Signature'LENGTH loop
         Answer(Index) := Format(Signature(Index + Signature'FIRST - 1));
      end loop;
      return Answer;
   end String_To_Signature;


   -----------------------------------------------------------------------------
   function Reg_Img(Reg : Register_Type) return String is
      Fmt : constant String := Data_Kind'IMAGE(Reg.Format);
   begin
      case Reg.Format is
         when Signed_Integer =>
            return " " & Interfaces.Integer_64'IMAGE(Reg.signed_value);
         when Unsigned_Integer =>
            return " uint:" & Interfaces.Unsigned_64'IMAGE(Reg.unsigned_value);
         when Bit_Or_Boolean =>
            return " " & Boolean'IMAGE(Reg.bit);
         when Floatingpoint =>
            return " " & Interfaces.IEEE_Float_64'IMAGE(Reg.value);
         when Actor_Definition =>
            return " Def:" & (+Reg.Actor_Kind);
         when Immutable_String =>
            return " '" & (+Reg.The_String) & "'";
         when Message_Definition =>
            return " Msg:" & (+Reg.Message_Name);
         when Future =>
            return " " & Fmt & Interfaces.Unsigned_32'IMAGE(Reg.ID);
         when Tuple =>
            return " [" & Reg.folded_tuple.To_String & "]";
         when Actor_Reference =>
            return " Ref:" & Reg.Instance.Image;
         when others =>
            null;
      end case;
      return Fmt;
   end Reg_Img;

   -----------------------------------------------------------------------------
   procedure Register_Write(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : in Register_Type) is
   begin
      Data_Kind'WRITE(Stream, Item.Format);
      case Item.Format is
         when Signed_Integer =>
            Interfaces.Integer_64'WRITE(Stream, Item.signed_value);
         when Unsigned_Integer =>
            Interfaces.Unsigned_64'WRITE(Stream, Item.Unsigned_Value);
         when Actor_Definition =>
            String'OUTPUT(Stream, +Item.Actor_Kind);
         when Message_Definition =>
            String'OUTPUT(Stream, +Item.Message_Name);
         when Immutable_String =>
            String'OUTPUT(Stream, +Item.The_String);
         when Tuple =>
            kv.avm.Tuples.Tuple_Type'WRITE(Stream, Item.Folded_Tuple);
         when Tuple_Map =>
            kv.avm.Tuples.Map_Type'WRITE(Stream, Item.map);
         when Bit_Or_Boolean =>
            Boolean'OUTPUT(Stream, Item.bit);
         when others =>
            Ada.Text_IO.Put_Line("ERROR: Register_Write of " & Data_Kind'IMAGE(Item.Format) & " is not implemented yet.");
            raise Unimplemented_Error;
      end case;
   end Register_Write;

   -----------------------------------------------------------------------------
   procedure Register_Read(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : out Register_Type) is
      Kind : Data_Kind;
   begin
      Data_Kind'READ(Stream, Kind);
      case Kind is
         when Signed_Integer =>
            declare
               Value : Interfaces.Integer_64;
            begin
               Interfaces.Integer_64'READ(Stream, Value);
               Item := (Format => Signed_Integer, signed_value => Value);
            end;
         when Unsigned_Integer =>
            declare
               Value : Interfaces.Unsigned_64;
            begin
               Interfaces.Unsigned_64'READ(Stream, Value);
               Item := (Format => Unsigned_Integer, Unsigned_Value => Value);
            end;
         when Actor_Definition =>
            Item := (Format => Actor_Definition, Actor_Kind => +String'INPUT(Stream));
         when Message_Definition =>
            Item := (Format => Message_Definition, Message_Name => +String'INPUT(Stream), Send_Count => 0, Reply_Count => 0);
         when Immutable_String =>
            Item := (Format => Immutable_String, The_String => +String'INPUT(Stream));
         when Tuple =>
            declare
               Value : kv.avm.Tuples.Tuple_Type;
            begin
               kv.avm.Tuples.Tuple_Type'READ(Stream, Value);
               Item := (Format => Tuple, Folded_Tuple => Value);
            end;
         when Tuple_Map =>
            declare
               Value : kv.avm.Tuples.Map_Type;
            begin
               kv.avm.Tuples.Map_Type'READ(Stream, Value);
               Item := (Format => Tuple_Map, map => Value);
            end;
         when Bit_Or_Boolean =>
            Item := (Format => Bit_Or_Boolean, bit => Boolean'INPUT(Stream));
         when others =>
            Ada.Text_IO.Put_Line("ERROR: Register_Read of " & Data_Kind'IMAGE(Kind) & " is not implemented yet.");
            raise Unimplemented_Error;
      end case;
   end Register_Read;


   -----------------------------------------------------------------------------
   function Bool(B : Boolean) return String is
   begin
      return " "&Boolean'IMAGE(B)(1..1);
   end Bool;


   -----------------------------------------------------------------------------
   function Make_Tuple_Map(Value : kv.avm.references.Reference_Array_Type) return Register_Type is
      Tuple_Layout : kv.avm.Tuples.Map_Type;
      Fold_List : aliased constant kv.avm.references.Reference_Array_Type := Value;
      use kv.avm.Registers;
   begin
      Tuple_Layout.Set(Fold_List'ACCESS);
      return (format => Tuple_Map, Map => Tuple_Layout);
   end Make_Tuple_Map;


   Reference_Set : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps.To_Set("SsIiLlAaFfCc0123456789");


   -----------------------------------------------------------------------------
   function Is_Reference_Character(C : in     Character) return Boolean is
   begin
      return Ada.Strings.Maps.Is_In(C, Reference_Set);
   end Is_Reference_Character;

   -----------------------------------------------------------------------------
   -- The *_First and *_Rest routines come from LISP car and cdr,
   -- and provide easy parsing support.
   --
   function Reference_First
      (Str : in     String) return String is
      S : Natural := Str'FIRST;
   begin
      while S < Str'LAST loop
      exit when Is_Reference_Character(Str(S));
         S := S + 1;
      end loop;
      for I in S .. Str'LAST loop
         if not Is_Reference_Character(Str(I)) then
            return Str(S..I-1);
         end if;
      end loop;
      return Str; -- no blanks, return the whole string.
   end Reference_First;
   -----------------------------------------------------------------------------
   function Reference_Rest
      (Str : in     String) return String is

      B : Boolean := False; -- found a blank
      S : Natural := Str'FIRST;

   begin
      while not Is_Reference_Character(Str(S)) loop
         if S = Str'LAST then
            return "";
         end if;
         S := S + 1;
      end loop;
      for I in S .. Str'LAST loop
         if not Is_Reference_Character(Str(I)) then
            B := True;
         elsif B then
            return Str(I..Str'LAST);
         end if;
      end loop;
      return ""; -- there was no second part to Str
   end Reference_Rest;


   Foundation_For_Empty_Reference_Array : constant kv.avm.references.Reference_Array_Type := (kv.avm.references.Make_Reference("L9"), kv.avm.references.Make_Reference("L1"));
   Empty_Reference_Array_Default : constant kv.avm.references.Reference_Array_Type := Foundation_For_Empty_Reference_Array(1..0);

   -----------------------------------------------------------------------------
   -- Tuple Maps should never be too long so a *simple* recursive parser will
   -- do the job.
   --
   function String_To_Reference_Array_Type(Token : String; List : kv.avm.references.Reference_Array_Type := Empty_Reference_Array_Default) return kv.avm.references.Reference_Array_Type is
      First : constant String := Reference_First(Token);
      use Interfaces;
   begin
      --Ada.Text_IO.Put_Line("String_To_Reference_Array_Type '" & Token & "', len(List) = " & Natural'IMAGE(List'LENGTH));
      if First = "" then
         return List;
      else
         declare
            Plus_One : kv.avm.references.Reference_Array_Type(1 .. List'LENGTH + 1);
         begin
            Plus_One(1 .. List'LENGTH) := List;
            Plus_One(List'LENGTH + 1) := kv.avm.references.Make_Reference(First);
            return String_To_Reference_Array_Type(Reference_Rest(Token), Plus_One);
         end;
      end if;
   end String_To_Reference_Array_Type;


   -----------------------------------------------------------------------------
   function String_To_Tuple_Map(Token : String) return Register_Type is
      Tuple_Layout : kv.avm.Tuples.Map_Type;
      Fold_List : aliased constant kv.avm.references.Reference_Array_Type := String_To_Reference_Array_Type(Token);
   begin
      Tuple_Layout.Set(Fold_List'ACCESS);
      return (format => Tuple_Map, Map => Tuple_Layout);
   end String_To_Tuple_Map;


   -----------------------------------------------------------------------------
   function Make_S(Value : Interfaces.Integer_64) return Register_Type is
   begin
      return (format => Signed_Integer, signed_value => Value);
   end Make_S;

   -----------------------------------------------------------------------------
   function Make_U(Value : Interfaces.Unsigned_64) return Register_Type is
   begin
      return (format => Unsigned_Integer, unsigned_value => Value);
   end Make_U;

   -----------------------------------------------------------------------------
   function Make_String(Value : String) return Register_Type is
   begin
      return (Format => Immutable_String, The_String => +Value);
   end Make_String;

   -----------------------------------------------------------------------------
   function Make_Tuple(Value : kv.avm.Tuples.Tuple_Type) return Register_Type is
   begin
      return (Format => Tuple, Folded_Tuple => Value);
   end Make_Tuple;

   -----------------------------------------------------------------------------
   function Make_Ref(Value : kv.avm.Actor_References.Actor_Reference_Type) return Register_Type is
   begin
      return (Format => Actor_Reference, Instance => Value);
   end Make_Ref;


end kv.avm.Registers;
