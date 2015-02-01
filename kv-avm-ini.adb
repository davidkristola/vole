with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO; --use Ada.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Characters.Latin_1;
with Interfaces;

with String_Ops;
with kv.avm.Assemblers;
with kv.avm.File_Reader;
with kv.avm.Memories;
with kv.avm.Registers;
with kv.avm.Log; use kv.avm.Log;

package body kv.avm.Ini is

   package String_Lists is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Positive,
       Element_Type => String);

   package Configurations is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => String_Lists.Vector,
       "="          => String_Lists."=");

   use Configurations;

   type Configuration_Access is access Configurations.Map;


   type Settings_Reference_Counter_Type is
      record
         Count : Natural := 0;
         Data  : Configuration_Access;
      end record;

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Settings_Reference_Counter_Type, Settings_Reference_Counter_Access);
   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Configurations.Map, Configuration_Access);

   -----------------------------------------------------------------------------
   procedure Initialize (Self : in out Settings_Type) is
   begin
      Self.Ref := new Settings_Reference_Counter_Type;
      Self.Ref.Count := 1;
      Self.Ref.Data := new Configurations.Map;
   end Initialize;

   -----------------------------------------------------------------------------
   procedure Adjust     (Self : in out Settings_Type) is
      Ref : Settings_Reference_Counter_Access := Self.Ref;
   begin
      if Ref /= null then
         Ref.Count := Ref.Count + 1;
      end if;
   end Adjust;

   -----------------------------------------------------------------------------
   procedure Finalize   (Self : in out Settings_Type) is
      Ref : Settings_Reference_Counter_Access := Self.Ref;
   begin
      Self.Ref := null;
      if Ref /= null then
         Ref.Count := Ref.Count - 1;
         if Ref.Count = 0 then
            Free(Ref.Data);
            Free(Ref);
         end if;
      end if;
   end Finalize;

   -----------------------------------------------------------------------------
   function Split_Index(The_String : String) return Natural is
   begin
      return Ada.Strings.Fixed.Index(The_String, "=");
   end Split_Index;

   -----------------------------------------------------------------------------
   procedure Parse_Line
      (Self : in out Settings_Type;
       Line : in     String) is

      Split_Point : Natural;
      Key_And_Value : constant String := String_Ops.Drop_Vole_Comments(Line);

   begin
      --Put_Line("Parse_Line = <"&Key_And_Value&">");
      Split_Point := Split_Index(Key_And_Value);
      --Put_Line("Split_Point =" & Natural'IMAGE(Split_Point));
      if Split_Point /= 0 then
         declare
            Key : constant String := String_Ops.Trim_Blanks(Key_And_Value(Key_And_Value'FIRST .. Split_Point-1));
            Value : constant String := String_Ops.Trim_Blanks(Key_And_Value(Split_Point+1 .. Key_And_Value'LAST));
         begin
            --Put_Line("Key = <"& Key &">");
            --Put_Line("Value = <"& Value &">");
            if Self.Has(Key) then
               Self.Add_Value_To_Existing_Key(Key, Value);
            else
               Self.Insert(Key, Value);
            end if;
            --Self.Ref.Data.Insert(Key, Value);
         end;
      end if;
   end Parse_Line;

   -----------------------------------------------------------------------------
   procedure Add_Value_To_Existing_Key
      (Self  : in out Settings_Type;
       Key   : in     String;
       Value : in     String) is

      Value_List : String_Lists.Vector;
      Location : Cursor;

   begin
      --Put_Line("Adding <" & Value & "> to existing key <" & Key & ">");
      Location := Self.Ref.Data.Find(Key);
      if Location /= No_Element then
         Value_List := Element(Location);
         String_Lists.Append(Value_List, Value);
         Self.Ref.Data.Replace_Element(Location, Value_List);
      else
         --raise Constraint_Error; --TODO: define an exception and raise it with a message
         Raise_Exception(Constraint_Error'IDENTITY, "Error: key <" & Key & "> not found.");
      end if;
   end Add_Value_To_Existing_Key;

   -----------------------------------------------------------------------------
   procedure Insert
      (Self  : in out Settings_Type;
       Key   : in     String;
       Value : in     String) is

      Value_List : String_Lists.Vector;

   begin
      --Put_Line("Inserting <" & Value & "> at key <" & Key & ">");
      String_Lists.Append(Value_List, Value);
      Self.Ref.Data.Insert(Key, Value_List);
   end Insert;

   -----------------------------------------------------------------------------
   function Has(Self : Settings_Type; Key : String) return Boolean is
   begin
      return Self.Ref.Data.Contains(Key);
   end Has;

   -----------------------------------------------------------------------------
   function Lookup_As_String(Self : Settings_Type; Key : String; Index : Positive := 1) return String is
      Location : Cursor;
      Value_List : String_Lists.Vector;
   begin
      Location := Self.Ref.Data.Find(Key);
      if Location /= No_Element then
         Value_List := Element(Location);
         return String_Lists.Element(Value_List, Index);
      end if;
      return "";
   end Lookup_As_String;

   -----------------------------------------------------------------------------
   function Lookup_As_Integer(Self : Settings_Type; Key : String; Index : Positive := 1) return Integer is
      Image : constant String := Self.Lookup_As_String(Key, Index);
   begin
      return Integer'VALUE(Image);
   end Lookup_As_Integer;


   ----------------------------------------------------------------------------
   function Tuple_String_Count_Elements(Tuple_String : String) return Positive is
   begin
      return Ada.Strings.Fixed.Count(Tuple_String, ",") + 1; -- The number of delimiters plus one
   end Tuple_String_Count_Elements;

   ----------------------------------------------------------------------------
   function Tuple_String_Get(Tuple_String : String; Index : Positive) return String is
   begin
      return String_Ops.Nth(Tuple_String, Index, Ada.Strings.Maps.To_Set(','));
   end Tuple_String_Get;

   ----------------------------------------------------------------------------
   function Make_String_Register(Value : String) return kv.avm.Registers.Register_Type is
      Clean : constant String := String_Ops.Trim_One_From_Both_Ends(Value);
      use kv.avm.Registers;
   begin
      return (Format => Immutable_String, The_String => +Clean);
   end Make_String_Register;

   ----------------------------------------------------------------------------
   function Make_Intger_Value(Value : String) return kv.avm.Registers.Register_Type is
   begin
      return kv.avm.Registers.Make_S(Interfaces.Integer_64'VALUE(Value));
   end Make_Intger_Value;

   ----------------------------------------------------------------------------
   function Make_Register(Value : String) return kv.avm.Registers.Register_Type is
   begin
      if Value(Value'FIRST) = Ada.Characters.Latin_1.Quotation then
         return Make_String_Register(Value);
      else
         return Make_Intger_Value(Value);
      end if;
   end Make_Register;
      

   ----------------------------------------------------------------------------
   function String_To_Registers(Tuple_String : String) return kv.avm.Memories.Register_Set_Type is
      Clean : constant String := String_Ops.Trim_One_From_Both_Ends(Tuple_String); -- drop []
      Count : constant Positive := Tuple_String_Count_Elements(Clean);
      Answer : kv.avm.Memories.Register_Set_Type(0 .. Interfaces.Unsigned_32(Count - 1));
      use Interfaces;
   begin
      --Put_Line("String_To_Registers processing <" & Clean & "> (count =" & Positive'IMAGE(Count) & ")");
      for Index in Answer'RANGE loop
         Answer(Index) := Make_Register(Tuple_String_Get(Clean, Positive(Index + 1)));
      end loop;
      return Answer;
   end String_To_Registers;

   ----------------------------------------------------------------------------
   function Lookup_As_Tuple(Self : Settings_Type; Key : String; Index : Positive := 1) return kv.avm.Tuples.Tuple_Type is
      Answer : kv.avm.Tuples.Tuple_Type;
      Contents : kv.avm.Memories.Register_Array_Type;
   begin
      Contents.Initialize(String_To_Registers(Self.Lookup_As_String(Key, Index)));
      Answer.Fold(Contents);
      return Answer;
   end Lookup_As_Tuple;

   ----------------------------------------------------------------------------
   function Value_Count_For_Key(Self : Settings_Type; Key : String) return Natural is
      Location : Cursor;
      Value_List : String_Lists.Vector;
   begin
      Location := Self.Ref.Data.Find(Key);
      if Location /= No_Element then
         Value_List := Element(Location);
         return Natural(String_Lists.Length(Value_List));
      end if;
      return 0;
   end Value_Count_For_Key;


   ----------------------------------------------------------------------------
   package Settings_Reader is new kv.avm.File_Reader(Settings_Type);

   ----------------------------------------------------------------------------
   procedure Parse_Input_File
      (Self    : in out Settings_Type;
       File_In : in     String) renames Settings_Reader.Parse_Input_File;

end kv.avm.Ini;
