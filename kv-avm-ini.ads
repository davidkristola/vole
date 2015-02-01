with Ada.Finalization;

with kv.avm.Line_Parser;
with kv.avm.Tuples;

package kv.avm.Ini is

   type Settings_Type is new Ada.Finalization.Controlled and kv.avm.Line_Parser.Parse_Line_Interface with private;

   overriding procedure Initialize (Self : in out Settings_Type);
   overriding procedure Adjust     (Self : in out Settings_Type);
   overriding procedure Finalize   (Self : in out Settings_Type);

   overriding procedure Parse_Line
      (Self : in out Settings_Type;
       Line : in     String);

   function Has(Self : Settings_Type; Key : String) return Boolean;
   function Lookup_As_String(Self : Settings_Type; Key : String; Index : Positive := 1) return String;
   function Lookup_As_Integer(Self : Settings_Type; Key : String; Index : Positive := 1) return Integer;
   function Lookup_As_Tuple(Self : Settings_Type; Key : String; Index : Positive := 1) return kv.avm.Tuples.Tuple_Type;
   procedure Add_Value_To_Existing_Key
      (Self  : in out Settings_Type;
       Key   : in     String;
       Value : in     String);
   procedure Insert
      (Self  : in out Settings_Type;
       Key   : in     String;
       Value : in     String);
   function Value_Count_For_Key(Self : Settings_Type; Key : String) return Natural;

   procedure Parse_Input_File
      (Self    : in out Settings_Type;
       File_In : in     String);

private

   type Settings_Reference_Counter_Type;
   type Settings_Reference_Counter_Access is access all Settings_Reference_Counter_Type;

   type Settings_Type is new Ada.Finalization.Controlled and kv.avm.Line_Parser.Parse_Line_Interface with
      record
         Ref : Settings_Reference_Counter_Access;
      end record;

end kv.avm.Ini;
