with Ada.Streams.Stream_IO;

with String_Ops;

with kv.avm.References;
with kv.avm.Instructions;
with kv.avm.Registers;
with kv.avm.Memories;
with kv.avm.Actors;
with kv.avm.Line_Parser;

package kv.avm.Assemblers is

   Word_Code_Identifier_Number : constant Integer := -1348;
   Word_Code_Human_Id : constant String(1..8) := "volecode";

   Invalid_Word_Code_Header : exception;
   Invalid_Op_Code : exception;
   Unrecognized_Type_Error : exception;

   function Make_Word_Code_Name(Asm_Name : String) return String;

   function Decode_Op_Code(Line : String) return kv.avm.Instructions.Instruction_Type;




   type Assembler_Type;
   type Assembler_Access is access all Assembler_Type;

   type Actor_Type;
   type Actor_Pointer is access all Actor_Type;

   type Message_Type;
   type Message_Access is access all Message_Type;


   type Message_Type is tagged
      record
         Name  : String_Ops.String_Pointer_Type;
         Count : Natural;
         Code  : kv.avm.Instructions.Code_Type(0..512);
         Has_P : Boolean := False;
         Pred  : kv.avm.References.Offset_Type;
         Next  : Message_Access;
      end record;

   procedure Initialize
      (Self  : access Message_Type;
       Name  : in     String;
       Next  : in     Message_Access);

   -- The line must be cleaned up before being parsed here.
   procedure Parse_Line
      (Self : in out Message_Type;
       Line : in     String);

   function Constructor_Code(Self : Message_Type) return kv.avm.Instructions.Code_Access;

   procedure Add_Non_Constructors
      (Self  : in out Message_Type;
       Actor : in     kv.avm.Actors.Actor_Access);

   procedure Write_Word_Code
      (Self : in out Message_Type;
       File : in     Ada.Streams.Stream_IO.Stream_Access);

   function New_From_Stream(Count : Natural; Stream : Ada.Streams.Stream_IO.Stream_Access) return Message_Access;



   type Actor_Section_Type is (Constants_Section, Message_Section);

   type Actor_Type is tagged
      record
         Name    : String_Ops.String_Pointer_Type;
         Parent  : String_Ops.String_Pointer_Type;
         Section : Actor_Section_Type;
         Fixed   : kv.avm.Memories.Register_Set_Type(0..63);
         F_Max   : Natural;
         M_Count : Natural;
         Message : Message_Access;
         Loaded  : kv.avm.Actors.Actor_Access;
         Next    : Actor_Pointer;
      end record;

   procedure Initialize
      (Self  : access Actor_Type;
       Name  : in     String;
       Next  : in     Actor_Pointer);

   -- The line must be cleaned up before being parsed here.
   procedure Parse_Line
      (Self : in out Actor_Type;
       Line : in     String);

   procedure Load
      (Self : in out Actor_Type);

   procedure Link
      (Self : in out Actor_Type);

   procedure Write_Word_Code
      (Self : in out Actor_Type;
       File : in     Ada.Streams.Stream_IO.Stream_Access);

   function New_From_Stream(Count : Natural; Stream : Ada.Streams.Stream_IO.Stream_Access) return Actor_Pointer;




   type Assembler_Type is new kv.avm.Line_Parser.Parse_Line_Interface with --tagged
      record
         Actor_Count : Natural;
         Lines_Parsed : Natural;
         Actor : Actor_Pointer;
      end record;

   procedure Initialize
      (Self : access Assembler_Type);

   overriding procedure Parse_Line
      (Self : in out Assembler_Type;
       Line : in     String);

   procedure Parse_Input_File
      (Self    : in out Assembler_Type;
       File_In : in     String);

   procedure Transfer_Actors_To_Store
      (Self : in out Assembler_Type);

   procedure Write_Word_Code
      (Self : in out Assembler_Type;
       File : in     Ada.Streams.Stream_IO.Stream_Access);

   procedure Read_Word_Code
      (Self : in out Assembler_Type;
       File : in     Ada.Streams.Stream_IO.Stream_Access);

end kv.avm.Assemblers;
