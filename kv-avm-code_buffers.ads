with Ada.Strings.Unbounded;

package kv.avm.Code_Buffers is

   type Buffer_Type is tagged private;

   procedure Put_Meta
      (Self : in out Buffer_Type;
       Data : in     String);

   procedure Put_Instruction
      (Self : in out Buffer_Type;
       Data : in     String);

   procedure Put_Comment
      (Self : in out Buffer_Type;
       Data : in     String);

   procedure Append
      (Self : in out Buffer_Type;
       Data : in     Buffer_Type);

   function Code_Count(Self : Buffer_Type) return Natural;

   procedure Print
      (Self : in     Buffer_Type);

   procedure Process_Lines
      (Self      : in     Buffer_Type;
       Processor : access procedure(Line : String));

private

   subtype String_Type is Ada.Strings.Unbounded.Unbounded_String;
   type String_Array is array (1..1024) of String_Type;

   type Buffer_Type is tagged
      record
         Buffer : String_Array;
         Count  : Natural := 0;
         Code   : Natural := 0;
      end record;

end kv.avm.Code_Buffers;
