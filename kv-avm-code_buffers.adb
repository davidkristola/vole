with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body kv.avm.Code_Buffers is

   function "+"(S : String) return String_Type renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+"(U : String_Type) return String renames Ada.Strings.Unbounded.To_String;


   procedure Put_Meta
      (Self : in out Buffer_Type;
       Data : in     String) is
   begin
      Self.Count := Self.Count + 1;
      Self.Buffer(Self.Count) := +Data;
   end Put_Meta;

   procedure Put_Instruction
      (Self : in out Buffer_Type;
       Data : in     String) is
   begin
      Self.Code := Self.Code + 1;
      Self.Count := Self.Count + 1;
      Self.Buffer(Self.Count) := +Data;
   end Put_Instruction;

   procedure Put_Comment
      (Self : in out Buffer_Type;
       Data : in     String) is
   begin
      Self.Count := Self.Count + 1;
      Self.Buffer(Self.Count) := +Data;
   end Put_Comment;

   procedure Append
      (Self : in out Buffer_Type;
       Data : in     Buffer_Type) is
   begin
      for I in 1 .. Data.Count loop
         Self.Count := Self.Count + 1;
         Self.Buffer(Self.Count) := Data.Buffer(I);
      end loop;
      Self.Code := Self.Code + Data.Code;
   end Append;

   function Code_Count(Self : Buffer_Type) return Natural is
   begin
      return Self.Code;
   end Code_Count;

   procedure Print
      (Self : in     Buffer_Type) is
   begin
      for I in 1 .. Self.Count loop
         Ada.Text_IO.Put_Line(+Self.Buffer(I));
      end loop;
   end Print;

   procedure Process_Lines
      (Self      : in     Buffer_Type;
       Processor : access procedure(Line : String)) is
   begin
      for I in 1 .. Self.Count loop
         Processor.all(+Self.Buffer(I));
      end loop;
   end Process_Lines;

end kv.avm.Code_Buffers;
