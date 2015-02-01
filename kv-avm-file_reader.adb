with Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body kv.avm.File_Reader is
   procedure Parse_Input_File
      (Self    : in out Reader;
       File_In : in     String) is

      File   : Ada.Text_IO.File_Type;
      Buffer : String(1..1024);
      Length : Natural := 0;
      Line   : Natural := 0;

   begin
      Ada.Text_IO.Open(File, Ada.Text_IO.IN_FILE, File_In);
      while not Ada.Text_IO.End_Of_File(File) loop
         Ada.Text_IO.Get_Line(File, Buffer, Length);
         Line := Line + 1;
         Self.Parse_Line(Buffer(1..Length));
      end loop;
      Ada.Text_IO.Close(File);
   exception
      when Error: others =>
         Ada.Text_IO.Put_Line("EXCEPTION: "&Exception_Information(Error));
         Ada.Text_IO.Put_Line("File: "&File_In);
         Ada.Text_IO.Put_Line("Line: "&Natural'IMAGE(Line)); -- Ada.Text_IO.Line gives wrong answers.
         Ada.Text_IO.Put_Line("Code: "&Buffer(1..Length));
         Ada.Text_IO.Close(File);
         raise;
   end Parse_Input_File;
end kv.avm.File_Reader;
