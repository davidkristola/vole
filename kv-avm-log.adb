with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body kv.avm.Log is

   Last_Log_Line : Ada.Strings.Unbounded.Unbounded_String;
   Last_Error_Line : Ada.Strings.Unbounded.Unbounded_String;

   procedure Put(Str : String) is
   begin
      if Verbose then
         Ada.Text_IO.Put(Str);
      end if;
   end Put;

   procedure Put_Line(Str : String) is
   begin
      Last_Log_Line := Ada.Strings.Unbounded.To_Unbounded_String(Str);
      if Verbose then
         Ada.Text_IO.Put_Line(Str);
      end if;
   end Put_Line;

   procedure Log_If(Callback : access function return String) is
   begin
      if Verbose then
         Ada.Text_IO.Put_Line(Callback.all);
      end if;
   end Log_If;

   procedure Put_Error(Str : String) is
   begin
      Last_Error_Line := Ada.Strings.Unbounded.To_Unbounded_String(Str);
      Ada.Text_IO.Put_Line(Str);
   end Put_Error;

   procedure New_Line(Count : Positive := 1) is
   begin
      if Verbose then
         Ada.Text_IO.New_Line(Ada.Text_Io.Count(Count));
      end if;
   end New_Line;

   function Get_Last_Log_Line return String is
   begin
      return Ada.Strings.Unbounded.To_String(Last_Log_Line);
   end Get_Last_Log_Line;

   function Get_Last_Error_Line return String is
   begin
      return Ada.Strings.Unbounded.To_String(Last_Error_Line);
   end Get_Last_Error_Line;

end kv.avm.Log;
