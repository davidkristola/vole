package kv.avm.Log is

   Verbose : Boolean := False;

   procedure Put(Str : String);
   procedure Put_Line(Str : String);
   procedure Log_If(Callback : access function return String);
   procedure Put_Error(Str : String);
   procedure New_Line(Count : Positive := 1);

   function Get_Last_Log_Line return String;
   function Get_Last_Error_Line return String;

end kv.avm.Log;
