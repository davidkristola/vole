with kv.avm.Line_Parser;

generic
   type Reader is new kv.avm.Line_Parser.Parse_Line_Interface with private;
package kv.avm.File_Reader is
   procedure Parse_Input_File
      (Self    : in out Reader;
       File_In : in     String);
end kv.avm.File_Reader;
