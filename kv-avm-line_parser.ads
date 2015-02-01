
package kv.avm.Line_Parser is

   type Parse_Line_Interface is interface;
   procedure Parse_Line
      (Self : in out Parse_Line_Interface;
       Line : in     String) is abstract;

end kv.avm.Line_Parser;
