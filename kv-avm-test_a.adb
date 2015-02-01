with Ada.Text_IO;
use Ada.Text_IO;

with kv.avm.Instructions;

procedure kv.avm.test_a is
   x : instruction.overlay_type := (instruction.directive, instruction.no_op, false, false, 0);
begin
   Put_Line("Hello there.");
end kv.avm.test_a;
