with AUnit.Reporter.Text;
with AUnit.Run;
with avm_suite; use avm_suite;

procedure test_avm is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end test_avm;
