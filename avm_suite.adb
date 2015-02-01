with kv.avm.Test;
with kv.avm.Asm_Tests;
with kv.avm.Comm_Tests;
with kv.avm.Vole_Tests;

package body avm_suite is

   function Suite return Access_Test_Suite is
      Answer : constant Access_Test_Suite := new Test_Suite;
   begin
      Answer.Add_Test(new kv.avm.Test.Test_1);
      Answer.Add_Test(new kv.avm.Test.Test_2);
      Answer.Add_Test(new kv.avm.Test.Test_3);
      Answer.Add_Test(new kv.avm.Test.Test_4);
      Answer.Add_Test(new kv.avm.Test.Test_5);
      Answer.Add_Test(new kv.avm.Test.Test_6);
      Answer.Add_Test(new kv.avm.Test.Test_6b);
      Answer.Add_Test(new kv.avm.Test.Test_7);
      Answer.Add_Test(new kv.avm.Test.Test_8);
      Answer.Add_Test(new kv.avm.Test.Test_9);
      Answer.Add_Test(new kv.avm.Test.Test_9b);
      Answer.Add_Test(new kv.avm.Test.Test_9c);
      Answer.Add_Test(new kv.avm.Test.Test_9d);
      Answer.Add_Test(new kv.avm.Test.Test_10);
      Answer.Add_Test(new kv.avm.Test.Test_11);
      Answer.Add_Test(new kv.avm.Test.Test_12);
      Answer.Add_Test(new kv.avm.Test.Test_13);
      Answer.Add_Test(new kv.avm.Test.Test_14);
      Answer.Add_Test(new kv.avm.Test.Test_15);
      Answer.Add_Test(new kv.avm.Test.Test_16);
      Answer.Add_Test(new kv.avm.Test.Test_17);
      Answer.Add_Test(new kv.avm.Test.Test_18);
      Answer.Add_Test(new kv.avm.Test.Test_19);
      Answer.Add_Test(new kv.avm.Test.Test_20);
      Answer.Add_Test(new kv.avm.Test.Test_21);
      Answer.Add_Test(new kv.avm.Test.Test_22);
      Answer.Add_Test(new kv.avm.Test.Test_23);
      Answer.Add_Test(new kv.avm.Test.Test_24);
      Answer.Add_Test(new kv.avm.Test.Test_25);
      Answer.Add_Test(new kv.avm.Test.Test_26);

      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A1);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A2);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A3);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A4);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A5);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A6);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A7);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A8);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A9);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A10);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A11);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A12);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A13);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A14);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A15);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A16);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A17);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A18);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A19);
      Answer.Add_Test(new kv.avm.Asm_Tests.Test_A20);

      Answer.Add_Test(new kv.avm.Comm_Tests.Test_01);
      Answer.Add_Test(new kv.avm.Comm_Tests.Test_02);
      Answer.Add_Test(new kv.avm.Comm_Tests.Test_03);
      Answer.Add_Test(new kv.avm.Comm_Tests.Test_04);
      Answer.Add_Test(new kv.avm.Comm_Tests.Test_05);
      Answer.Add_Test(new kv.avm.Comm_Tests.Test_06);
      Answer.Add_Test(new kv.avm.Comm_Tests.Test_07);
      Answer.Add_Test(new kv.avm.Comm_Tests.Test_08);
      Answer.Add_Test(new kv.avm.Comm_Tests.Test_09);

      Answer.Add_Test(new kv.avm.Vole_Tests.Test_01);
      Answer.Add_Test(new kv.avm.Vole_Tests.Test_02);
      Answer.Add_Test(new kv.avm.Vole_Tests.Test_03);
      Answer.Add_Test(new kv.avm.Vole_Tests.Test_04);
      Answer.Add_Test(new kv.avm.Vole_Tests.Test_05);
      Answer.Add_Test(new kv.avm.Vole_Tests.Test_06);
      Answer.Add_Test(new kv.avm.Vole_Tests.Test_07);
      Answer.Add_Test(new kv.avm.Vole_Tests.Test_08);

      return Answer;
   end Suite;

end avm_suite;
