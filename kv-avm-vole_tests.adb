with Ada.Unchecked_Deallocation;

with AUnit.Assertions; use AUnit.Assertions;

with kv.avm.Log; use kv.avm.Log;
with kv.avm.Symbol_Tables;
with kv.avm.Registers; use kv.avm.Registers;

with kv.avm.Tree_Rewrite;
with kv.avm.vole_tree;

package body kv.avm.Vole_Tests is

   procedure Set_Up (T : in out Base_Test_Case) is
   begin
      T.Symbols.Initialize;
   end;
   procedure Tear_Down (T : in out Base_Test_Case) is begin null; end;

   ----------------------------------------------------------------------------
   function Name (T : Test_01) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test vole 01: Basic kv.avm.Symbol_Tables.");
   end Name;

   procedure Run_Test (T : in out Test_01) is
      S : constant String := "Fred";
   begin
      T.Symbols.Add(S);
      Assert(T.Symbols.Has(S), "Symbol that was just added is not in the collection");
      Assert(T.Symbols.Get_Index(S) = 0, "First auto-inxed is not zero");
      Assert(T.Symbols.Get_Kind(S) = kv.avm.Registers.Unset, "Default kind does not the expected value");
   end Run_Test;



   ----------------------------------------------------------------------------
   function Name (T : Test_02) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test vole 02: Multi kv.avm.Symbol_Tables.");
   end Name;

   package Symbol_Names is
      -- These have to be in alphabetical order for the tests to pass (while still using a map).
      A : constant String := "a_symbol";
      B : constant String := "b_symbol";
      C : constant String := "c_symbol";
      D : constant String := "d_symbol";
   end Symbol_Names;

   procedure Run_Test (T : in out Test_02) is
      use Symbol_Names;
   begin
      T.Symbols.Add(A);
      T.Symbols.Add(B);
      T.Symbols.Add(C);
      Assert(T.Symbols.Count = 3, "Wrong count");
      Assert(T.Symbols.Has(A), "Symbol A missing");
      Assert(T.Symbols.Has(B), "Symbol B missing");
      Assert(T.Symbols.Has(C), "Symbol C missing");
      Assert(T.Symbols.Get_Index(C) = 2, "Third auto-inxed is not two");
      T.Symbols.Set_All_Indexes;
      Assert(T.Symbols.Get_Index(C) = 3, "Third updated-inxed is not three");
      T.Symbols.Set_All_Indexes(5);
      Assert(T.Symbols.Get_Index(C) = 3+5-1, "Third updated-inxed is not now 7");
   end Run_Test;



   ----------------------------------------------------------------------------
   function Name (T : Test_03) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test vole 03: Update kv.avm.Symbol_Tables.");
   end Name;

   procedure Run_Test (T : in out Test_03) is
      use Symbol_Names;
   begin
      T.Symbols.Add(A);
      T.Symbols.Add(B);
      T.Symbols.Add(C);
      T.Symbols.Set_Kind(B, kv.avm.Registers.Tuple);
      Assert(T.Symbols.Get_Kind(B) = kv.avm.Registers.Tuple, "Updated kind does not the expected value");
   end Run_Test;



   procedure Check_Name_Index(Name : in String; Indx : in Natural) is
      use Symbol_Names;
   begin
      case Indx is
         when 0 =>
            Assert(Name = A, "Wrong symbol name in position 1, expected '" & A & "', got '" & Name & "'.");
         when 1 =>
            Assert(Name = B, "Wrong symbol name in position 2, expected '" & B & "', got '" & Name & "'.");
         when 2 =>
            Assert(Name = C, "Wrong symbol name in position 3, expected '" & C & "', got '" & Name & "'.");
         when 3 =>
            Assert(Name = D, "Wrong symbol name in position 4, expected '" & D & "', got '" & Name & "'.");
         when others =>
            Assert(False, "Invalid index");
      end case;
   end Check_Name_Index;


   ----------------------------------------------------------------------------
   function Name (T : Test_04) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test vole 04: For_Each kv.avm.Symbol_Tables.");
   end Name;

   procedure Run_Test (T : in out Test_04) is
      use Symbol_Names;
      Current : Natural := 0;
      procedure Check(Name : in String; Kind : in kv.avm.Registers.Data_Kind; Indx : in Natural; Init : in String) is
      begin
         Assert(Indx = Current, "Wrong index");
         case Current is
         when 0 =>
            Assert(Name = A, "Wrong symbol name in position 1");
         when 1 =>
            Assert(Kind = Tuple, "Wrong kind in position 2");
         when 2 =>
            Assert(Init = "omega", "Wrong init in position 3");
         when others =>
            null;
         end case;
         Check_Name_Index(Name, Indx);
         Current := Current + 1;
      end Check;
   begin
      T.Symbols.Add(A);
      T.Symbols.Add(B);
      T.Symbols.Add(C);
      T.Symbols.Set_Kind(B, kv.avm.Registers.Tuple);
      T.Symbols.Set_Init(C, "omega");
      T.Symbols.For_Each(Check'ACCESS);
   end Run_Test;



   ----------------------------------------------------------------------------
   function Name (T : Test_05) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test vole 05: Negative kv.avm.Symbol_Tables.");
   end Name;

   procedure Run_Test (T : in out Test_05) is
      Kind : kv.avm.Registers.Data_Kind;
      Index : Natural;
   begin
      Assert(not T.Symbols.Has("Foo"), "Has lied");
      begin
         Kind := T.Symbols.Get_Kind("Foo");
         Assert(False, "Failed to raise exception when accessing missing symbol");
      exception
         when kv.avm.Symbol_Tables.Missing_Element_Error =>
            null;
         when others =>
            Assert(True, "Unexpected exception raised");
      end;
      begin
         Index := T.Symbols.Get_Index("Foo");
         Assert(False, "Failed to raise exception when accessing missing symbol");
      exception
         when kv.avm.Symbol_Tables.Missing_Element_Error =>
            null;
         when others =>
            Assert(True, "Unexpected exception raised");
      end;
   end Run_Test;



   ----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(kv.avm.Symbol_Tables.Symbol_Table, kv.avm.Symbol_Tables.Symbol_Table_Access);


   ----------------------------------------------------------------------------
   function Name (T : Test_06) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test vole 06: Link_Superclass_Table.");
   end Name;

   procedure Run_Test (T : in out Test_06) is
      Super_Symbols : kv.avm.Symbol_Tables.Symbol_Table_Access;
      use Symbol_Names;

      Count : Integer := 0;
      procedure Check(Name : in String; Kind : in kv.avm.Registers.Data_Kind; Indx : in Natural; Init : in String) is
      begin
         Count := Count + 1;
         Check_Name_Index(Name, Indx);
      end Check;

   begin
      Super_Symbols := new kv.avm.Symbol_Tables.Symbol_Table;
      Super_Symbols.Initialize;

      T.Symbols.Link_Superclass_Table(Super_Symbols);

      Super_Symbols.Add(A);
      Super_Symbols.Add(B);
      Super_Symbols.Set_All_Indexes(0);
      T.Symbols.Add(C);
      T.Symbols.Add(D);
      T.Symbols.Set_All_Indexes(Super_Symbols.Count);

      T.Symbols.For_Each(Check'ACCESS);

      Assert(Count = 4, "Wrong number of symbols in the combined table, expected 4, got " & Integer'IMAGE(Count));

      Free(Super_Symbols);
   end Run_Test;



   ----------------------------------------------------------------------------
   function Name (T : Test_07) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test vole 07: rewrite a subclass node.");
   end Name;

   procedure Run_Test (T : in out Test_07) is
      use kv.avm.vole_tree;
      use Symbol_Names;
      Rewriter  : aliased kv.avm.Tree_Rewrite.Rewriter_Class;
      SubName_NP : kv.avm.vole_tree.Node_Pointer;
      SuperName_NP : kv.avm.vole_tree.Node_Pointer;
      Subclass_NP : kv.avm.vole_tree.Node_Pointer;
      Superclass_NP : kv.avm.vole_tree.Node_Pointer;
      Program_NP : kv.avm.vole_tree.Node_Pointer;
      Program_PP : Program_Pointer;
      Table : access kv.avm.Symbol_Tables.Symbol_Table;
   begin
      kv.avm.Log.Verbose := True;
      Put_Line("test vole 07");
      -- kv.avm.vole_tree uses a singleton to store the parsed program
      Build_Id_Node(SubName_NP, 1, "SubClass");
      Assert(SubName_NP /= null, "SubName_NP is null");
      Build_Id_Node(SuperName_NP, 2, "SuperClass");
      Build_Actor_Node(Subclass_NP, 3, SubName_NP, null, null, SuperName_NP);
      Table := Actor_Definition_Pointer(Subclass_NP).Get_Symbol_Table(CONSTANT_SYMBOLS);
      Table.Add(C);
      Table.Add(D);

      Table := Actor_Definition_Pointer(Subclass_NP).Get_Symbol_Table(VARIABLE_SYMBOLS);
      Table.Add(C);
      Table.Add(D);

      Build_Actor_Node(Superclass_NP, 4, SuperName_NP, null, null);
      Add_Next(Superclass_NP, Subclass_NP);
      Table := Actor_Definition_Pointer(Superclass_NP).Get_Symbol_Table(CONSTANT_SYMBOLS);
      Table.Add(A);
      Table.Add(B);

      Table := Actor_Definition_Pointer(Superclass_NP).Get_Symbol_Table(VARIABLE_SYMBOLS);
      Table.Add(A);
      Table.Add(B);

      Build_Program(Program_NP, 5, null, Superclass_NP);
      Save_Program(Program_NP);
      Assert(Program_NP /= null, "Program_NP is null");
      Program_PP := Program_Pointer(Program_NP);

      Rewriter.Init;
      Put_Line("visit...");
      Program_PP.Visit(Rewriter'ACCESS, 0);
      Rewriter.Finalize;

      Table := Actor_Definition_Pointer(Superclass_NP).Get_Symbol_Table(CONSTANT_SYMBOLS);
      Assert(Table.Get_Index(A) = 0, "Super Constant A has wrong index, expected 0, got " & Natural'IMAGE(Table.Get_Index(A)));

      Table := Actor_Definition_Pointer(Superclass_NP).Get_Symbol_Table(VARIABLE_SYMBOLS);
      Assert(Table.Get_Index(B) = 1, "Super Attribute B has wrong index, expected 1, got " & Natural'IMAGE(Table.Get_Index(B)));

      Table := Actor_Definition_Pointer(Subclass_NP).Get_Symbol_Table(CONSTANT_SYMBOLS);
      Assert(Table.Get_Index(A) = 0, "Sub Constant A has wrong index, expected 0, got " & Natural'IMAGE(Table.Get_Index(A)));
      Assert(Table.Get_Index(B) = 1, "Sub Constant B has wrong index, expected 1, got " & Natural'IMAGE(Table.Get_Index(B)));
      Assert(Table.Get_Index(C) = 2, "Sub Constant C has wrong index, expected 2, got " & Natural'IMAGE(Table.Get_Index(C)));
      Assert(Table.Get_Index(D) = 3, "Sub Constant D has wrong index, expected 3, got " & Natural'IMAGE(Table.Get_Index(D)));

      Table := Actor_Definition_Pointer(Subclass_NP).Get_Symbol_Table(VARIABLE_SYMBOLS);
      Assert(Table.Get_Index(A) = 0, "Sub Attribute A has wrong index, expected 0, got " & Natural'IMAGE(Table.Get_Index(A)));
      Assert(Table.Get_Index(B) = 1, "Sub Attribute B has wrong index, expected 1, got " & Natural'IMAGE(Table.Get_Index(B)));
      Assert(Table.Get_Index(C) = 2, "Sub Attribute C has wrong index, expected 2, got " & Natural'IMAGE(Table.Get_Index(C)));
      Assert(Table.Get_Index(D) = 3, "Sub Attribute D has wrong index, expected 3, got " & Natural'IMAGE(Table.Get_Index(D)));
   end Run_Test;



   ----------------------------------------------------------------------------
   function Name (T : Test_08) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("test vole 08: XXX.");
   end Name;

   procedure Run_Test (T : in out Test_08) is
   begin
      null;
   end Run_Test;



end kv.avm.Vole_Tests;
