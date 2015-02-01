with Ada.Unchecked_Deallocation;

with kv.avm.Tuples;

package body kv.avm.Memories is

   use Interfaces;
   use kv.avm.Registers;

   Last_Used_ID : Natural := 0;

   type Register_Array_Reference_Counter_Type is
      record
         ID     : Positive;
         Count  : Natural := 0;
         Length : Natural := 0; -- Empty arrays are allowed in which case Data will be null.
         Data   : Register_Set_Access;
      end record;

   procedure Free is new Ada.Unchecked_Deallocation(Register_Set_Type, Register_Set_Access);
   procedure Free is new Ada.Unchecked_Deallocation(Register_Array_Reference_Counter_Type, Register_Array_Reference_Counter_Access);


   -----------------------------------------------------------------------------
   procedure Initialize (Self : in out Register_Array_Type) is
   begin
      Self.Ref := new Register_Array_Reference_Counter_Type;
      Self.Ref.Count := 1;
      Self.Ref.Length := 0;
      Last_Used_ID := Last_Used_ID + 1;
      Self.Ref.ID := Last_Used_ID;
      --Put_Line("RAT: Just created " & Positive'IMAGE(Self.Ref.ID));
   end Initialize;

   -----------------------------------------------------------------------------
   procedure Adjust     (Self : in out Register_Array_Type) is
      Ref : Register_Array_Reference_Counter_Access := Self.Ref;
   begin
      if Ref /= null then
         --Put_Line("RAT: Adjust " & Positive'IMAGE(Self.Ref.ID));
         Ref.Count := Ref.Count + 1;
      end if;
   end Adjust;

   -----------------------------------------------------------------------------
   procedure Finalize   (Self : in out Register_Array_Type) is
      Ref : Register_Array_Reference_Counter_Access := Self.Ref;
   begin
      Self.Ref := null;
      if Ref /= null then
         --Put_Line("RAT: Finalize " & Positive'IMAGE(Ref.ID) & ", count was " & Natural'IMAGE(Ref.Count));
         Ref.Count := Ref.Count - 1;
         if Ref.Count = 0 then
            if Ref.Length /= 0 then
               Free(Ref.Data);
            end if;
            Free(Ref);
         end if;
      end if;
   end Finalize;




   -----------------------------------------------------------------------------
   procedure Initialize(Self : in out Register_Array_Type; Data : Register_Set_Type) is
   begin
      if Self.Ref /= null then
         --Put_Line("RAT: reusing " & Positive'IMAGE(Self.Ref.ID) & " init w/set");
         Self.Finalize; -- Clean up the previous entity
      end if;
      Self.Initialize;
      Self.Ref.Length := Data'LENGTH;
      if Self.Ref.Length /= 0 then
         Self.Ref.Data := new Register_Set_Type(0 .. Interfaces.Unsigned_32(Self.Ref.Length) - 1);
         Self.Ref.Data.all := Data;
      end if;
      Self.Ref.Count := 1;
   end Initialize;


   -----------------------------------------------------------------------------
   procedure Initialize(Self : in out Register_Array_Type; Tuple : kv.avm.Tuples.Tuple_Type) is
      Data : access constant Register_Set_Type := Tuple.Unfolded;
   begin
      if Self.Ref /= null then
         --Put_Line("RAT: reusing " & Positive'IMAGE(Self.Ref.ID) & " init w/tuple");
         Self.Finalize; -- Clean up the previous entity
      end if;
      Self.Initialize(Data.all);
   end Initialize;


   -----------------------------------------------------------------------------
   function Is_Set(Self : Register_Array_Type) return Boolean is
   begin
      return Self.Ref /= null and then Self.Ref.Data /= null;
   end Is_Set;


   -----------------------------------------------------------------------------
   function Get(Self : Register_Array_Type) return Register_Set_Access is
   begin
      return Self.Ref.Data;
   end Get;


   -----------------------------------------------------------------------------
   procedure Set(Self : in out Register_Array_Type; Registers : in Register_Set_Access) is
   begin
      if Self.Ref /= null then
         --Put_Line("RAT: reusing" & Positive'IMAGE(Self.Ref.ID) & " set");
         Self.Finalize; -- Clean up the previous entity
      end if;
      Self.Initialize;
      if Registers /= null then
         Self.Ref.Length := Registers.all'LENGTH;
      end if;
      if Self.Ref.Length /= 0 then
         Self.Ref.Data := new kv.avm.Memories.Register_Set_Type(0 .. Registers.all'LENGTH - 1);
         Self.Ref.Data.all := Registers.all;
      end if;
      Self.Ref.Count := 1;
   end Set;


   -----------------------------------------------------------------------------
   procedure Write(Self : in out Register_Array_Type; Where : kv.avm.References.Offset_Type; Data : kv.avm.Registers.Register_Type) is
   begin
      Self.Ref.Data(Interfaces.Unsigned_32(Where)) := Data;
   end Write;


   -----------------------------------------------------------------------------
   function Read(Self : Register_Array_Type; Where : kv.avm.References.Offset_Type) return kv.avm.Registers.Register_Type is
   begin
      return Self.Ref.Data(Interfaces.Unsigned_32(Where));
   end Read;


   -----------------------------------------------------------------------------
   procedure Allocate(Self : in out Register_Array_Type; Count : in Positive) is
   begin
      if Self.Ref /= null then
         --Put_Line("RAT: reusing" & Positive'IMAGE(Self.Ref.ID) & " allocate");
         Self.Finalize; -- Clean up the previous entity
      end if;
      Self.Initialize;
      Self.Ref.Length := Count;
      if Self.Ref.Length /= 0 then
         Self.Ref.Data := new Register_Set_Type(0 .. Interfaces.Unsigned_32(Count) - 1);
      end if;
      Self.Ref.Count := 1;
   end Allocate;


   -----------------------------------------------------------------------------
   procedure Deallocate(Self : in out Register_Array_Type) is
   begin
      Self.Finalize;
   end Deallocate;


   -----------------------------------------------------------------------------
   procedure Find_Future
      (Self     : in     Register_Array_Type;
       Future   : in     Interfaces.Unsigned_32;
       Found    :    out Boolean;
       Location :    out kv.avm.References.Offset_Type) is

   begin
      for Index in Self.Ref.Data'RANGE loop
         if Self.Ref.Data(Index).Format = kv.avm.Registers.Future then
            if Self.Ref.Data(Index).ID = Future then
               Found := True;
               Location := kv.avm.References.Offset_Type(Index);
               return;
            end if;
         end if;
      end loop;
      Found := False;
   end Find_Future;


   -----------------------------------------------------------------------------
   function Reachable(Registers : Register_Set_Access) return kv.avm.Actor_References.Sets.Set is
      Can_Reach : kv.avm.Actor_References.Sets.Set := kv.avm.Actor_References.Sets.Empty_Set;
   begin
      if Registers /= null then
         for Index in Registers'RANGE loop
            if Registers(Index).Format = kv.avm.Registers.Actor_Reference then
               Can_Reach.Include(Registers(Index).Instance);
            elsif Registers(Index).Format = kv.avm.Registers.Tuple then
               Can_Reach.Union(Registers(Index).Folded_Tuple.Reachable);
            end if;
         end loop;
      end if;
      return Can_Reach;
   end Reachable;


   -----------------------------------------------------------------------------
   function Reachable(Self : Register_Array_Type) return kv.avm.Actor_References.Sets.Set is
   begin
      if Self.Ref /= null then
         return Reachable(Self.Ref.Data);
      else
         return kv.avm.Actor_References.Sets.Empty_Set;
      end if;
   end Reachable;









   -----------------------------------------------------------------------------
   procedure Set(Self : in out Memory_Type; Bank : kv.avm.references.Register_Bank_Type; Registers : Register_Array_Type'CLASS) is
   begin
      Self.vq(Bank) := Register_Array_Type(Registers);
   end Set;


   -----------------------------------------------------------------------------
   function Get(Self : Memory_Type; Bank : kv.avm.references.Register_Bank_Type) return Register_Array_Type'CLASS is
   begin
      return Self.vq(Bank);
   end Get;


   -----------------------------------------------------------------------------
   procedure Write(Self : in out Memory_Type; Where : kv.avm.References.Reference_Type; Data : kv.avm.Registers.Register_Type) is
   begin
      Self.vq(Where.Memory).Write(Where.Index, Data);
   end Write;


   -----------------------------------------------------------------------------
   function Read(Self : Memory_Type; Where : kv.avm.References.Reference_Type) return kv.avm.Registers.Register_Type is
   begin
      return Self.vq(Where.Memory).Read(Where.Index);
   end Read;



   -----------------------------------------------------------------------------
   procedure Find_Future
      (Self     : in     Memory_Type;
       Bank     : in     kv.avm.References.Register_Bank_Type;
       Future   : in     Interfaces.Unsigned_32;
       Found    :    out Boolean;
       Location :    out kv.avm.References.Reference_Type) is

      Index : kv.avm.References.Offset_Type;

   begin
      Self.vq(Bank).Find_Future(Future, Found, Index);
      if Found then
         Location := (Memory => Bank, Index => Index);
      end if;
   end Find_Future;



   -----------------------------------------------------------------------------
   function Reachable(Self : Memory_Type; Bank : kv.avm.References.Register_Bank_Type) return kv.avm.Actor_References.Sets.Set is
   begin
      return Self.vq(Bank).Reachable;
   end Reachable;


   -----------------------------------------------------------------------------
   procedure Deallocate(Self : in out Memory_Type) is
   begin
      for Bank in Self.vq'RANGE loop
         Self.vq(Bank).Deallocate;
      end loop;
   end Deallocate;



   -----------------------------------------------------------------------------
   function To_String(Registers : Register_Set_Type) return String is
      Index_First : constant Interfaces.Unsigned_32 := Registers'FIRST;
      Index_Last  : constant Interfaces.Unsigned_32 := Registers'LAST;
   begin
      if Registers'LENGTH = 1 then
         return kv.avm.Registers.Reg_Img(Registers(Index_First));
      else
         return kv.avm.Registers.Reg_Img(Registers(Index_First)) & To_String(Registers(Index_First+1..Index_Last));
      end if;
   end To_String;


   -----------------------------------------------------------------------------
   function Dereferenced_Register_Image(Ref : kv.avm.references.reference_type; M : Memory_Type) return String is
      V   : constant Register_Type := M.Read(Ref);
      Img : constant String := Reg_Img(V);
   begin
      return "(="&Img&")";
   exception
      when others =>
         return "(=ERROR)";
   end Dereferenced_Register_Image;

   -----------------------------------------------------------------------------
   function To_String(Ref : kv.avm.references.reference_type; M : Memory_Type) return String is
   begin
      return kv.avm.references.Ref_Img(Ref) & Dereferenced_Register_Image(Ref, M);
   end To_String;


   -----------------------------------------------------------------------------
   function Instruction_Image(Inst : kv.avm.Instructions.Instruction_Type; M : Memory_Type) return String is
      use kv.avm.Instructions;
      use kv.avm.References;
   begin
      case Inst.op_code is
         when format_0_type =>
            return op_code_type'IMAGE(Inst.op_code);
         when format_1_type =>
            return op_code_type'IMAGE(Inst.op_code) & " value=" & To_String(Inst.value, M);
         when format_1b_type =>
            return op_code_type'IMAGE(Inst.op_code) & " jump=" & Interfaces.Unsigned_32'IMAGE(Inst.jump);
         when format_2_type =>
            return op_code_type'IMAGE(Inst.op_code) & " lhs=" & To_String(Inst.lhs, M) & " rhs=" & To_String(Inst.rhs, M);
         when format_2b_type =>
            return op_code_type'IMAGE(Inst.op_code) & " condition=" & To_String(Inst.condition, M) & " target=" & Interfaces.Unsigned_32'IMAGE(Inst.target);
         when format_3_type =>
            return op_code_type'IMAGE(Inst.op_code) & " a=" & To_String(Inst.a, M) & " x=" & To_String(Inst.x, M) & " y=" & To_String(Inst.y, M);
         when format_3b_type =>
            return op_code_type'IMAGE(Inst.op_code) & " lvalue=" & To_String(Inst.lvalue, M) & " tuple=" & To_String(Inst.tuple, M)& " index=" & offset_type'IMAGE(Inst.index);
         when format_4_type =>
            return op_code_type'IMAGE(Inst.op_code) & " result=" & To_String(Inst.result, M) & " left=" & To_String(Inst.left, M)& " action=" & operation_type'IMAGE(Inst.action) & " right=" & To_String(Inst.right, M);
         when format_5a1_type =>
            return op_code_type'IMAGE(Inst.op_code) &
                   " rply_5a1=" & To_String(Inst.rply_5a1, M) & " args_5a1=" & To_String(Inst.args_5a1, M) & " actr_5a1=" & To_String(Inst.actr_5a1, M) & " mdef_5a1=" & To_String(Inst.mdef_5a1, M);
         when format_5a2_type =>
            return op_code_type'IMAGE(Inst.op_code) &
                   " args_5a2=" & To_String(Inst.args_5a2, M) & " actr_5a2=" & To_String(Inst.actr_5a2, M) & " mdef_5a2=" & To_String(Inst.mdef_5a2, M);
         when format_5b1_type =>
            return op_code_type'IMAGE(Inst.op_code) &
                   " rply_5b1=" & To_String(Inst.rply_5b1, M) & " mdef_5b1=" & To_String(Inst.mdef_5b1, M) & " args_5b1=" & To_String(Inst.args_5b1, M);
         when format_5b2_type =>
            return op_code_type'IMAGE(Inst.op_code) &
                   " mdef_5b2=" & To_String(Inst.mdef_5b2, M) & " args_5b2=" & To_String(Inst.args_5b2, M);
         when others =>
            return "?";
      end case;
   end Instruction_Image;


end kv.avm.Memories;
