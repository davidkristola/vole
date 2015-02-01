with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;

with kv.avm.Registers;
with kv.avm.Memories;

package body kv.avm.Tuples is

   use kv.avm.Registers; --!@#$ obe?
   use kv.avm.Memories;

   type Tuple_Reference_Counter_Type is
      record
         Count  : Natural := 0;
         Length : Natural := 0; -- Empty tuples are allowed in which case Data will be null.
         Data   : kv.avm.Memories.Register_Set_Access;
      end record;

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(kv.avm.Memories.Register_Set_Type, kv.avm.Memories.Register_Set_Access);

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Tuple_Reference_Counter_Type, Tuple_Reference_Counter_Access);



   type Map_Reference_Counter_Type is
      record
         Count  : Natural := 0;
         Length : Natural := 0; -- Length of Data (Data remains null if Length = 0; empty tuples are allowed)
         Data   : kv.avm.References.Reference_Array_Access;
      end record;

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(kv.avm.References.Reference_Array_Type, kv.avm.References.Reference_Array_Access);

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Map_Reference_Counter_Type, Map_Reference_Counter_Access);


   ----------------------------------------------------------------------------
   procedure Adjust
      (Self : in out Map_Type) is
   begin
      if Self.Ref /= null then
         Self.Ref.Count := Self.Ref.Count + 1;
      end if;
   end Adjust;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self : in out Map_Type) is
      Ref : Map_Reference_Counter_Access := Self.Ref;
   begin
      Self.Ref := null;
      if Ref /= null then
         Ref.Count := Ref.Count - 1;
         if Ref.Count = 0 then
            if Ref.Length /= 0 then
               Free(Ref.Data);
            end if;
            Free(Ref);
         end if;
      end if;
   end Finalize;

   ----------------------------------------------------------------------------
   procedure Set
      (Self : in out Map_Type;
       Data : access constant kv.avm.References.Reference_Array_Type) is
   begin
      if Self.Ref /= null then
         raise Immutability_Error;
      end if;
      Self.Ref := new Map_Reference_Counter_Type;
      Self.Ref.Length := Data.all'LENGTH;
      if Self.Ref.Length /= 0 then
         Self.Ref.Data := new kv.avm.References.Reference_Array_Type(0 .. Data.all'LENGTH - 1);
         Self.Ref.Data.all := Data.all;
      end if;
      Self.Ref.Count := Self.Ref.Count + 1;
   end Set;

   ----------------------------------------------------------------------------
   function Get(Self : Map_Type) return access constant kv.avm.References.Reference_Array_Type is
   begin
      if Self.Ref = null or else Self.Ref.Data = null then
         return null;
      end if;
      return Self.Ref.Data.all'ACCESS;
   end Get;


   ----------------------------------------------------------------------------
   procedure Tuple_Map_Write(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : in Map_Type) is
   begin
      Natural'OUTPUT(Stream, Item.Ref.Length);
      if Item.Ref.Length /= 0 then
         kv.avm.References.Reference_Array_Type'OUTPUT(Stream, Item.Ref.Data.all);
      end if;
   end Tuple_Map_Write;


   ----------------------------------------------------------------------------
   procedure Tuple_Map_Read(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : out Map_Type) is
      Length : Natural;
   begin
      Length := Natural'INPUT(Stream);
      if Length /= 0 then
         declare
            Data : aliased constant kv.avm.References.Reference_Array_Type := kv.avm.References.Reference_Array_Type'INPUT(Stream);
         begin
            Item.Set(Data'ACCESS);
         end;
      else
         -- Make an empty tuple map
         Item.Ref := new Map_Reference_Counter_Type;
         Item.Ref.Length := Length;
         Item.Ref.Count := 1;
      end if;
   end Tuple_Map_Read;


   ----------------------------------------------------------------------------
   function "="(L, R: Map_Type) return Boolean is
   begin
      if L.Ref = Null or R.Ref = Null then
         return False;
      end if;
      if L.Ref.Data = Null or R.Ref.Data = Null then
         return False;
      end if;
      return (L.Ref.Data.all = R.Ref.Data.all);
   end "=";






   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Tuple_Type) is
   begin
      -- Don't need to do anything here with Ref.
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Adjust
      (Self : in out Tuple_Type) is
   begin
      if Self.Ref /= null then
         Self.Ref.Count := Self.Ref.Count + 1;
      end if;
   end Adjust;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self : in out Tuple_Type) is
      Ref : Tuple_Reference_Counter_Access := Self.Ref;
   begin
      Self.Ref := null;
      if Ref /= null then
         Ref.Count := Ref.Count - 1;
         if Ref.Count = 0 then
            if Ref.Length /= 0 then
               Free(Ref.Data);
            end if;
            Free(Ref);
         end if;
      end if;
   end Finalize;




   ----------------------------------------------------------------------------
   procedure Fold
      (Self : in out Tuple_Type;
       Data : in     kv.avm.Memories.Register_Array_Type) is

      Registers : kv.avm.Memories.Register_Set_Access;

   begin
      if Self.Ref /= null then
         raise Immutability_Error;
      end if;
      Self.Ref := new Tuple_Reference_Counter_Type;
      Registers := Data.Get;
      Self.Ref.Length := Registers.all'LENGTH;
      if Self.Ref.Length /= 0 then
         Self.Ref.Data := new kv.avm.Memories.Register_Set_Type(0 .. Registers.all'LENGTH - 1);
         Self.Ref.Data.all := Registers.all;
      end if;
      Self.Ref.Count := Self.Ref.Count + 1;
   end Fold;

   ----------------------------------------------------------------------------
   procedure Fold_Empty
      (Self : in out Tuple_Type) is

   begin
      if Self.Ref /= null then
         raise Immutability_Error;
      end if;
      Self.Ref := new Tuple_Reference_Counter_Type;
      Self.Ref.Length := 0;
      Self.Ref.Count := Self.Ref.Count + 1;
   end Fold_Empty;

   ----------------------------------------------------------------------------
   procedure Fold_One
      (Self : in out Tuple_Type;
       Data : access kv.avm.Registers.Register_Type) is

   begin
      if Self.Ref /= null then
         raise Immutability_Error;
      end if;
      Self.Ref := new Tuple_Reference_Counter_Type;
      Self.Ref.Length := 1;
      Self.Ref.Data := new kv.avm.Memories.Register_Set_Type(0 .. 0);
      Self.Ref.Data(0) := Data.all;
      Self.Ref.Count := Self.Ref.Count + 1;
   end Fold_One;

   ----------------------------------------------------------------------------
   procedure Fold
      (Self : in out Tuple_Type;
       Data : in     kv.avm.Memories.Memory_Type;
       Map  : in     Map_Type'CLASS) is
      Pick_List : access constant kv.avm.References.Reference_Array_Type := Map.Get;
   begin
      if Self.Ref /= null then
         raise Immutability_Error;
      end if;
      Self.Ref := new Tuple_Reference_Counter_Type;
      if Pick_List = null then
         Self.Ref.Length := 0;
      else
         Self.Ref.Length := Pick_List.all'LENGTH;
      end if;
      if Self.Ref.Length /= 0 then
         Self.Ref.Data := new kv.avm.Memories.Register_Set_Type(0 .. Pick_List.all'LENGTH - 1);
         for I in Pick_List'RANGE loop
            Self.Ref.Data(I) := Data.Read(Pick_List(I));
         end loop;
      end if;
      Self.Ref.Count := Self.Ref.Count + 1;
   end Fold;

   ----------------------------------------------------------------------------
   function Peek
      (Self  : in     Tuple_Type;
       Index : in     Interfaces.Unsigned_32) return access constant kv.avm.Registers.Register_Type is
   begin
      if Self.Ref = null or else Self.Ref.Data = null then
         --Put_Line("WARNING: kv.avm.tuple.Peek, Data is null!");
         return null;
      end if;
      return Self.Ref.Data(Index)'ACCESS;
   end Peek;

   ----------------------------------------------------------------------------
   function Unfolded(Self : Tuple_Type) return access constant kv.avm.Memories.Register_Set_Type is
   begin
      if Self.Ref = null or else Self.Ref.Data = null then
         --Put_Line("WARNING: kv.avm.tuple.Unfolded, Data is null!");
         return null;
      end if;
      return Self.Ref.Data.all'ACCESS;
   end Unfolded;


   ----------------------------------------------------------------------------
   function To_String(Self : Tuple_Type) return String is
   begin
      if Self.Ref = null or else Self.Ref.Data = null then
         return "<>";
      end if;
      return kv.avm.Memories.To_String(Self.Ref.Data.all);
   end To_String;


   -----------------------------------------------------------------------------
   function Reachable(Self : Tuple_Type) return kv.avm.Actor_References.Sets.Set is
   begin
      if Self.Ref = null or else Self.Ref.Data = null then
         return kv.avm.Memories.Reachable(Self.Ref.Data);
      else
         return kv.avm.Actor_References.Sets.Empty_Set;
      end if;
   end Reachable;


   ----------------------------------------------------------------------------
   function Length(Self : Tuple_Type) return Natural is
   begin
      if Self.Ref = null then
         return 0;
      end if;
      return Self.Ref.Length;
   end Length;


   ----------------------------------------------------------------------------
   function "="(L, R: Tuple_Type) return Boolean is
   begin
      if L.Ref = Null or R.Ref = Null then
         return False;
      end if;
      if L.Ref.Data = Null or R.Ref.Data = Null then
         return False;
      end if;
      return (L.Ref.Data.all = R.Ref.Data.all);
   end "=";

   ----------------------------------------------------------------------------
   procedure Tuple_Write(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : in Tuple_Type) is
   begin
      Natural'OUTPUT(Stream, Item.Ref.Length);
      if Item.Ref.Length /= 0 then
         for Index in Item.Ref.Data.all'RANGE loop
            kv.avm.Registers.Register_Type'OUTPUT(Stream, Item.Ref.Data(Index));
         end loop;
      end if;
   end Tuple_Write;


   ----------------------------------------------------------------------------
   procedure Tuple_Read(Stream : not null access Ada.Streams.Root_Stream_Type'CLASS; Item : out Tuple_Type) is
      Length : Natural;
   begin
      Item.Finalize; -- Clear out the old one, if there was something there.
      Length := Natural'INPUT(Stream);
      if Length /= 0 then
         Item.Ref := new Tuple_Reference_Counter_Type;
         Item.Ref.Length := Length;
         Item.Ref.Data := new kv.avm.Memories.Register_Set_Type(0 .. Interfaces.Unsigned_32(Length) - 1);
         for Index in Item.Ref.Data'RANGE loop
            Item.Ref.Data(Index) := kv.avm.Registers.Register_Type'INPUT(Stream);
         end loop;
         Item.Ref.Count := 1;
      else
         -- Make an empty tuple
         Item.Fold_Empty;
      end if;
   end Tuple_Read;






   type Definition_Reference_Counter_Type is
      record
         Count  : Natural := 0;
         Length : Natural := 0; -- Length of Data (Data remains null if Length = 0; empty tuples are allowed)
         Data   : kv.avm.Registers.Signature_Access;
      end record;


   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(kv.avm.Registers.Signature_Type, kv.avm.Registers.Signature_Access);

   -----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation(Definition_Reference_Counter_Type, Definition_Reference_Counter_Access);

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Definition_Type) is
   begin
      -- Don't need to do anything here with Ref.
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Adjust
      (Self : in out Definition_Type) is
   begin
      if Self.Ref /= null then
         Self.Ref.Count := Self.Ref.Count + 1;
      end if;
   end Adjust;

   ----------------------------------------------------------------------------
   procedure Finalize
      (Self : in out Definition_Type) is
      Ref : Definition_Reference_Counter_Access := Self.Ref;
   begin
      Self.Ref := null;
      if Ref /= null then
         Ref.Count := Ref.Count - 1;
         if Ref.Count = 0 then
            if Ref.Length /= 0 then
               Free(Ref.Data);
            end if;
            Free(Ref);
         end if;
      end if;
   end Finalize;

   ----------------------------------------------------------------------------
   function "="(L, R: Definition_Type) return Boolean is
   begin
      if L.Ref = Null or R.Ref = Null then
         return False;
      end if;
      if L.Ref.Length = 0 or R.Ref.Length = 0 then
         return L.Ref.Length = R.Ref.Length; -- True if both are 0 length
      end if;
      return (L.Ref.Data.all = R.Ref.Data.all);
   end "=";

   ----------------------------------------------------------------------------
   procedure Make
      (Self  : in out Definition_Type;
       Tuple : in     Tuple_Type'CLASS) is
   begin
      if Self.Ref /= null then
         raise Immutability_Error;
      end if;
      Self.Ref := new Definition_Reference_Counter_Type;
      Self.Ref.Length := Tuple.Length;
      if Self.Ref.Length /= 0 then
         Self.Ref.Data := new kv.avm.Registers.Signature_Type(1 .. Self.Ref.Length);
         for Index in Tuple.Ref.Data.all'RANGE loop
            Self.Ref.Data(Integer(Index+1)) := Tuple.Ref.Data(Index).Format;
         end loop;
      end if;
      Self.Ref.Count := Self.Ref.Count + 1;
   end Make;

   ----------------------------------------------------------------------------
   function To_String(Self : Definition_Type) return String is
   begin
      if Self.Ref = null or else Self.Ref.Length = 0 then
         return "";
      else
         declare
            Answer : string(1..Self.Ref.Length);
         begin
            for Index in Self.Ref.Data'RANGE loop
               Answer(Index) := kv.avm.Registers.Signature(Self.Ref.Data(Index));
            end loop;
            return Answer;
         end;
      end if;
   end To_String;

   ----------------------------------------------------------------------------
   function Length(Self : Definition_Type) return Natural is
   begin
      if Self.Ref = null then
         return 0;
      end if;
      return Self.Ref.Length;
   end Length;

end kv.avm.Tuples;
