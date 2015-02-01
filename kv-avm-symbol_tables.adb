with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with String_Ops; use String_Ops;

with kv.avm.Log; use kv.avm.Log;

package body kv.avm.Symbol_Tables is


   subtype String_Type is Ada.Strings.Unbounded.Unbounded_String;

   function "+"(S : String) return String_Type renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+"(U : String_Type) return String renames Ada.Strings.Unbounded.To_String;

   type Data_Type is
      record
         Name  : String_Type;
         Index : Integer;
         Kind  : kv.avm.Registers.Data_Kind;
         Init  : String_Type;
      end record;
   type Data_Pointer is access Data_Type;

   package Symbol_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => Data_Pointer);

   type Table_Type is limited
      record
         Symbols : Symbol_Maps.Map;
         Count   : Natural := 0;
         Super   : Symbol_Table_Access;
      end record;

   -----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Symbol_Table) is
   begin
      Self.Table := new Table_Type;
   end Initialize;

   -----------------------------------------------------------------------------
   function Count(Self : Symbol_Table) return Natural is
   begin
      return Natural(Self.Table.Symbols.Length);
   end Count;

   -----------------------------------------------------------------------------
   procedure Add
      (Self : in out Symbol_Table;
       Name : in     String;
       Kind : in     kv.avm.Registers.Data_Kind := kv.avm.Registers.Unset;
       Init : in     String := "") is
      Data : Data_Pointer;
   begin
      Data := new Data_Type;
      Data.Name := +Name;
      Data.Index := Self.Table.Count;
      Data.Kind := Kind;
      Data.Init := +Init;
      Self.Table.Symbols.Insert(Name, Data);
      Self.Table.Count := Self.Table.Count + 1;
   exception
      when Error: others =>
         Put_Error("EXCEPTION: "&Exception_Information(Error)&" Name=<"&Name&">.");
         raise;
   end Add;

   -----------------------------------------------------------------------------
   procedure Set_Kind
      (Self : in out Symbol_Table;
       Name : in     String;
       Kind : in     kv.avm.Registers.Data_Kind) is

      Data     : Data_Pointer;
      Location : Symbol_Maps.Cursor;

      use Symbol_Maps;

   begin
      Location := Self.Table.Symbols.Find(Name);
      if Location /= Symbol_Maps.No_Element then
         Data := Element(Location);
         Data.Kind := Kind;
      else
         raise Missing_Element_Error;
      end if;
   end Set_Kind;


   -----------------------------------------------------------------------------
   procedure Set_Init
      (Self : in out Symbol_Table;
       Name : in     String;
       Init : in     String) is

      Data     : Data_Pointer;
      Location : Symbol_Maps.Cursor;

      use Symbol_Maps;

   begin
      Location := Self.Table.Symbols.Find(Name);
      if Location /= Symbol_Maps.No_Element then
         Data := Element(Location);
         Data.Init := +Init;
      else
         raise Missing_Element_Error;
      end if;
   end Set_Init;

   -----------------------------------------------------------------------------
   function Get_Kind(Self : Symbol_Table; Name : String) return kv.avm.Registers.Data_Kind is

      Data     : Data_Pointer;
      Location : Symbol_Maps.Cursor;

      use Symbol_Maps;

   begin
      Location := Self.Table.Symbols.Find(Name);
      if Location /= Symbol_Maps.No_Element then
         Data := Element(Location);
      else
         raise Missing_Element_Error;
      end if;
      return Data.Kind;
   end Get_Kind;

   -----------------------------------------------------------------------------
   function Get_Index(Self : Symbol_Table; Name : String) return Natural is

      Data     : Data_Pointer;
      Location : Symbol_Maps.Cursor;

      use Symbol_Maps;

   begin
      Location := Self.Table.Symbols.Find(Name);
      if Location /= Symbol_Maps.No_Element then
         Data := Element(Location);
      else
         if Self.Table.Super /= null then
            return Self.Table.Super.Get_Index(Name);
         else
            raise Missing_Element_Error;
         end if;
      end if;
      return Natural(Data.Index);
   end Get_Index;

   -----------------------------------------------------------------------------
   function Has(Self : Symbol_Table; Name : String) return Boolean is
   begin
      return Self.Table.Symbols.Contains(Name);
   end Has;

   -----------------------------------------------------------------------------
   procedure Set_All_Indexes
      (Self     : in out Symbol_Table;
       Starting : in     Natural := 1) is

      Next_Index : Natural := Starting;

      procedure Process
         (Position : in Symbol_Maps.Cursor) is
         Data : Data_Pointer;
      begin
         Data := Symbol_Maps.Element(Position);
         Data.Index := Next_Index;
         Next_Index := Next_Index + 1;
      end Process;

   begin
      Self.Table.Symbols.Iterate(Process'ACCESS);
   end Set_All_Indexes;

   -----------------------------------------------------------------------------
   procedure For_Each
      (Self : in out Symbol_Table;
       Proc : not null access procedure
          (Name : in String;
           Kind : in kv.avm.Registers.Data_Kind;
           Indx : in Natural;
           Init : in String)) is

      procedure Process
         (Position : in Symbol_Maps.Cursor) is
         Data : Data_Pointer;
      begin
         Data := Symbol_Maps.Element(Position);
         Proc(+Data.Name, Data.Kind, Natural(Data.Index), +Data.Init);
      end Process;

   begin
      if Self.Table.Super /= null then
         Self.Table.Super.For_Each(Proc);
      end if;
      Self.Table.Symbols.Iterate(Process'ACCESS);
   end For_Each;

   -----------------------------------------------------------------------------
   procedure Link_Superclass_Table
      (Self  : in out Symbol_Table;
       Super : in     Symbol_Table_Access) is
   begin
      Self.Table.Super := Super;
   end Link_Superclass_Table;


end kv.avm.Symbol_Tables;
