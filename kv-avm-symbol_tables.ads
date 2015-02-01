with kv.avm.Registers;

package kv.avm.Symbol_Tables is

   Missing_Element_Error : exception;

   type Symbol_Table is tagged limited private;
   type Symbol_Table_Access is access all Symbol_Table;

   procedure Initialize
      (Self : in out Symbol_Table);

   function Count(Self : Symbol_Table) return Natural;

   procedure Add
      (Self : in out Symbol_Table;
       Name : in     String;
       Kind : in     kv.avm.Registers.Data_Kind := kv.avm.Registers.Unset;
       Init : in     String := "");

   procedure Set_Kind
      (Self : in out Symbol_Table;
       Name : in     String;
       Kind : in     kv.avm.Registers.Data_Kind);

   procedure Set_Init
      (Self : in out Symbol_Table;
       Name : in     String;
       Init : in     String);

   function Get_Kind(Self : Symbol_Table; Name : String) return kv.avm.Registers.Data_Kind;

   function Get_Index(Self : Symbol_Table; Name : String) return Natural;

   function Has(Self : Symbol_Table; Name : String) return Boolean;

   procedure Set_All_Indexes
      (Self     : in out Symbol_Table;
       Starting : in     Natural := 1);

   procedure For_Each
      (Self : in out Symbol_Table;
       Proc : not null access procedure
          (Name : in String;
           Kind : in kv.avm.Registers.Data_Kind;
           Indx : in Natural;
           Init : in String));

   procedure Link_Superclass_Table
      (Self  : in out Symbol_Table;
       Super : in     Symbol_Table_Access);

private

   type Table_Type;
   type Table_Pointer is access Table_Type;

   type Symbol_Table is tagged limited
      record
         Table : Table_Pointer;
      end record;

end kv.avm.Symbol_Tables;
