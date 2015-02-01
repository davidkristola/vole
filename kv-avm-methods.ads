with kv.avm.References;
with kv.avm.Registers;
with kv.avm.Instructions;

package kv.avm.Methods is

   type Method_Type is tagged private;
   type Method_Access is access Method_Type;

   function New_Method(Name : String; Code : kv.avm.Instructions.Code_Access) return Method_Access;

   procedure Initialize
      (Self : in out Method_Type;
       Name : in     String;
       Code : in     kv.avm.Instructions.Code_Access);

   procedure Add_Predicate
      (Self      : in out Method_Type;
       Predicate : in     kv.avm.References.Offset_Type);

   function Has_Predicate(Self : Method_Type) return Boolean;
   function Get_Predicate(Self : Method_Type) return kv.avm.References.Offset_Type;
   function Get_Predicate(Self : Method_Type) return kv.avm.References.Reference_Type;
   function Get_Name(Self : Method_Type) return String;
   function Get_Code(Self : Method_Type) return kv.avm.Instructions.Code_Access;

private

   type Method_Type is tagged
      record
         Name      : kv.avm.Registers.Constant_String_Access;
         Code      : kv.avm.Instructions.Code_Access;
         Gated     : Boolean := False;
         Predicate : kv.avm.References.Offset_Type;
      end record;

end kv.avm.Methods;
