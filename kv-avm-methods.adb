package body kv.avm.Methods is

   ----------------------------------------------------------------------------
   function New_Method(Name : String; Code : kv.avm.Instructions.Code_Access) return Method_Access is
      Method : Method_Access;
   begin
      Method := new Method_Type;
      Method.Initialize(Name, Code);
      return Method;
   end New_Method;

   ----------------------------------------------------------------------------
   procedure Initialize
      (Self : in out Method_Type;
       Name : in     String;
       Code : in     kv.avm.Instructions.Code_Access) is
   begin
      Self.Name := new String'(Name);
      Self.Code := Code;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Add_Predicate
      (Self      : in out Method_Type;
       Predicate : in     kv.avm.References.Offset_Type) is
   begin
      Self.Predicate := Predicate;
      Self.Gated := True;
   end Add_Predicate;

   ----------------------------------------------------------------------------
   function Has_Predicate(Self : Method_Type) return Boolean is
   begin
      return Self.Gated;
   end Has_Predicate;

   ----------------------------------------------------------------------------
   function Get_Predicate(Self : Method_Type) return kv.avm.References.Offset_Type is
   begin
      return Self.Predicate;
   end Get_Predicate;

   ----------------------------------------------------------------------------
   function Get_Predicate(Self : Method_Type) return kv.avm.References.Reference_Type is
   begin
      return (Memory => kv.avm.References.Attribute, Index => Self.Predicate);
   end Get_Predicate;

   ----------------------------------------------------------------------------
   function Get_Name(Self : Method_Type) return String is
   begin
      return Self.Name.all;
   end Get_Name;

   ----------------------------------------------------------------------------
   function Get_Code(Self : Method_Type) return kv.avm.Instructions.Code_Access is
   begin
      return Self.Code;
   end Get_Code;

end kv.avm.Methods;
