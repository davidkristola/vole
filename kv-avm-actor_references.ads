with Interfaces;

package kv.avm.Actor_References is

   procedure Set_Local_Domain(Domain : Interfaces.Unsigned_32);
   function Get_Local_Domain return Interfaces.Unsigned_32;


   type Actor_Reference_Type is tagged private;

   procedure Initialize
      (Self   : in out Actor_Reference_Type;
       Key    : in     Interfaces.Unsigned_32;
       Domain : in     Interfaces.Unsigned_32 := Get_Local_Domain);

   function Get_Key(Self : Actor_Reference_Type) return Interfaces.Unsigned_32;
   function Get_Domain(Self : Actor_Reference_Type) return Interfaces.Unsigned_32;
   function Is_Local(Self : Actor_Reference_Type) return Boolean;

   function Image(Self : Actor_Reference_Type) return String;

   function "="(L, R : Actor_Reference_Type) return Boolean;
   function "<"(L, R : Actor_Reference_Type) return Boolean;

   Null_Reference : constant Actor_Reference_Type;

private

   type Actor_Reference_Type is tagged
      record
         Key    : Interfaces.Unsigned_32 := 0;
         Domain : Interfaces.Unsigned_32 := 0;
      end record;

   Null_Reference : constant Actor_Reference_Type := (Key => 0, Domain => 0);

end kv.avm.Actor_References;
